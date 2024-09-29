use clippy_utils::diagnostics::span_lint_hir_and_then;
use clippy_utils::ty::make_normalized_projection;
use clippy_utils::{fn_def_id_with_node_args, is_def_id_trait_method};
use rustc_hir::def::DefKind;
use rustc_hir::def_id::DefId;
use rustc_hir::intravisit::{FnKind, Visitor, walk_expr, walk_fn};
use rustc_hir::{Body, Expr, ExprKind, FnDecl, HirId, Node, Stmt, StmtKind, YieldSource};
use rustc_infer::infer::{DefineOpaqueTypes, TyCtxtInferExt};
use rustc_infer::traits::{Obligation, ObligationCause};
use rustc_lint::{LateContext, LateLintPass};
use rustc_middle::hir::nested_filter;
use rustc_middle::ty::{self, Ty};
use rustc_session::impl_lint_pass;
use rustc_span::def_id::{LocalDefId, LocalDefIdSet};
use rustc_span::{DUMMY_SP, Span, sym};
use rustc_trait_selection::traits::ObligationCtxt;
use std::iter;

declare_clippy_lint! {
    /// ### What it does
    /// Checks for functions that are declared `async` but have no `.await`s inside of them.
    ///
    /// ### Why is this bad?
    /// Async functions with no async code create overhead, both mentally and computationally.
    /// Callers of async methods either need to be calling from an async function themselves or run it on an executor, both of which
    /// causes runtime overhead and hassle for the caller.
    ///
    /// ### Example
    /// ```no_run
    /// async fn get_random_number() -> i64 {
    ///     4 // Chosen by fair dice roll. Guaranteed to be random.
    /// }
    /// let number_future = get_random_number();
    /// ```
    ///
    /// Use instead:
    /// ```no_run
    /// fn get_random_number_improved() -> i64 {
    ///     4 // Chosen by fair dice roll. Guaranteed to be random.
    /// }
    /// let number_future = async { get_random_number_improved() };
    /// ```
    #[clippy::version = "1.54.0"]
    pub UNUSED_ASYNC,
    pedantic,
    "finds async functions with no await statements"
}

#[derive(Default)]
pub struct UnusedAsync {
    /// Keeps track of async functions used as values (i.e. path expressions to async functions that
    /// are not immediately called)
    async_fns_as_value: LocalDefIdSet,
    /// Functions with unused `async`, linted post-crate after we've found all uses of local async
    /// functions
    unused_async_fns: Vec<UnusedAsyncFn>,
}

#[derive(Copy, Clone)]
struct UnusedAsyncFn {
    def_id: LocalDefId,
    fn_span: Span,
    await_in_async_block: Option<Span>,
}

impl_lint_pass!(UnusedAsync => [UNUSED_ASYNC]);

struct AsyncFnVisitor<'a, 'tcx> {
    cx: &'a LateContext<'tcx>,
    found_await: bool,
    /// Also keep track of `await`s in nested async blocks so we can mention
    /// it in a note
    await_in_async_block: Option<Span>,
    async_depth: usize,
}

impl<'a, 'tcx> Visitor<'tcx> for AsyncFnVisitor<'a, 'tcx> {
    type NestedFilter = nested_filter::OnlyBodies;

    fn visit_expr(&mut self, ex: &'tcx Expr<'tcx>) {
        if let ExprKind::Yield(_, YieldSource::Await { .. }) = ex.kind {
            if self.async_depth == 1 {
                self.found_await = true;
            } else if self.await_in_async_block.is_none() {
                self.await_in_async_block = Some(ex.span);
            }
        }

        let is_async_block = matches!(
            ex.kind,
            ExprKind::Closure(rustc_hir::Closure {
                kind: rustc_hir::ClosureKind::Coroutine(rustc_hir::CoroutineKind::Desugared(
                    rustc_hir::CoroutineDesugaring::Async,
                    _
                )),
                ..
            })
        );

        if is_async_block {
            self.async_depth += 1;
        }

        walk_expr(self, ex);

        if is_async_block {
            self.async_depth -= 1;
        }
    }

    fn nested_visit_map(&mut self) -> Self::Map {
        self.cx.tcx.hir()
    }
}

impl<'tcx> LateLintPass<'tcx> for UnusedAsync {
    fn check_fn(
        &mut self,
        cx: &LateContext<'tcx>,
        fn_kind: FnKind<'tcx>,
        fn_decl: &'tcx FnDecl<'tcx>,
        body: &Body<'tcx>,
        span: Span,
        def_id: LocalDefId,
    ) {
        if !span.from_expansion() && fn_kind.asyncness().is_async() && !is_def_id_trait_method(cx, def_id) {
            let mut visitor = AsyncFnVisitor {
                cx,
                found_await: false,
                async_depth: 0,
                await_in_async_block: None,
            };
            walk_fn(&mut visitor, fn_kind, fn_decl, body.id(), def_id);
            if !visitor.found_await {
                // Don't lint just yet, but store the necessary information for later.
                // The actual linting happens in `check_crate_post`, once we've found all
                // uses of local async functions that do require asyncness to pass typeck
                self.unused_async_fns.push(UnusedAsyncFn {
                    await_in_async_block: visitor.await_in_async_block,
                    fn_span: span,
                    def_id,
                });
            }
        }
    }

    fn check_path(&mut self, cx: &LateContext<'tcx>, path: &rustc_hir::Path<'tcx>, hir_id: HirId) {
        // Find paths to local async functions in a context where the asyncness might be required.
        // E.g. `async fn f() {}; let x = f;`
        // Depending on how `x` is used, f's asyncness might be required despite not having any `await`
        // statements, so don't lint at all if there are any such paths.
        if let Some(def_id) = path.res.opt_def_id()
            && let Some(local_def_id) = def_id.as_local()
            && cx.tcx.def_kind(def_id) == DefKind::Fn
            && cx.tcx.asyncness(def_id).is_async()
            && cx.maybe_typeck_results().is_some()
            && !matches!(
                cx.tcx.parent_hir_node(hir_id),
                Node::Expr(Expr { kind: ExprKind::Call(&Expr { hir_id: parent_id, .. }, _), .. })
                    if parent_id == hir_id
            )
            && !can_erase_future(cx, hir_id, local_def_id)
        {
            self.async_fns_as_value.insert(local_def_id);
        }
    }

    // After collecting all unused `async` and problematic paths to such functions,
    // lint those unused ones that didn't have any path expressions to them.
    fn check_crate_post(&mut self, cx: &LateContext<'tcx>) {
        let iter = self
            .unused_async_fns
            .iter()
            .filter(|UnusedAsyncFn { def_id, .. }| (!self.async_fns_as_value.contains(def_id)));

        for fun in iter {
            span_lint_hir_and_then(
                cx,
                UNUSED_ASYNC,
                cx.tcx.local_def_id_to_hir_id(fun.def_id),
                fun.fn_span,
                "unused `async` for function with no await statements",
                |diag| {
                    diag.help("consider removing the `async` from this function");

                    if let Some(span) = fun.await_in_async_block {
                        diag.span_note(
                            span,
                            "`await` used in an async block, which does not require \
                            the enclosing function to be `async`",
                        );
                    }
                },
            );
        }
    }
}

/// Checks if a path expression (`expr_id`) to an async function (`async_fn_def_id`) can have its
/// asyncness removed based on its ancestor expressions
///
/// More specifically, it iterates through parent expressions and checks if the type to change to
/// (initially from an `fn() -> impl Future<Output = ...>` type to a `fn() -> ...` type) is also
/// valid in that context.
///
/// Example:
/// ```ignore
/// async fn unused_async() -> i32 { 0 }
///
/// struct FutureFnWrapper<F>(F);
/// fn block_on_wrapper_fn<Fut: Future>(_: FutureFnWrapper<impl Fn() -> Fut>) {}
///
/// block_on_wrapper_fn(FutureFnWrapper(unused_async));
/// ```
///
/// The process for checking if `unused_async` needs its asyncness in the last expression above is:
/// 1) create a fn pointer type that is identical to the async fn signature except that the return
///    type is changed to its `Future::Output`: we get `fn() -> i32`.
/// 2) check if there is no type mismatch if we were to pass a `fn() -> i32` to `FutureFnWrapper()`
///    and that all where clauses still hold. If that passes (which it does), take the new return
///    type (`FutureWrapper<fn() -> i32>`) and repeat step 2 with the parent expression and that new
///    type
/// 3) when checking if we can pass `FutureWrapper<fn() -> i32>` to `block_on_wrapper()` we fail
///    because the where clause `<fn() -> i32 as Fn>::Output: Future` does not hold, so we return
///    false
fn can_erase_future(cx: &LateContext<'_>, mut expr_id: HirId, async_fn_def_id: LocalDefId) -> bool {
    // Start with a fn pointer type that is equivalent to the async fn signature with the `impl Future`
    // return type replaced with its `Future::Output`, i.e. `async fn() -> i32` to `fn() -> i32`
    let mut changed_ty = {
        let async_fn_node_args = cx.typeck_results().node_args(expr_id);
        let async_fn_sig = cx.tcx.instantiate_bound_regions_with_erased(
            cx.tcx.fn_sig(async_fn_def_id).instantiate(cx.tcx, async_fn_node_args),
        );

        if let Some(future_trait) = cx.tcx.lang_items().future_trait()
            && let Some(future_output_ty) =
                make_normalized_projection(cx.tcx, cx.param_env, future_trait, sym::Output, [async_fn_sig.output()])
        {
            Ty::new(
                cx.tcx,
                ty::FnPtr(
                    ty::Binder::dummy(ty::FnSigTys {
                        inputs_and_output: cx
                            .tcx
                            .mk_type_list_from_iter(async_fn_sig.inputs().iter().copied().chain([future_output_ty])),
                    }),
                    ty::FnHeader {
                        abi: async_fn_sig.abi,
                        c_variadic: async_fn_sig.c_variadic,
                        safety: async_fn_sig.safety,
                    },
                ),
            )
        } else {
            return false;
        }
    };

    for (_, node) in cx.tcx.hir().parent_iter(expr_id) {
        let expr = match node {
            Node::Expr(expr) => expr,
            // If we've reached a `;`, then there is nothing else to check
            Node::Stmt(Stmt {
                kind: StmtKind::Semi(_),
                ..
            }) => return true,
            _ => break,
        };

        let find_arg_index = |args: &[Expr<'_>]| args.iter().position(|arg| arg.hir_id == expr_id);

        let Some((def_id, node_args)) = fn_def_id_with_node_args(cx, expr) else {
            break;
        };
        let ty = match expr.kind {
            ExprKind::MethodCall(_, recv, args, _) => {
                let arg_index = if recv.hir_id == expr_id {
                    0
                } else if let Some(arg_index) = find_arg_index(args) {
                    arg_index
                } else {
                    break;
                };

                try_change_fn_arg_ty(
                    cx,
                    arg_index,
                    iter::once(recv).chain(args),
                    changed_ty,
                    def_id,
                    node_args,
                )
            },
            ExprKind::Call(_, args) if let Some(arg_index) = find_arg_index(args) => {
                try_change_fn_arg_ty(cx, arg_index, args.iter(), changed_ty, def_id, node_args)
            },
            _ => break,
        };

        let Some(ty) = ty else { break };
        if ty == cx.typeck_results().expr_ty(expr) {
            return true;
        }
        expr_id = expr.hir_id;
        changed_ty = ty;
    }

    false
}

/// Checks if the argument's type of a function call can be changed without causing type errors
/// and returns the new return type (in case it is dependent on the type being changed) if no errors
/// occurred.
fn try_change_fn_arg_ty<'tcx>(
    cx: &LateContext<'tcx>,
    arg_index: usize,
    args: impl Iterator<Item = &'tcx Expr<'tcx>>,
    changed_ty: Ty<'tcx>,
    def_id: DefId,
    node_args: ty::GenericArgsRef<'tcx>,
) -> Option<Ty<'tcx>> {
    let infcx = cx.tcx.infer_ctxt().build();
    let substs = cx.tcx.mk_args_from_iter(
        node_args
            .iter()
            .map(|_| ty::GenericArg::from(infcx.next_ty_var(DUMMY_SP))),
    );
    let fn_sig = cx
        .tcx
        .instantiate_bound_regions_with_erased(cx.tcx.fn_sig(def_id).instantiate(cx.tcx, substs));

    let ocx = ObligationCtxt::new(&infcx);
    for (idx, (arg, &param_ty)) in args.zip(fn_sig.inputs()).enumerate() {
        let arg_ty = if idx == arg_index {
            changed_ty
        } else {
            cx.typeck_results().expr_ty(arg)
        };

        ocx.register_infer_ok_obligations(
            infcx
                .at(&ObligationCause::dummy(), cx.param_env)
                .eq(DefineOpaqueTypes::No, param_ty, arg_ty)
                .ok()?,
        );
    }

    let ty::InstantiatedPredicates { predicates, .. } = cx.tcx.predicates_of(def_id).instantiate(cx.tcx, substs);
    let obligations = predicates
        .iter()
        .copied()
        .map(|predicate| Obligation::new(cx.tcx, ObligationCause::dummy(), cx.param_env, predicate));
    ocx.register_obligations(obligations);

    // Make sure all where clauses hold
    if !ocx.select_all_or_error().is_empty() {
        return None;
    }

    Some(infcx.resolve_vars_if_possible(fn_sig.output()))
}
