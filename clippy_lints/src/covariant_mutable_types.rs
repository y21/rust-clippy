use clippy_utils::diagnostics::span_lint_and_then;
use clippy_utils::visitors::for_each_expr;
use rustc_hir::def::{DefKind, Res};
use rustc_hir::def_id::{DefId, LocalDefId};
use rustc_hir::*;
use rustc_lint::{LateContext, LateLintPass};
use rustc_middle::ty::{self, Ty, TyCtxt};
use rustc_session::{declare_lint_pass, declare_tool_lint};
use rustc_span::{sym, Span};
use std::ops::ControlFlow;

declare_clippy_lint! {
    /// ### What it does
    /// Looks for a combination of properties on types that lead to an [unsound] API:
    ///
    /// - Being generic over a type `T`
    /// - Storing a pointer type that is [covariant] in the pointee type
    ///     - Currently, the only ones that are considered are `NonNull<T>` and `*const T`
    /// - Implementing `Copy` or deriving `Clone`
    /// - Exposing mutability of the contained `T` safely to the user (e.g. an `as_mut` method, `DerefMut`, ...)
    ///
    /// This lint exists primarily to catch a pitfall that can be easy to fall into when designing a datastructure
    /// using `NonNull<T>` and forgetting about variance.
    ///
    /// As such, this lint is only relevant to unsafe code and the last property in the list requires it.
    /// Safe code cannot create a `&mut T` from a covariant pointer type such as `NonNull<T>` or `*const T`.
    ///
    /// [covariant]: https://doc.rust-lang.org/nomicon/subtyping.html#variance
    /// [unsound]: https://rust-lang.github.io/unsafe-code-guidelines/glossary.html#soundness-of-code--of-a-library
    ///
    /// ### Why is this bad?
    /// This is unsound and allows violating the memory safety guarantees provided by the language.
    ///
    /// A user can create a copy of the type -- say `P<&'static str>` -- which shares the same underlying pointer.
    /// Due to covariance, the copy can be coerced to `P<&'a str>`.
    /// If a method exists that now also allows the user to get a `&mut T`,
    /// the user can assign a `&'a str` to the `P<&'a str>`, which *updates the original `P<&'static str>` as well*.
    ///
    /// The point at which this can introduce Undefined Behavior is to then go back to the original `P<&'static str>`
    /// and use the `&'static str`, which isn't actually `'static`, after the referent of `&'a str`
    /// has gone out of scope.
    ///
    /// This can be fixed by making the type invariant in its type parameter such that no coercions are possible - a
    /// `P<&'static str>` must remain a `P<&'static str>`.
    /// Forcing invariance can be achieved by adding a `PhantomData<*mut T>` field to the struct.
    ///
    /// If you'd like to learn more about variance and subtyping, read the [nomicon chapter on subtyping].
    ///
    /// [nomicon chapter on subtyping]: https://doc.rust-lang.org/nomicon/subtyping.html
    ///
    /// ### Example
    /// ```no_run
    /// struct 
    /// ```
    /// Use instead:
    /// ```no_run
    /// // example code which does not raise clippy warning
    /// ```
    #[clippy::version = "1.75.0"]
    pub COVARIANT_MUTABLE_TYPES,
    suspicious,
    "default lint description"
}

declare_lint_pass!(CovariantMutableTypes => [COVARIANT_MUTABLE_TYPES]);

fn find_local_trait_impl_for_type<'tcx>(tcx: TyCtxt<'tcx>, ty: Ty<'tcx>, trait_did: DefId) -> Option<LocalDefId> {
    tcx.hir().trait_impls(trait_did).iter().find_map(|&impl_did| {
        if let Some(Node::Item(item)) = tcx.hir().find_by_def_id(impl_did)
            && let self_ty = tcx.type_of(item.owner_id.to_def_id()).instantiate_identity()
            && self_ty == ty
        {
            Some(impl_did)
        } else {
            None
        }
    })
}

// TODO: also consider traits that create a `&mut T` and invoke user code
fn exposes_safe_mutability<'tcx>(tcx: TyCtxt<'tcx>, ty_def_id: LocalDefId) -> Option<Span> {
    tcx.inherent_impls(ty_def_id)
        .iter()
        .filter_map(|&impl_did| {
            if let Some(Node::Item(item)) = tcx.hir().find_by_def_id(impl_did.expect_local())
                && let ItemKind::Impl(imp) = item.kind
            {
                Some(imp)
            } else {
                None
            }
        })
        .flat_map(|imp| imp.items)
        .find_map(|item| {
            if let AssocItemKind::Fn { has_self: true } = item.kind
                && let sig = tcx.fn_sig(item.id.owner_id.def_id)
                    .skip_binder()
                    .skip_binder()
                && let output = sig.output()
                // check for `impl<T> ... -> &mut T`
                && let ty::Ref(_region /* TODO */, rty, Mutability::Mut) = output.kind()
                // TODO: actually support >1 type parameter and lifetimes too
                // TODO: check that the body actually returns *something* associated with the relevant field
                // TODO TODO: must be pub!!!! and not unsafe!! check signature more closely
                && rty.is_param(0)
            // We've checked that the signature matches, now we should check that the return expression(s)
            // reference the relevant field, e.g. `unsafe { self.0.as_mut() }`
                && let body_id = tcx.hir().body_owned_by(item.id.owner_id.def_id)
                && let body = tcx.hir().body(body_id)
                && true
            {
                // TODO: what do we do if we have multiple return points??
                // TODO: also wrong if we have e.g. `return { ... a lot of code involving the field...;
                // Box::leak(..) }`
                for_each_expr(body.value, |expr| {
                    if let ExprKind::Field(e, ident) = expr.kind
                        && ident.name == sym!(0) // TODO: dont hardcode
                        && let ExprKind::Path(QPath::Resolved(_, path)) = e.kind
                        && let [segment] = path.segments
                        && segment.ident.name == sym!(self)
                    {
                        ControlFlow::Break(item.span)
                    } else {
                        ControlFlow::Continue(())
                    }
                })
            } else {
                None
            }
        })
}

/// Checks if a [`FieldDef`] is a pointer-like type that is covariant in the pointee,
/// and the type parameter is a given type parameter
fn field_def_is_covariant_ptr<'tcx>(tcx: TyCtxt<'tcx>, def: &FieldDef<'_>, ty_param_def_id: DefId) -> bool {
    match def.ty.kind {
        TyKind::Ptr(MutTy {
            mutbl: Mutability::Mut, ..
        }) => true,
        TyKind::Path(qpath)
            if let QPath::Resolved(_, path) = qpath
                && let Some(did) = path.res.opt_def_id()
                && tcx.get_diagnostic_item(sym::NonNull) == Some(did)
                && let Some(nonnull) = path.segments.last()
                && let Some([GenericArg::Type(elem_ty)]) = nonnull.args.map(|v| v.args)
                && let TyKind::Path(QPath::Resolved(_, elem_path)) = elem_ty.kind
                && let Res::Def(DefKind::TyParam, param_did) = elem_path.res
                && param_did == ty_param_def_id =>
        {
            true
        },
        _ => false,
    }
}

impl<'tcx> LateLintPass<'tcx> for CovariantMutableTypes {
    fn check_item(&mut self, cx: &LateContext<'tcx>, item: &Item<'_>) {
        let item_def_id = item.owner_id.def_id;

        if let ItemKind::Struct(variant, generics) = item.kind
            && let variances = cx.tcx.variances_of(item_def_id)
            // TODO: dont hardcode ty param 0
            && let Some(ty_param) = generics.params.first()
            && variances[0] == ty::Variance::Covariant
            && let ty = cx.tcx.type_of(item_def_id).instantiate_identity()
            // TODO: this should probably give us the ident(s?) of the field
            && match variant {
                VariantData::Struct(fields, ..) | VariantData::Tuple(fields, ..) => {
                    fields
                        .iter()
                        .any(|def| field_def_is_covariant_ptr(cx.tcx, def, ty_param.def_id.to_def_id()))
                },
                VariantData::Unit(..) => false,
            }
            && let Some(copy_did) = cx.tcx.lang_items().copy_trait()
            && let Some(clone_did) = cx.tcx.get_diagnostic_item(sym::Clone)
            // ... If the type can be "trivially" duplicated, i.e. the pointer type can be copied as is.
            // This is the case when the type is `Copy` or has a derived `Clone` impl.
            // Derived impls are marked with `#[automatically_derived]`.
            && (find_local_trait_impl_for_type(cx.tcx, ty, copy_did).is_some()
                || find_local_trait_impl_for_type(cx.tcx, ty, clone_did).is_some_and(|did| {
                    cx.tcx.get_attr(did, sym::automatically_derived).is_some()
            }))
            && let Some(mut_span) = exposes_safe_mutability(cx.tcx, item_def_id)
        {
            span_lint_and_then(
                cx,
                COVARIANT_MUTABLE_TYPES,
                item.span,
                "struct is covariant in its type parameter when it should be invariant",
                |diag| {
                    diag.span_note(mut_span, "mutability exposed here, which allows assigning a subtype");
                    diag.note("add a `PhantomData<*mut T>` to force invariance");
                },
            )
        }
    }
}
