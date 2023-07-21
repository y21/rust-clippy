use clippy_utils::{find_binding_init, fn_def_id, get_parent_expr, is_expr_path_def_path, match_def_path};
use rustc_hir::def::Res;
use rustc_hir::{Expr, ExprKind, HirId, QPath, UnOp};
use rustc_lint::{LateContext, LateLintPass};
use rustc_session::{declare_tool_lint, impl_lint_pass};
use rustc_span::def_id::DefIdSet;

declare_clippy_lint! {
    /// ### What it does
    ///
    /// ### Why is this bad?
    ///
    /// ### Example
    /// ```rust
    /// // example code where clippy issues a warning
    /// ```
    /// Use instead:
    /// ```rust
    /// // example code which does not raise clippy warning
    /// ```
    #[clippy::version = "1.73.0"]
    pub UNNECESSARY_REFCELL,
    nursery,
    "default lint description"
}
// declare_lint_pass!(UnnecessaryRefcell => [UNNECESSARY_REFCELL]);

#[derive(Default)]
pub struct UnnecessaryRefcell {
    unlintable_refcell_fields: DefIdSet,
}
impl_lint_pass!(UnnecessaryRefcell => [UNNECESSARY_REFCELL]);

impl LateLintPass<'_> for UnnecessaryRefcell {
    fn check_expr(&mut self, cx: &LateContext<'_>, expr: &'_ rustc_hir::Expr<'_>) {
        if let ExprKind::MethodCall(_, receiver, ..) | ExprKind::Call(_, [receiver]) = expr.kind
            && let Some(callee_did) = fn_def_id(cx, expr)
            && (match_def_path(cx, callee_did, &["core", "cell", "RefCell", "borrow"])
                || match_def_path(cx, callee_did, &["core", "cell", "RefCell", "borrow_mut"]))
            && let ExprKind::Field(sub, ident) = receiver.kind
            && let Some(adt) = cx.typeck_results().expr_ty(sub).peel_refs().ty_adt_def()
            && adt.is_struct() // for now
            && let Some(field) = adt.all_fields().find(|v| v.name == ident.name)
            && !self.unlintable_refcell_fields.contains(&field.did)
            && let Some(parent) = get_parent_expr(cx, expr)
        {
            dbg!(parent);
            // Found a call to `RefCell::borrow(_mut)` and the corresponding field def.
            // Now let's see if it's a unsupported operation and store it for later so we can exclude it

            // Allow:
            // - `*x.ref_cell.borrow_mut()`
            dbg!(cx.typeck_results().expr_ty_adjusted(parent));
            if let ExprKind::Unary(UnOp::Deref, _) = parent.kind
                && let Some(parent_parent) = get_parent_expr(cx, parent)
            {
                return;
            }

            // Any other expression should be considered unlintable
            self.unlintable_refcell_fields.insert(field.did);
        }
        // if let Some(receiver) = refcell_borrow_receiver(cx, expr) {
        //     dbg!(receiver);
        // }
    }

    fn check_crate_post(&mut self, _: &LateContext<'_>) {
        dbg!(&self.unlintable_refcell_fields);
    }
}
