use clippy_utils::ty::is_copy;
use rustc_hir::*;
use rustc_lint::{LateContext, LateLintPass};
use rustc_session::{declare_lint_pass, declare_tool_lint};

declare_clippy_lint! {
    /// ### What it does
    ///
    /// ### Why is this bad?
    ///
    /// ### Example
    /// ```no_run
    /// // example code where clippy issues a warning
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

impl LateLintPass<'_> for CovariantMutableTypes {
    fn check_item(&mut self, cx: &LateContext<'_>, item: &Item<'_>) {
        let item_def_id = item.owner_id.to_def_id();
        if let ItemKind::Struct(variant, generics) = item.kind
            && let variances = cx.tcx.variances_of(item_def_id)
            && let ty = cx.tcx.type_of(item_def_id).instantiate_identity()
            // TODO: or derived clone
            && dbg!(is_copy(cx, ty))
            && dbg!(cx.tcx.inherent_impls(item_def_id)).iter().any(|imp| {
                false
            })
        {}
    }
}
