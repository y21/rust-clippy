use clippy_utils::{
    diagnostics::span_lint_and_sugg, eager_or_lazy, path_to_local_id, source::snippet, ty::is_type_diagnostic_item,
};
use rustc_ast::LitKind;
use rustc_errors::Applicability;
use rustc_hir::{BinOpKind, Expr, ExprKind, PatKind};
use rustc_lint::LateContext;
use rustc_span::sym;

use super::MAP_OR_EQ;

pub(super) fn check(cx: &LateContext<'_>, expr: &Expr<'_>, recv: &Expr<'_>, def: &Expr<'_>, map: &Expr<'_>) {
    if is_type_diagnostic_item(cx, cx.typeck_results().expr_ty(recv).peel_refs(), sym::Option)
        && let ExprKind::Lit(lit) = def.kind
        && let LitKind::Bool(false) = lit.node

        && let ExprKind::Closure(closure) = map.kind
        && let body = cx.tcx.hir().body(closure.body)
        && let [param] = body.params
        && let PatKind::Binding(_, id, _, None) = param.pat.kind

        && let ExprKind::Binary(op, lhs, rhs) = body.value.kind
        && let BinOpKind::Eq = op.node
        && path_to_local_id(lhs, id)
        && eager_or_lazy::switch_to_eager_eval(cx, rhs)
    {
        let lhs_snippet = snippet(cx, recv.span, "<lhs>");
        let rhs_snippet = snippet(cx, rhs.span, "<rhs>");

        span_lint_and_sugg(
            cx,
            MAP_OR_EQ,
            expr.span,
            "this can be written more concisely using `==`",
            "try",
            format!("{lhs_snippet} == Some({rhs_snippet})"),
            Applicability::MachineApplicable,
        );
    }
}
