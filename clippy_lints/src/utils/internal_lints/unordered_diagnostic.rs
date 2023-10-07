use clippy_utils::diagnostics::span_lint_and_note;
use clippy_utils::fn_def_id;
use clippy_utils::higher::ForLoop;
use rustc_hir::*;
use rustc_lint::{LateContext, LateLintPass};
use rustc_middle::ty::Ty;
use rustc_session::{declare_tool_lint, impl_lint_pass};
use rustc_span::{sym, Span};

declare_clippy_lint! {
    ///dgdfg
    ///
    #[clippy::version = "1.75.0"]
    pub UNORDERED_DIAGNOSTIC,
    internal,
    "emitting diagnostics in arbitrary order"
}

impl_lint_pass!(UnorderedDiagnostic => [UNORDERED_DIAGNOSTIC]);

#[derive(Default)]
pub struct UnorderedDiagnostic {
    pub unordered_loop_stack: Vec<Span>,
}

fn is_unordered_type(cx: &LateContext<'_>, ty: Ty<'_>) -> bool {
    if let Some(adt_def) = ty.peel_refs().ty_adt_def() {
        cx.tcx.is_diagnostic_item(sym::HashMap, adt_def.did()) || cx.tcx.is_diagnostic_item(sym::HashSet, adt_def.did())
    } else {
        false
    }
}

impl LateLintPass<'_> for UnorderedDiagnostic {
    fn check_expr(&mut self, cx: &LateContext<'_>, expr: &Expr<'_>) {
        if let Some(fl) = ForLoop::hir(expr)
            && is_unordered_type(
                cx,
                cx
                    .typeck_results()
                    .expr_ty(fl.arg)
                    .peel_refs()
                )
        {
            eprintln!("Enter @ {:?}", expr.span);
            self.unordered_loop_stack.push(expr.span);
        } else if let Some(callee_did) = fn_def_id(cx, expr)
            && let Some(&span) = self.unordered_loop_stack.last()
            // assume anything in clippy_utils::diagnostics might emit a lint and should be in a defined order
            && dbg!(cx.get_def_path(callee_did)).starts_with(&[sym!(clippy_utils), sym!(diagnostics)])
        {
            dbg!(expr.span);
            span_lint_and_note(
                cx,
                UNORDERED_DIAGNOSTIC,
                expr.span,
                "emitting diagnostic in arbitrary order",
                Some(span),
                "iterating over a type that yields item in arbitrary order here"
            );
        }
    }

    fn check_expr_post(&mut self, _cx: &LateContext<'_>, expr: &Expr<'_>) {
        if self.unordered_loop_stack.last().is_some_and(|&last| last == expr.span) {
            self.unordered_loop_stack.pop();
        }
    }
}
