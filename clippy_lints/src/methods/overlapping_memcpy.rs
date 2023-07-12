// use clippy_utils::{
//     consts::{constant, Constant},
//     diagnostics::span_lint_and_then,
//     is_expr_path_def_path, paths, SpanlessEq,
// };
// use rustc_errors::Applicability;
// use rustc_hir::{Expr, ExprKind};
// use rustc_lint::LateContext;
// use rustc_middle::ty::TypeckResults;

// use super::OVERLAPPING_MEMCPY;

// fn find_pointee_and_offset<'tcx, 'hir>(
//     cx: &LateContext<'tcx>,
//     typeck_results: &'tcx TypeckResults<'tcx>,
//     expr: &'hir Expr<'hir>,
//     offset: u128,
// ) -> Option<(&'hir Expr<'hir>, u128)> { if let ExprKind::MethodCall(seg, recv, args, _) =
//   expr.kind { if typeck_results.expr_ty(recv).is_unsafe_ptr() && seg.ident.name == sym!(add) &&
//   let [add_offset] = args && let Some(Constant::Int(add_offset)) = constant(cx, typeck_results,
//   add_offset) { find_pointee_and_offset(cx, typeck_results, recv, offset + add_offset) } else if
//   [sym!(as_mut_ptr), sym!(as_ptr)].contains(&seg.ident.name) { Some((recv, offset)) } else { None
//   } } else if typeck_results.expr_ty(expr).is_unsafe_ptr() { Some((expr, offset)) } else { None }
// }

// // !! TODO !! MOVE TO clippy_lints/src/ptr.rs
// pub(super) fn check<'tcx, 'hir>(
//     cx: &LateContext<'tcx>,
//     call: &Expr<'_>,
//     args: &'hir [Expr<'hir>],
//     func: &'hir Expr<'hir>,
// ) { if is_expr_path_def_path(cx, func, &paths::PTR_COPY_NONOVERLAPPING) && let [src, dest, count]
//   = args && let Some((src_expr, src_offset)) = find_pointee_and_offset(cx, cx.typeck_results(),
//   src, 0) && let Some((dest_expr, dest_offset)) = find_pointee_and_offset(cx,
//   cx.typeck_results(), dest, 0) && SpanlessEq::new(cx).deny_side_effects().eq_expr(src_expr,
//   dest_expr) && let Some(Constant::Int(count)) = constant(cx, cx.typeck_results(), count) &&
//   src_offset + count > dest_offset { span_lint_and_then(cx, OVERLAPPING_MEMCPY, call.span,
//   "calling `ptr::copy_nonoverlapping` with overlapping regions", |diag| {
//   diag.span_note(src.span, format!("source pointer covers range `{src_offset}..{end}`", end =
//   src_offset + count)); diag.span_note(dest.span, format!("destination pointer covers range
//   `{dest_offset}..{end}`", end = dest_offset + count)); diag.span_suggestion(func.span, "instead
//   use", "std::ptr::copy", Applicability::MachineApplicable); diag.note("it is Undefined Behavior
//   to call `ptr::copy_nonoverlapping` with overlapping regions"); }); }
// }
