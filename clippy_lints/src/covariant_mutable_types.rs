use clippy_utils::diagnostics::span_lint_and_then;
use clippy_utils::is_path_diagnostic_item;
use clippy_utils::ty::is_copy;
use rustc_hir::def::{DefKind, Res};
use rustc_hir::def_id::{DefId, LocalDefId};
use rustc_hir::*;
use rustc_lint::{LateContext, LateLintPass};
use rustc_middle::ty::{self, Ty, TyCtxt};
use rustc_session::{declare_lint_pass, declare_tool_lint};
use rustc_span::sym;

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

/// Checks whether `T<X: ?Copy>: Copy` holds, or in other words, if there exists an
/// `impl Copy for T`, *even if* its generic maybe aren't
fn is_copy_modulo_bounds<'tcx>(tcx: TyCtxt<'tcx>, ty: Ty<'tcx>) -> bool {
    let Some(copy_did) = tcx.lang_items().copy_trait() else {
        return false;
    };

    tcx.hir().trait_impls(copy_did).iter().any(|&imp| {
        // TODO: bad name (imp)
        if let Some(Node::Item(item)) = tcx.hir().find_by_def_id(imp)
            && let self_ty = tcx.type_of(item.owner_id.to_def_id()).instantiate_identity()
            && self_ty == ty
        {
            true
        } else {
            false
        }
    })
}

fn exposes_safe_mutability<'tcx>(tcx: TyCtxt<'tcx>, ty_def_id: LocalDefId) -> bool {
    tcx.inherent_impls(ty_def_id)
        .iter()
        .filter_map(|&imp| {
            if let Some(Node::Item(item)) = tcx.hir().find_by_def_id(imp.expect_local())
                && let ItemKind::Impl(imp) = item.kind
            {
                Some(imp)
            } else {
                None
            }
        })
        .flat_map(|imp| imp.items)
        .any(|item| {
            if let AssocItemKind::Fn { has_self: true } = item.kind
                && let sig = tcx.fn_sig(item.id.owner_id.def_id)
                    .skip_binder()
                    .skip_binder()
                && let output = sig.output()
                // check for `impl<T> ... -> &mut T`
                && let ty::Ref(_region /* TODO */, rty, Mutability::Mut) = output.kind()
                // TODO: actually support >1 type parameter and lifetimes too
                // TODO 2: check that the body actually returns *something* associated with the relevant field
                && rty.is_param(0)
            {
                true
            } else {
                false
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
            // TODO: perhaps be smarter and even allow smth like  NonNull<Com<Po<Site<T>>>>
            // right now it just looks for NonNull<T>, nothing more, nothing less.
            // we've already checked that its covariant
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
            && match variant {
                VariantData::Struct(fields, ..) | VariantData::Tuple(fields, ..) => {
                    fields
                        .iter()
                        .any(|def| field_def_is_covariant_ptr(cx.tcx, def, ty_param.def_id.to_def_id()))
                },
                VariantData::Unit(..) => false,
            }
            // TODO: or derived clone
            && is_copy_modulo_bounds(cx.tcx, ty)
            && exposes_safe_mutability(cx.tcx, item_def_id)
        {
            span_lint_and_then(
                cx,
                COVARIANT_MUTABLE_TYPES,
                item.span,
                "covariant mutable types are unsound",
                |diag| {
                    diag.note("add a `PhantomData<*mut T>` to force invariance");
                },
            )
        }
    }
}
