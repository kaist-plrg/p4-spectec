//! Variable-to-expression conversion
//!
//! A variable `x` with iterations `*?` denotes the expression `(x*)?`;
//! `as_exp` builds that expression with the right type at every level.

use super::ast::*;
/// Converts `var` to an expression with the same type and iteration structure.
///
/// Starts with a variable reference
/// and wraps it once for each iterator in `var.iters`,
/// from innermost to outermost.
/// When `is_dim` is true,
/// each iteration carries a binder describing that dimension;
/// otherwise its binder list is empty.
pub fn as_exp(is_dim: bool, var: &Var) -> Exp {
    // Start from the bare variable at its base type
    let mut exp: Exp = crate::note_phrase!(node: ExpKind::Id(var.id.clone()), note: var.typ.node.clone(), span: var.id.span.clone());
    let mut iters_prior = Vec::new();
    // Wrap one iteration at a time, lifting the type each time
    for iter in &var.iters {
        let typ_iter = crate::phrase! {
            node: TypKind::Iter(
                Box::new(crate::phrase! {
                    node: exp.note.as_ref().clone(),
                    span: exp.span.clone(),
                }),
                *iter,
            ),
            span: var.typ.span.clone(),
        };
        // The binder names the variable as seen at this depth
        let var_binder =
            Var { id: var.id.clone(), typ: typ_iter.clone(), iters: iters_prior.clone() };
        let span = exp.span.clone();
        exp = crate::note_phrase! {
            node: ExpKind::Iter(
                Box::new(exp),
                ExpIter { iter: *iter, vars: if is_dim { vec![var_binder] } else { vec![] } },
            ),
            note: typ_iter.node.clone(),
            span: span,
        };
        iters_prior.push(*iter);
    }
    exp
}
