//! Value operations shared by expression, guard, and path evaluation
//!
//! Operators, predicates, casts, access, and updates on arena values;
//! every failure is located at the span the caller passes in.

use std::rc::Rc;

use num_bigint::BigInt;

use crate::lang::{
    common::{
        prim::{bool, num},
        source::{Phrase, Span},
    },
    data::{
        arena::Arena,
        value::flat::{self as value, Value, ValueKind},
    },
    traits::eq::SyntaxEq,
};

use crate::lang::il::{ast, prepared::Prepared};

use crate::runtime::ops::{
    self,
    typ::{Theta, subst_typ},
};

use crate::interp::shared::{
    backtrack::{self, Backtrack, fatal, ok, unwrap, unwrap_from_result},
    error,
};

use super::super::context::ReadContext;

// = Operators

// - Unary operators

/// Applies a boolean or numeric unary operator.
pub(crate) fn unop(
    arena: &mut Arena,
    span: &Span,
    op: &ast::UnOp,
    value: Value,
) -> Backtrack<Value> {
    let value = match op {
        // Boolean negation
        ast::UnOp::Bool(bool::UnOp::Not) => {
            let bool = !value::get::bool(arena, &value).expect("operand must be a boolean");
            unwrap_from_result!(value::make::bool(arena, bool, Span::default()), span)
        }
        // Numeric unary operator
        ast::UnOp::Num(op) => {
            let num = value::get::num(arena, &value).expect("operand must be a number");
            let num = num::un(*op, num);
            unwrap_from_result!(value::make::num(arena, num, Span::default()), span)
        }
    };
    ok!(value)
}

// - Binary operators

/// Applies a boolean or numeric binary operator.
pub(crate) fn binop(
    arena: &mut Arena,
    span: &Span,
    op: &ast::BinOp,
    value_l: Value,
    value_r: Value,
) -> Backtrack<Value> {
    let value = match op {
        // Boolean connectives
        ast::BinOp::Bool(op) => {
            let bool_l = value::get::bool(arena, &value_l).expect("operand must be a boolean");
            let bool_r = value::get::bool(arena, &value_r).expect("operand must be a boolean");
            let result = match op {
                bool::BinOp::And => bool_l && bool_r,
                bool::BinOp::Or => bool_l || bool_r,
                bool::BinOp::Impl => !bool_l || bool_r,
                bool::BinOp::Equiv => bool_l == bool_r,
            };
            unwrap_from_result!(value::make::bool(arena, result, Span::default()), span)
        }
        // Arithmetic
        ast::BinOp::Num(op) => {
            let num_l = value::get::num(arena, &value_l).expect("operand must be a number");
            let num_r = value::get::num(arena, &value_r).expect("operand must be a number");
            let num = unwrap_from_result!(num::bin(*op, num_l, num_r), span);
            unwrap_from_result!(value::make::num(arena, num, Span::default()), span)
        }
    };
    ok!(value)
}

// - Comparison operators

/// Compares two values: syntactically for `=`/`!=`, numerically otherwise.
pub(crate) fn cmpop(
    arena: &Arena,
    span: &Span,
    op: &ast::CmpOp,
    value_l: Value,
    value_r: Value,
) -> Backtrack<bool> {
    ok!(match op {
        // Equality is syntactic
        ast::CmpOp::Bool(bool::CmpOp::Eq) => value_l.view(arena).syntax_eq(&value_r.view(arena)),
        // So is inequality
        ast::CmpOp::Bool(bool::CmpOp::Ne) => {
            !value_l.view(arena).syntax_eq(&value_r.view(arena))
        }
        // Ordering compares numbers
        ast::CmpOp::Num(op) => {
            let num_l = value::get::num(arena, &value_l).expect("operand must be a number");
            let num_r = value::get::num(arena, &value_r).expect("operand must be a number");
            unwrap_from_result!(num::cmp(*op, num_l, num_r), span)
        }
    })
}

// = Predicates

// - Subtype checks

/// Runs the precomputed subtype check against a value.
pub(crate) fn sub(
    arena: &Arena,
    ctx: &impl ReadContext,
    span: &Span,
    subcheck: &ast::Subcheck<Prepared>,
    value: Value,
) -> Backtrack<bool> {
    let find_typdef_opt = |id: &ast::Id| ctx.find_typdef_opt(id);
    let find_func = |name: &str| {
        let id = crate::phrase!(node: name.to_owned(), span: span.clone());
        ctx.find_func_typ(&id).ok()
    };
    backtrack::from_result(
        ops::value::check(arena, &find_typdef_opt, &find_func, subcheck, &value),
        span,
    )
}

// - Pattern matching

/// Tests a value against a case, list, or option pattern.
pub(crate) fn r#match(arena: &Arena, pattern: &ast::Pattern<Prepared>, value: Value) -> bool {
    match (pattern, arena.value.kind(value.node)) {
        // Case: same constructor shape
        (ast::Pattern::Case(mixop), ValueKind::Case(value_case)) => {
            let arena_mixop = &arena.mixop;
            let mixop_pattern = mixop.view(arena_mixop);
            let mixop_value = value_case.mixop().view(arena_mixop);
            mixop_pattern.syntax_eq(&mixop_value)
        }
        // List: non-empty, fixed length, or empty
        (ast::Pattern::List(pattern), ValueKind::List(values)) => match pattern {
            ast::ListPattern::Cons => !values.is_empty(),
            ast::ListPattern::Fixed(len) => values.len() == *len,
            ast::ListPattern::Nil => values.is_empty(),
        },
        // Option: present or absent
        (ast::Pattern::Opt(ast::OptPattern::Some), ValueKind::Opt(Some(_)))
        | (ast::Pattern::Opt(ast::OptPattern::None), ValueKind::Opt(None)) => true,
        // Other combinations never match
        _ => false,
    }
}

// - Membership

/// Tests list membership by syntactic equality.
pub(crate) fn mem(
    arena: &Arena,
    _span: &Span,
    value_elem: Value,
    value_list: Value,
) -> Backtrack<bool> {
    let values = value::get::list(arena, &value_list).expect("operand must be a list");
    ok!(values
        .iter()
        .any(|value| value.view(arena).syntax_eq(&value_elem.view(arena))),)
}

// = Casts

// - Upcast

/// Upcasts a value to `typ` through aliases, tuples, and iterations.
pub(crate) fn cast_up(
    arena: &mut Arena,
    ctx: &impl ReadContext,
    typ: &ast::Typ,
    value: Value,
) -> Backtrack<Value> {
    let span = &typ.span;
    let result = match &typ.node {
        // Natural to integer
        ast::TypKind::Num(num::Typ::Int) => {
            let num = value::get::num(arena, &value).expect("operand must be a number");
            match num {
                num::Number::Nat(num) => {
                    let num = num.as_bigint().clone();
                    unwrap_from_result!(value::make::int(arena, num, Span::default()), span)
                }
                num::Number::Int(_) => value,
            }
        }
        // Named type: unfold a plain alias; variants and structs stay as is
        ast::TypKind::Var(id, targs) => {
            let (tparams, def_typ) = unwrap_from_result!(ctx.find_defined_typdef(id), span);
            let theta = unwrap_from_result!(Theta::from_lists(tparams, targs), span);
            match &def_typ.node {
                ast::DefTypKind::Plain(typ) => {
                    let typ = unwrap_from_result!(subst_typ(&|id| theta.get(id), typ), span);
                    return cast_up(arena, ctx, &typ, value);
                }
                _ => value,
            }
        }
        // Tuple: componentwise
        ast::TypKind::Tuple(typs) => {
            let values = value::get::tuple(arena, &value)
                .expect("operand must be a tuple")
                .to_vec();
            assert_eq!(typs.len(), values.len(), "tuple cast arity mismatch");
            let mut values_cast = Vec::with_capacity(values.len());
            for (typ, value) in typs.iter().zip(values) {
                values_cast.push(unwrap!(cast_up(arena, ctx, typ, value)));
            }
            unwrap_from_result!(
                value::make::tuple(arena, typ.node.clone().into(), values_cast, Span::default()),
                span
            )
        }
        // Option: the payload
        ast::TypKind::Iter(typ_inner, ast::Iter::Opt) => {
            let value = value::get::opt(arena, &value).expect("operand must be an option");
            let value = match value {
                Some(value) => Some(unwrap!(cast_up(arena, ctx, typ_inner, value))),
                None => None,
            };
            unwrap_from_result!(
                value::make::opt(arena, typ_inner.node.clone().into(), value, Span::default()),
                span
            )
        }
        // List: every element
        ast::TypKind::Iter(typ_inner, ast::Iter::List) => {
            let values = value::get::list(arena, &value)
                .expect("operand must be a list")
                .to_vec();
            let mut values_cast = Vec::with_capacity(values.len());
            for value in values {
                values_cast.push(unwrap!(cast_up(arena, ctx, typ_inner, value)));
            }
            unwrap_from_result!(
                value::make::list(
                    arena,
                    typ_inner.node.clone().into(),
                    values_cast,
                    Span::default()
                ),
                span
            )
        }
        // Other types need no representation change
        _ => value,
    };
    ok!(result)
}

// - Downcast

/// Downcasts a value to `typ` through aliases, tuples, and iterations.
pub(crate) fn cast_down(
    arena: &mut Arena,
    ctx: &impl ReadContext,
    typ: &ast::Typ,
    value: Value,
) -> Backtrack<Value> {
    let span = &typ.span;
    let result = match &typ.node {
        // Integer to natural, failing on negatives
        ast::TypKind::Num(num::Typ::Nat) => {
            let num = value::get::num(arena, &value).expect("operand must be a number");
            match num {
                num::Number::Nat(_) => value,
                num::Number::Int(num) => {
                    let num = unwrap_from_result!(num::Natural::try_from(num.clone()), span);
                    unwrap_from_result!(value::make::nat(arena, num, Span::default()), span)
                }
            }
        }
        // Named type: unfold a plain alias; variants and structs stay as is
        ast::TypKind::Var(id, targs) => {
            let (tparams, def_typ) = unwrap_from_result!(ctx.find_defined_typdef(id), span);
            let theta = unwrap_from_result!(Theta::from_lists(tparams, targs), span);
            match &def_typ.node {
                ast::DefTypKind::Plain(typ) => {
                    let typ = unwrap_from_result!(subst_typ(&|id| theta.get(id), typ), span);
                    return cast_down(arena, ctx, &typ, value);
                }
                _ => value,
            }
        }
        // Tuple: componentwise
        ast::TypKind::Tuple(typs) => {
            let values = value::get::tuple(arena, &value)
                .expect("operand must be a tuple")
                .to_vec();
            assert_eq!(typs.len(), values.len(), "tuple cast arity mismatch");
            let mut values_cast = Vec::with_capacity(values.len());
            for (typ, value) in typs.iter().zip(values) {
                values_cast.push(unwrap!(cast_down(arena, ctx, typ, value)));
            }
            unwrap_from_result!(
                value::make::tuple(arena, typ.node.clone().into(), values_cast, Span::default()),
                span
            )
        }
        // Option: the payload
        ast::TypKind::Iter(typ_inner, ast::Iter::Opt) => {
            let value = value::get::opt(arena, &value).expect("operand must be an option");
            let value = match value {
                Some(value) => Some(unwrap!(cast_down(arena, ctx, typ_inner, value))),
                None => None,
            };
            unwrap_from_result!(
                value::make::opt(arena, typ_inner.node.clone().into(), value, Span::default()),
                span
            )
        }
        // List: every element
        ast::TypKind::Iter(typ_inner, ast::Iter::List) => {
            let values = value::get::list(arena, &value)
                .expect("operand must be a list")
                .to_vec();
            let mut values_cast = Vec::with_capacity(values.len());
            for value in values {
                values_cast.push(unwrap!(cast_down(arena, ctx, typ_inner, value)));
            }
            unwrap_from_result!(
                value::make::list(
                    arena,
                    typ_inner.node.clone().into(),
                    values_cast,
                    Span::default()
                ),
                span
            )
        }
        // Other types need no representation change
        _ => value,
    };
    ok!(result)
}

// = Access

// - Field access

/// Reads a struct field by atom.
pub(crate) fn access_dot(
    arena: &Arena,
    value: &Value,
    atom: &ast::Atom,
    _span: &Span,
) -> Backtrack<Value> {
    let value_fields = value::get::structure(arena, value).expect("operand must be a structure");
    match value_fields
        .iter()
        .find(|value_field| value_field.atom.node == atom.node)
    {
        Some(value_field) => ok!(value_field.value),
        None => unreachable!("structure must contain the field"),
    }
}

/// Reads a number as an integer.
fn get_int(arena: &Arena, value: &Value, _span: &Span) -> Backtrack<BigInt> {
    let num = value::get::num(arena, value).expect("operand must be a number");
    ok!(num::to_int(num).clone())
}

// - Index access

/// Indexes a text or list; a text index yields the one-character text.
pub(crate) fn access_index(
    arena: &mut Arena,
    value_base: &Value,
    value_idx: &Value,
    span_base: &Span,
    span_idx: &Span,
) -> Backtrack<Value> {
    // The operand must be a text or list and the index in bounds
    let int_idx = unwrap!(get_int(arena, value_idx, span_idx));
    let len = match arena.value.kind(value_base.node) {
        ValueKind::Text(text) => text.len(),
        ValueKind::List(values) => values.len(),
        _ => unreachable!("index operand must be a text or list"),
    };
    let Some(idx) = usize::try_from(&int_idx).ok().filter(|idx| *idx < len) else {
        return fatal!(span_idx.clone(), error::expr::index_out_of_bounds(int_idx, len),);
    };
    match arena.value.kind(value_base.node) {
        // Text: a one-character slice
        ValueKind::Text(_) => {
            let typ = crate::phrase!(node: arena.value.typ(value_base.note).clone(), span: arena.value.span(value_base.span).clone());
            let value_len = unwrap_from_result!(
                value::make::nat(arena, 1u64.into(), Span::default()),
                span_idx
            );
            access_slice(
                arena, value_base, value_idx, &value_len, &typ.node, &typ.span, span_base,
                span_idx, span_idx, span_idx,
            )
        }
        // List: the element
        ValueKind::List(values) => ok!(values[idx]),
        _ => unreachable!(),
    }
}

// - Slice access

#[expect(clippy::too_many_arguments, reason = "operand and bounds spans remain explicit")]
/// Slices a text or list; a text slice must cut on UTF-8 boundaries.
pub(crate) fn access_slice(
    arena: &mut Arena,
    value_base: &Value,
    value_idx: &Value,
    value_len: &Value,
    typ: &Rc<ast::TypKind>,
    span_typ: &Span,
    _span_base: &Span,
    span_idx: &Span,
    span_len: &Span,
    span_bounds: &Span,
) -> Backtrack<Value> {
    // The operand must be a text or list and the range within it
    let int_idx = unwrap!(get_int(arena, value_idx, span_idx));
    let int_len = unwrap!(get_int(arena, value_len, span_len));
    let size = match arena.value.kind(value_base.node) {
        ValueKind::Text(text) => text.len(),
        ValueKind::List(values) => values.len(),
        _ => unreachable!("slice operand must be a text or list"),
    };
    let Some((idx, idx_end)) = usize::try_from(&int_idx)
        .ok()
        .zip(usize::try_from(&int_len).ok())
        .and_then(|(idx, len)| idx.checked_add(len).map(|idx_end| (idx, idx_end)))
        .filter(|(_, idx_end)| *idx_end <= size)
    else {
        let int_end = &int_idx + &int_len;
        return fatal!(
            span_bounds.clone(),
            error::expr::slice_out_of_bounds(int_idx, int_end, size),
        );
    };
    match arena.value.kind(value_base.node) {
        // Text: the byte range must fall on character boundaries
        ValueKind::Text(text) => match text.get(idx..idx_end) {
            Some(text) => {
                let text = text.to_owned();
                backtrack::from_result(value::make::text(arena, text, Span::default()), span_typ)
            }
            None => {
                fatal!(span_bounds.clone(), error::expr::text_slice_boundary_mismatch(),)
            }
        },
        // List: copy the range
        ValueKind::List(values) => {
            let values = values[idx..idx_end].to_vec();
            ok!(unwrap_from_result!(
                value::make::list(arena, typ.clone(), values, Span::default()),
                span_typ
            ))
        }
        _ => unreachable!(),
    }
}

// = Updates

// - Index update

/// Replaces one element of a list or one character of a text.
pub(crate) fn update_index(
    arena: &mut Arena,
    value_base: &Value,
    value_idx: &Value,
    value_upd: Value,
    typ: &Phrase<Rc<ast::TypKind>>,
    span_base: &Span,
    span_idx: &Span,
) -> Backtrack<Value> {
    // Operand and index checks as for access
    let int_idx = unwrap!(get_int(arena, value_idx, span_idx));
    let len = match arena.value.kind(value_base.node) {
        ValueKind::Text(text) => text.len(),
        ValueKind::List(values) => values.len(),
        _ => unreachable!("index operand must be a text or list"),
    };
    let Some(idx) = usize::try_from(&int_idx).ok().filter(|idx| *idx < len) else {
        return fatal!(span_idx.clone(), error::expr::index_out_of_bounds(int_idx, len),);
    };
    let value = match arena.value.kind(value_base.node) {
        // Text: the replacement must be a single character
        ValueKind::Text(text) => {
            let size = text.len();
            let text_upd = value::get::text(arena, &value_upd).expect("operand must be a text");
            if text_upd.len() != 1 {
                return fatal!(span_idx.clone(), error::expr::character_update_length_mismatch(),);
            }
            // Rebuild as prefix, replacement, suffix
            let text_upd = text_upd.to_owned();
            let value_l_idx = unwrap_from_result!(
                value::make::int(arena, (0).into(), Span::default()),
                &typ.span
            );
            let value_l_len = unwrap_from_result!(
                value::make::int(arena, (idx).into(), Span::default()),
                &typ.span
            );
            let value_l = unwrap!(access_slice(
                arena,
                value_base,
                &value_l_idx,
                &value_l_len,
                &typ.node,
                &typ.span,
                span_base,
                span_idx,
                span_idx,
                span_idx
            ));
            let value_r_idx = unwrap_from_result!(
                value::make::int(arena, (idx + 1).into(), Span::default()),
                &typ.span
            );
            let value_r_len = unwrap_from_result!(
                value::make::int(arena, (size - (idx + 1)).into(), Span::default()),
                &typ.span
            );
            let value_r = unwrap!(access_slice(
                arena,
                value_base,
                &value_r_idx,
                &value_r_len,
                &typ.node,
                &typ.span,
                span_base,
                span_idx,
                span_idx,
                span_idx
            ));
            let text_l = value::get::text(arena, &value_l).expect("operand must be a text");
            let text_r = value::get::text(arena, &value_r).expect("operand must be a text");
            {
                let text = format!("{text_l}{text_upd}{text_r}");
                unwrap_from_result!(value::make::text(arena, text, Span::default()), &typ.span)
            }
        }
        // List: replace in a copy
        ValueKind::List(values) => {
            let mut values = values.clone();
            values[idx] = value_upd;
            unwrap_from_result!(
                value::make::list(arena, typ.node.clone(), values, Span::default()),
                &typ.span
            )
        }
        _ => unreachable!(),
    };
    ok!(value)
}

// - Slice update

#[expect(clippy::too_many_arguments, reason = "operand spans remain explicit")]
/// Replaces a range of a list or text with a value of the same length.
pub(crate) fn update_slice(
    arena: &mut Arena,
    value_base: &Value,
    value_idx: &Value,
    value_len: &Value,
    value_upd: Value,
    typ: &Phrase<Rc<ast::TypKind>>,
    span_base: &Span,
    span_idx: &Span,
    span_len: &Span,
) -> Backtrack<Value> {
    // Operand and range checks as for access
    let int_idx = unwrap!(get_int(arena, value_idx, span_idx));
    let int_len = unwrap!(get_int(arena, value_len, span_len));
    let size = match arena.value.kind(value_base.node) {
        ValueKind::Text(text) => text.len(),
        ValueKind::List(values) => values.len(),
        _ => unreachable!("slice operand must be a text or list"),
    };
    let Some((idx, idx_end)) = usize::try_from(&int_idx)
        .ok()
        .zip(usize::try_from(&int_len).ok())
        .and_then(|(idx, len)| idx.checked_add(len).map(|idx_end| (idx, idx_end)))
        .filter(|(_, idx_end)| *idx_end <= size)
    else {
        let int_end = &int_idx + &int_len;
        return fatal!(span_len.clone(), error::expr::slice_out_of_bounds(int_idx, int_end, size),);
    };
    let value = match arena.value.kind(value_base.node) {
        // Text: the replacement must have the range's length
        ValueKind::Text(text) => {
            let size = text.len();
            let text_upd = value::get::text(arena, &value_upd).expect("operand must be a text");
            if text_upd.len() != idx_end - idx {
                return fatal!(
                    span_len.clone(),
                    error::expr::text_slice_update_length_mismatch(idx_end - idx, text_upd.len()),
                );
            }
            // Rebuild as prefix, replacement, suffix
            let text_upd = text_upd.to_owned();
            let value_l_idx = unwrap_from_result!(
                value::make::int(arena, (0).into(), Span::default()),
                &typ.span
            );
            let value_l_len = unwrap_from_result!(
                value::make::int(arena, (idx).into(), Span::default()),
                &typ.span
            );
            let value_l = unwrap!(access_slice(
                arena,
                value_base,
                &value_l_idx,
                &value_l_len,
                &typ.node,
                &typ.span,
                span_base,
                span_len,
                span_len,
                span_len
            ));
            let value_r_idx = unwrap_from_result!(
                value::make::int(arena, (idx_end).into(), Span::default()),
                &typ.span
            );
            let value_r_len = unwrap_from_result!(
                value::make::int(arena, (size - (idx_end)).into(), Span::default()),
                &typ.span
            );
            let value_r = unwrap!(access_slice(
                arena,
                value_base,
                &value_r_idx,
                &value_r_len,
                &typ.node,
                &typ.span,
                span_base,
                span_len,
                span_len,
                span_len
            ));
            let text_l = value::get::text(arena, &value_l).expect("operand must be a text");
            let text_r = value::get::text(arena, &value_r).expect("operand must be a text");
            {
                let text = format!("{text_l}{text_upd}{text_r}");
                unwrap_from_result!(value::make::text(arena, text, Span::default()), &typ.span)
            }
        }
        // List: the replacement must have the range's length
        ValueKind::List(values) => {
            let values_upd = value::get::list(arena, &value_upd).expect("operand must be a list");
            if values_upd.len() != idx_end - idx {
                return fatal!(
                    span_len.clone(),
                    error::expr::list_slice_update_length_mismatch(idx_end - idx, values_upd.len()),
                );
            }
            let mut values = values.clone();
            values[idx..idx_end].clone_from_slice(values_upd);
            unwrap_from_result!(
                value::make::list(arena, typ.node.clone(), values, Span::default()),
                &typ.span
            )
        }
        _ => unreachable!(),
    };
    ok!(value)
}
