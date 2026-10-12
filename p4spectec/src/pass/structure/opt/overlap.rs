//! Compare conditions syntactically to guide If merging and case analysis
//!
//! ```text
//! p        vs p        -> Identical
//! x == 1   vs x == 2   -> Disjoint
//! p        vs !p       -> Partition
//! p        vs q        -> Fuzzy
//! ```
//!
//! A partition covers every case;
//! disjoint conditions may leave cases uncovered.
//! Fuzzy means the analysis cannot decide whether the conditions overlap.

use std::rc::Rc;

use crate::lang::{common::prim::bool, traits::eq::SyntaxEq};

use crate::lang::il::ast::*;

use crate::lang::sl::ast::Guard;

use crate::runtime::{envs::algo::TDEnv, ops::typ::expand_typ, typdef::TypeDef};

use crate::pass::structure::error::{self, StructureError};

// == Overlap results

/// How two conditions on the same value relate.
#[derive(Clone, Debug, PartialEq)]
pub(crate) enum Overlap {
    /// Syntactically the same condition.
    Identical,
    /// Never both true; together they may leave values uncovered.
    Disjoint { exp: Exp, guard_a: Guard, guard_b: Guard },
    /// Exactly one of them holds for every value.
    Partition { exp: Exp, guard_a: Guard, guard_b: Guard },
    /// Unknown relation; the conditions are kept apart.
    Fuzzy,
}

// == Guard conversion

// - Expressions to guards

/// Reads a condition as a guard on `exp_target`: `x == 1` becomes `Cmp(==, 1)`.
pub(crate) fn exp_as_guard(exp_target: &Exp, exp_cond: &Exp) -> Option<Guard> {
    match &exp_cond.node {
        // Negation of the target itself
        ExpKind::Un(UnOp::Bool(bool::UnOp::Not), _, exp) if exp_target.syntax_eq(exp.as_ref()) => {
            Some(Guard::Bool(false))
        }
        // Equality or inequality with the target on either side
        ExpKind::Cmp(op @ CmpOp::Bool(bool::CmpOp::Eq | bool::CmpOp::Ne), optyp, exp_l, exp_r)
            if exp_target.syntax_eq(exp_l) =>
        {
            Some(Guard::Cmp(*op, *optyp, exp_r.as_ref().clone()))
        }
        ExpKind::Cmp(op @ CmpOp::Bool(bool::CmpOp::Eq | bool::CmpOp::Ne), optyp, exp_l, exp_r)
            if exp_target.syntax_eq(exp_r) =>
        {
            Some(Guard::Cmp(*op, *optyp, exp_l.as_ref().clone()))
        }
        // Subtype, pattern, and membership tests on the target
        ExpKind::Sub(exp, typ, subcheck) if exp_target.syntax_eq(exp.as_ref()) => {
            Some(Guard::Sub(typ.as_ref().clone(), subcheck.clone()))
        }
        ExpKind::Match(exp, pattern) if exp_target.syntax_eq(exp.as_ref()) => {
            Some(Guard::Match(pattern.clone()))
        }
        ExpKind::Mem(exp_elem, exp_set) if exp_target.syntax_eq(exp_elem.as_ref()) => {
            Some(Guard::Mem(exp_set.as_ref().clone()))
        }
        _ => None,
    }
}

// - Guards to expressions

/// Rebuilds the condition a guard stands for on `exp_target`.
pub(crate) fn guard_as_exp(exp_target: &Exp, guard: &Guard) -> Exp {
    let exp_kind = match guard {
        Guard::Bool(true) => return exp_target.clone(),
        Guard::Bool(false) => {
            ExpKind::Un(UnOp::Bool(bool::UnOp::Not), OpTyp::Bool, Box::new(exp_target.clone()))
        }
        Guard::Cmp(op, optyp, exp) => {
            ExpKind::Cmp(*op, *optyp, Box::new(exp_target.clone()), Box::new(exp.clone()))
        }
        Guard::Sub(typ, subcheck) => {
            ExpKind::Sub(Box::new(exp_target.clone()), Box::new(typ.clone()), subcheck.clone())
        }
        Guard::Match(pattern) => ExpKind::Match(Box::new(exp_target.clone()), pattern.clone()),
        Guard::Mem(exp) => ExpKind::Mem(Box::new(exp_target.clone()), Box::new(exp.clone())),
    };
    crate::note_phrase!(node: exp_kind, note: TypKind::Bool, span: exp_target.span)
}

// == Condition overlap

/// Compares two guards on the same value by comparing their conditions.
pub(crate) fn overlap_guard(
    tdenv: &TDEnv,
    exp: &Exp,
    guard_a: &Guard,
    guard_b: &Guard,
) -> Result<Overlap, StructureError> {
    let exp_a = guard_as_exp(exp, guard_a);
    let exp_b = guard_as_exp(exp, guard_b);
    overlap_exp(tdenv, &exp_a, &exp_b)
}

/// Classifies how two conditions relate; the module header lists the cases.
pub(crate) fn overlap_exp(
    tdenv: &TDEnv,
    exp_a: &Exp,
    exp_b: &Exp,
) -> Result<Overlap, StructureError> {
    // x == 1 vs x == 1 -> Identical
    if exp_a.syntax_eq(exp_b) {
        return Ok(Overlap::Identical);
    }
    match (&exp_a.node, &exp_b.node) {
        // Negation: !p vs p -> Partition
        (ExpKind::Un(UnOp::Bool(bool::UnOp::Not), _, exp_inner), _)
            if exp_inner.as_ref().syntax_eq(exp_b) =>
        {
            Ok(Overlap::Partition {
                exp: exp_inner.as_ref().clone(),
                guard_a: Guard::Bool(false),
                guard_b: Guard::Bool(true),
            })
        }
        // p vs !p -> Partition, with the guards in the opposite order
        (_, ExpKind::Un(UnOp::Bool(bool::UnOp::Not), _, exp_inner))
            if exp_a.syntax_eq(exp_inner.as_ref()) =>
        {
            Ok(Overlap::Partition {
                exp: exp_inner.as_ref().clone(),
                guard_a: Guard::Bool(true),
                guard_b: Guard::Bool(false),
            })
        }
        // Equals literal
        // x == true vs x == false -> Partition
        (
            ExpKind::Cmp(op_a @ CmpOp::Bool(bool::CmpOp::Eq), optyp_a, exp_a_l, exp_a_r),
            ExpKind::Cmp(op_b @ CmpOp::Bool(bool::CmpOp::Eq), optyp_b, exp_b_l, exp_b_r),
        ) if optyp_a == optyp_b
            && exp_a_l.syntax_eq(exp_b_l)
            && partition_exp_literal(exp_a_r, exp_b_r) =>
        {
            Ok(Overlap::Partition {
                exp: exp_a_l.as_ref().clone(),
                guard_a: Guard::Cmp(*op_a, *optyp_a, exp_a_r.as_ref().clone()),
                guard_b: Guard::Cmp(*op_b, *optyp_b, exp_b_r.as_ref().clone()),
            })
        }
        // x == true vs false == x -> Partition
        (
            ExpKind::Cmp(op_a @ CmpOp::Bool(bool::CmpOp::Eq), optyp_a, exp_a_l, exp_a_r),
            ExpKind::Cmp(op_b @ CmpOp::Bool(bool::CmpOp::Eq), optyp_b, exp_b_l, exp_b_r),
        ) if optyp_a == optyp_b
            && exp_a_l.syntax_eq(exp_b_r)
            && partition_exp_literal(exp_a_r, exp_b_l) =>
        {
            Ok(Overlap::Partition {
                exp: exp_a_l.as_ref().clone(),
                guard_a: Guard::Cmp(*op_a, *optyp_a, exp_a_r.as_ref().clone()),
                guard_b: Guard::Cmp(*op_b, *optyp_b, exp_b_l.as_ref().clone()),
            })
        }
        // x == 1 vs x == 2 -> Disjoint
        (
            ExpKind::Cmp(op_a @ CmpOp::Bool(bool::CmpOp::Eq), optyp_a, exp_a_l, exp_a_r),
            ExpKind::Cmp(op_b @ CmpOp::Bool(bool::CmpOp::Eq), optyp_b, exp_b_l, exp_b_r),
        ) if optyp_a == optyp_b
            && exp_a_l.syntax_eq(exp_b_l)
            && disjoint_exp_literal(exp_a_r, exp_b_r) =>
        {
            Ok(Overlap::Disjoint {
                exp: exp_a_l.as_ref().clone(),
                guard_a: Guard::Cmp(*op_a, *optyp_a, exp_a_r.as_ref().clone()),
                guard_b: Guard::Cmp(*op_b, *optyp_b, exp_b_r.as_ref().clone()),
            })
        }
        // x == 1 vs 2 == x -> Disjoint
        (
            ExpKind::Cmp(op_a @ CmpOp::Bool(bool::CmpOp::Eq), optyp_a, exp_a_l, exp_a_r),
            ExpKind::Cmp(op_b @ CmpOp::Bool(bool::CmpOp::Eq), optyp_b, exp_b_l, exp_b_r),
        ) if optyp_a == optyp_b
            && exp_a_l.syntax_eq(exp_b_r)
            && disjoint_exp_literal(exp_a_r, exp_b_l) =>
        {
            Ok(Overlap::Disjoint {
                exp: exp_a_l.as_ref().clone(),
                guard_a: Guard::Cmp(*op_a, *optyp_a, exp_a_r.as_ref().clone()),
                guard_b: Guard::Cmp(*op_b, *optyp_b, exp_b_l.as_ref().clone()),
            })
        }
        // Equals and not equals
        // x == 1 vs x != 1 -> Partition
        (
            ExpKind::Cmp(op_a @ CmpOp::Bool(bool::CmpOp::Eq), optyp_a, exp_a_l, exp_a_r),
            ExpKind::Cmp(op_b @ CmpOp::Bool(bool::CmpOp::Ne), optyp_b, exp_b_l, exp_b_r),
        ) if optyp_a == optyp_b && exp_a_l.syntax_eq(exp_b_l) && exp_a_r.syntax_eq(exp_b_r) => {
            Ok(Overlap::Partition {
                exp: exp_a_l.as_ref().clone(),
                guard_a: Guard::Cmp(*op_a, *optyp_a, exp_a_r.as_ref().clone()),
                guard_b: Guard::Cmp(*op_b, *optyp_b, exp_b_r.as_ref().clone()),
            })
        }
        // x == 1 vs 1 != x -> Partition
        (
            ExpKind::Cmp(op_a @ CmpOp::Bool(bool::CmpOp::Eq), optyp_a, exp_a_l, exp_a_r),
            ExpKind::Cmp(op_b @ CmpOp::Bool(bool::CmpOp::Ne), optyp_b, exp_b_l, exp_b_r),
        ) if optyp_a == optyp_b && exp_a_l.syntax_eq(exp_b_r) && exp_a_r.syntax_eq(exp_b_l) => {
            Ok(Overlap::Partition {
                exp: exp_a_l.as_ref().clone(),
                guard_a: Guard::Cmp(*op_a, *optyp_a, exp_a_r.as_ref().clone()),
                guard_b: Guard::Cmp(*op_b, *optyp_b, exp_b_l.as_ref().clone()),
            })
        }
        // Subtyping: x <: T vs x <: U, with T={A}, U={B} -> Disjoint
        (ExpKind::Sub(exp_a, typ_a, subcheck_a), ExpKind::Sub(exp_b, typ_b, subcheck_b))
            if exp_a.syntax_eq(exp_b) =>
        {
            overlap_sub_exp(tdenv, exp_a, typ_a, subcheck_a, typ_b, subcheck_b)
        }
        // Match on patterns: x matches Some vs x matches None -> Partition
        (ExpKind::Match(exp_a, pattern_a), ExpKind::Match(exp_b, pattern_b))
            if exp_a.syntax_eq(exp_b) =>
        {
            Ok(overlap_match_exp(exp_a, pattern_a, pattern_b))
        }
        // x <: T vs x matches B, with T={A} -> Disjoint
        (ExpKind::Sub(exp_a, typ_a, subcheck_a), ExpKind::Match(exp_b, pattern_b))
            if exp_a.syntax_eq(exp_b) =>
        {
            overlap_sub_match_exp(tdenv, exp_a, typ_a, subcheck_a, pattern_b)
        }
        // x matches B vs x <: T, with T={A} -> Disjoint in input order
        (ExpKind::Match(exp_a, pattern_a), ExpKind::Sub(exp_b, typ_b, subcheck_b))
            if exp_a.syntax_eq(exp_b) =>
        {
            overlap_match_sub_exp(tdenv, exp_a, pattern_a, typ_b, subcheck_b)
        }
        // x in [1, 2] vs x in [3] -> Disjoint
        (ExpKind::Mem(exp_elem_a, exp_set_a), ExpKind::Mem(exp_elem_b, exp_set_b))
            if exp_elem_a.syntax_eq(exp_elem_b) =>
        {
            let (ExpKind::List(exps_a), ExpKind::List(exps_b)) = (&exp_set_a.node, &exp_set_b.node)
            else {
                // x in ys vs x in [3] -> Fuzzy
                return Ok(Overlap::Fuzzy);
            };
            for exp_a in exps_a {
                for exp_b in exps_b {
                    // x in [1, 2] vs x in [2, 3] -> Fuzzy
                    if !disjoint_exp_literal(exp_a, exp_b) {
                        return Ok(Overlap::Fuzzy);
                    }
                }
            }
            Ok(Overlap::Disjoint {
                exp: exp_elem_a.as_ref().clone(),
                guard_a: Guard::Mem(exp_set_a.as_ref().clone()),
                guard_b: Guard::Mem(exp_set_b.as_ref().clone()),
            })
        }
        // x != 1 vs x == 1 -> Fuzzy: comparison rules require Eq first
        _ => Ok(Overlap::Fuzzy),
    }
}

// - Helper for subtyping

/// Lists the constructors of a variant type through aliases; `None` otherwise.
pub(crate) fn typ_as_variant(
    tdenv: &TDEnv,
    typ: &Typ,
) -> Result<Option<Vec<Rc<Mixop>>>, StructureError> {
    // Only a defined variant type has constructors
    let typ_unrolled = expand_typ(tdenv, typ).map_err(error::type_operation_invalid)?;
    let TypKind::Var(id, _) = &typ_unrolled.node else {
        return Ok(None);
    };
    match tdenv.get(id) {
        Some(TypeDef::Defined(_, deftyp)) => Ok(match &deftyp.node {
            DefTypKind::Variant(typ_cases) => Some(
                typ_cases
                    .iter()
                    .map(|TypCase { not_typ: nottyp, .. }| Rc::clone(nottyp.node.mixop()))
                    .collect(),
            ),
            _ => None,
        }),
        _ => Ok(None),
    }
}

// - Subtyping

/// Compares two subtype tests on the same value by their constructor sets.
fn overlap_sub_exp(
    tdenv: &TDEnv,
    exp: &Exp,
    typ_a: &Typ,
    subcheck_a: &Subcheck,
    typ_b: &Typ,
    subcheck_b: &Subcheck,
) -> Result<Overlap, StructureError> {
    let mixops_a = typ_as_variant(tdenv, typ_a)?;
    let mixops_b = typ_as_variant(tdenv, typ_b)?;
    let (Some(mixops_a), Some(mixops_b)) = (mixops_a, mixops_b) else {
        // x <: bool vs x <: int -> Fuzzy: neither type is a variant
        return Ok(Overlap::Fuzzy);
    };
    let contains = |mixops: &[Rc<Mixop>], mixop: &Rc<Mixop>| {
        mixops
            .iter()
            .any(|mixop_other| mixop.syntax_eq(mixop_other))
    };
    if mixops_a.iter().all(|mixop| contains(&mixops_b, mixop))
        && mixops_b.iter().all(|mixop| contains(&mixops_a, mixop))
    {
        // Variant tags {A, B} vs {B, A} -> Identical
        Ok(Overlap::Identical)
    } else if !mixops_a.iter().any(|mixop| contains(&mixops_b, mixop)) {
        // Variant tags {A} vs {B, C} -> Disjoint
        Ok(Overlap::Disjoint {
            exp: exp.clone(),
            guard_a: Guard::Sub(typ_a.clone(), Box::new(subcheck_a.clone())),
            guard_b: Guard::Sub(typ_b.clone(), Box::new(subcheck_b.clone())),
        })
    } else {
        // Variant tags {A, B} vs {B, C} -> Fuzzy
        Ok(Overlap::Fuzzy)
    }
}

// - Patterns

/// Compares two pattern matches on the same value.
fn overlap_match_exp(exp: &Exp, pattern_a: &Pattern, pattern_b: &Pattern) -> Overlap {
    // Some vs Some -> Identical
    if pattern_a.syntax_eq(pattern_b) {
        return Overlap::Identical;
    }
    let guard_a = Guard::Match(pattern_a.clone());
    let guard_b = Guard::Match(pattern_b.clone());
    match (pattern_a, pattern_b) {
        // A vs B -> Disjoint
        (Pattern::Case(_), Pattern::Case(_)) => {
            Overlap::Disjoint { exp: exp.clone(), guard_a, guard_b }
        }
        // Cons vs Fixed(0) -> Partition; Cons vs Fixed(2) -> Disjoint
        (Pattern::List(ListPattern::Cons), Pattern::List(ListPattern::Fixed(num)))
        | (Pattern::List(ListPattern::Fixed(num)), Pattern::List(ListPattern::Cons)) => {
            if *num == 0 {
                Overlap::Partition { exp: exp.clone(), guard_a, guard_b }
            } else {
                Overlap::Disjoint { exp: exp.clone(), guard_a, guard_b }
            }
        }
        // Cons vs Nil -> Partition
        (Pattern::List(ListPattern::Cons), Pattern::List(ListPattern::Nil))
        | (Pattern::List(ListPattern::Nil), Pattern::List(ListPattern::Cons)) => {
            Overlap::Partition { exp: exp.clone(), guard_a, guard_b }
        }
        // Fixed(1) vs Fixed(2) -> Disjoint
        (Pattern::List(ListPattern::Fixed(_)), Pattern::List(ListPattern::Fixed(_))) => {
            Overlap::Disjoint { exp: exp.clone(), guard_a, guard_b }
        }
        // Fixed(0) vs Nil -> Identical; Fixed(2) vs Nil -> Disjoint
        (Pattern::List(ListPattern::Fixed(num)), Pattern::List(ListPattern::Nil))
        | (Pattern::List(ListPattern::Nil), Pattern::List(ListPattern::Fixed(num))) => {
            if *num == 0 {
                Overlap::Identical
            } else {
                Overlap::Disjoint { exp: exp.clone(), guard_a, guard_b }
            }
        }
        // Some vs None -> Partition
        (Pattern::Opt(OptPattern::Some), Pattern::Opt(OptPattern::None))
        | (Pattern::Opt(OptPattern::None), Pattern::Opt(OptPattern::Some)) => {
            Overlap::Partition { exp: exp.clone(), guard_a, guard_b }
        }
        // Case(A) vs Opt(Some) -> Fuzzy
        _ => Overlap::Fuzzy,
    }
}

// - Subtyping and patterns

/// Compares a subtype test with a constructor match on the same value.
fn overlap_sub_match_exp(
    tdenv: &TDEnv,
    exp: &Exp,
    typ: &Typ,
    subcheck: &Subcheck,
    pattern: &Pattern,
) -> Result<Overlap, StructureError> {
    let Pattern::Case(mixop) = pattern else {
        // x <: T vs x matches Some -> Fuzzy
        return Ok(Overlap::Fuzzy);
    };
    // x <: bool vs x matches A -> Fuzzy: bool has no variant tags
    let Some(mixops) = typ_as_variant(tdenv, typ)? else {
        return Ok(Overlap::Fuzzy);
    };
    // x <: T vs x matches A, with T={A, B} -> Fuzzy
    if mixops
        .iter()
        .any(|mixop_other| mixop.as_ref().syntax_eq(mixop_other))
    {
        return Ok(Overlap::Fuzzy);
    }
    // x <: T vs x matches C, with T={A, B} -> Disjoint
    Ok(Overlap::Disjoint {
        exp: exp.clone(),
        guard_a: Guard::Sub(typ.clone(), Box::new(subcheck.clone())),
        guard_b: Guard::Match(pattern.clone()),
    })
}

/// Like `overlap_sub_match_exp`, with the guards kept in input order.
fn overlap_match_sub_exp(
    tdenv: &TDEnv,
    exp: &Exp,
    pattern: &Pattern,
    typ: &Typ,
    subcheck: &Subcheck,
) -> Result<Overlap, StructureError> {
    // x matches C vs x <: T keeps Match first and Sub second
    let overlap = overlap_sub_match_exp(tdenv, exp, typ, subcheck, pattern)?;
    Ok(match overlap {
        Overlap::Disjoint { exp, guard_a, guard_b } => {
            Overlap::Disjoint { exp, guard_a: guard_b, guard_b: guard_a }
        }
        overlap => overlap,
    })
}

// == Literal comparison

// - Partitions

/// Checks for `true` against `false`, the only literal pair that partitions.
fn partition_exp_literal(exp_a: &Exp, exp_b: &Exp) -> bool {
    matches!((&exp_a.node, &exp_b.node), (ExpKind::Bool(bool_a), ExpKind::Bool(bool_b)) if bool_a != bool_b)
}

// - Disjointness

/// Checks whether two literal expressions can never be equal.
fn disjoint_exp_literal(exp_a: &Exp, exp_b: &Exp) -> bool {
    match (&exp_a.node, &exp_b.node) {
        // true vs false -> disjoint
        (ExpKind::Bool(bool_a), ExpKind::Bool(bool_b)) => bool_a != bool_b,
        // 1 vs 2 -> disjoint
        (ExpKind::Num(num_a), ExpKind::Num(num_b)) => num_a != num_b,
        // "a" vs "b" -> disjoint
        (ExpKind::Text(text_a), ExpKind::Text(text_b)) => text_a != text_b,
        // UpCast(T, 1) vs UpCast(T, 2) -> compare 1 and 2
        (ExpKind::UpCast(typ_a, exp_a), ExpKind::UpCast(typ_b, exp_b))
            if typ_a.syntax_eq(typ_b) =>
        {
            disjoint_exp_literal(exp_a, exp_b)
        }
        // (1, true) vs (1, false) -> disjoint at the second element
        (ExpKind::Tuple(exps_a), ExpKind::Tuple(exps_b)) => disjoint_exps_literal(
            &exps_a.iter().collect::<Vec<_>>(),
            &exps_b.iter().collect::<Vec<_>>(),
        ),
        // A(1) vs B(1) -> disjoint; A(1) vs A(2) -> compare arguments
        (ExpKind::Case(notexp_a), ExpKind::Case(notexp_b)) => {
            if !notexp_a.eq_mixop(notexp_b) {
                return true;
            }
            disjoint_exps_literal(
                &notexp_a.args().iter().collect::<Vec<_>>(),
                &notexp_b.args().iter().collect::<Vec<_>>(),
            )
        }
        // [] vs [1] -> disjoint by length; [1] vs [2] -> compare elements
        (ExpKind::List(exps_a), ExpKind::List(exps_b)) => {
            if exps_a.len() != exps_b.len() {
                return true;
            }
            let exps_a = exps_a.iter().collect::<Vec<_>>();
            let exps_b = exps_b.iter().collect::<Vec<_>>();
            disjoint_exps_literal(&exps_a, &exps_b)
        }
        // x vs y -> unknown, so not proven disjoint
        _ => false,
    }
}

/// Checks literal lists pairwise; one disjoint position suffices.
fn disjoint_exps_literal(exps_a: &[&Exp], exps_b: &[&Exp]) -> bool {
    // Typed tuples, matching mixfix shapes, and checked list lengths agree
    assert_eq!(exps_a.len(), exps_b.len(), "validated literal arity");
    for (exp_a, exp_b) in exps_a.iter().zip(exps_b) {
        if disjoint_exp_literal(exp_a, exp_b) {
            return true;
        }
    }
    false
}
