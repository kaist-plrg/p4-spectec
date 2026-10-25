//! Decodes IL meta data into elaborated IL syntax
//!
//! Expressions, types, paths, arguments, and parameters use the decoders of
//! `meta::shared::decode`.
//! Relation notation, otherwise markers, and general hints are omitted
//! from the meta syntax and reconstructed canonically by `stub`.

use num_traits::ToPrimitive;

use super::{mixops as m, stub};
use crate::{
    interface::meta::shared::{Cache, Error, decode::Decode},
    lang::{
        common::{Id, Iter, prim::num::Number},
        data::{
            arena::Arena,
            value::flat::{Value, get},
        },
        hints::input::InputHint,
        il::ast as il,
    },
    phrase,
};

// == Decoding

// - Input positions

/// Decodes input positions in their declared order.
impl Decode for InputHint {
    fn decode_cached(
        arena: &mut Arena,
        _cache: &mut Cache,
        value_meta: Value,
    ) -> Result<Self, Error> {
        let mut indices = Vec::new();
        for value_idx in get::list(arena, &value_meta)?.to_vec() {
            let span = *arena.value.span(value_idx.span);
            let idx = match get::num(arena, &value_idx)? {
                Number::Nat(nat) => nat.as_bigint().to_usize(),
                Number::Int(_) => None,
            }
            .ok_or_else(|| Error::unexpected(arena, value_idx, "an input position"))?;
            indices.push(phrase!(node: idx, span: span));
        }
        Ok(InputHint::new(indices))
    }
}

// - Premises

/// Decodes an IL premise with positional relation notation.
impl Decode for il::Prem {
    fn decode_cached(
        arena: &mut Arena,
        cache: &mut Cache,
        value_meta: Value,
    ) -> Result<Self, Error> {
        let span = *arena.value.span(value_meta.span);
        let prem_kind_il = get::matches_string! { arena, &value_meta,
            m::PREM_REL => |[value_id, value_exps, value_input]| {
                il::PremKind::Rule(il::RulePrem {
                    id: Id::decode_cached(arena, cache, *value_id)?,
                    not_exp: stub::notation(Vec::<il::Exp>::decode_cached(arena, cache, *value_exps)?)?,
                    input_hint: InputHint::decode_cached(arena, cache, *value_input)?,
                })
            },
            m::PREM_IF => |[value_exp]| {
                il::PremKind::If(il::IfPrem { exp: il::Exp::decode_cached(arena, cache, *value_exp)? })
            },
            m::PREM_IFHOLD => |[value_id, value_exps]| {
                il::PremKind::IfHold(il::IfHoldPrem {
                    id: Id::decode_cached(arena, cache, *value_id)?,
                    not_exp: stub::notation(Vec::<il::Exp>::decode_cached(arena, cache, *value_exps)?)?,
                })
            },
            m::PREM_IFNOTHOLD => |[value_id, value_exps]| {
                il::PremKind::IfNotHold(il::IfNotHoldPrem {
                    id: Id::decode_cached(arena, cache, *value_id)?,
                    not_exp: stub::notation(Vec::<il::Exp>::decode_cached(arena, cache, *value_exps)?)?,
                })
            },
            m::PREM_ITER => |[value_prem, value_iter]| {
                il::PremKind::Iter(il::IterPrem {
                    prem: Box::new(il::Prem::decode_cached(arena, cache, *value_prem)?),
                    prem_iter: PremIterIl::decode_cached(arena, cache, *value_iter)?.0,
                })
            },
            m::PREM_DEBUG => |[value_exp]| {
                il::PremKind::Debug(il::DebugPrem { exp: il::Exp::decode_cached(arena, cache, *value_exp)? })
            },
            _ => return Err(Error::unexpected(arena, value_meta, "an IL premise")),
        };
        Ok(phrase!(node: prem_kind_il, span: span))
    }
}

/// An iteration premise as the meta `premIterIL`; AL encodes the same type as `premIterAL`.
struct PremIterIl(il::PremIter);

/// Decodes an iteration premise, which elaboration leaves without binders.
impl Decode for PremIterIl {
    fn decode_cached(
        arena: &mut Arena,
        cache: &mut Cache,
        value_meta: Value,
    ) -> Result<Self, Error> {
        get::matches_string! { arena, &value_meta,
            m::ITER_PREM => |[value_iter, value_bound]| {
                Ok(Self(il::PremIter {
                    iter: Iter::decode_cached(arena, cache, *value_iter)?,
                    vars_bound: Vec::<il::Var>::decode_cached(arena, cache, *value_bound)?,
                    vars_bind: vec![],
                }))
            },
            _ => Err(Error::unexpected(arena, value_meta, "iter vari*")),
        }
    }
}

// - Rules

/// Decodes a rule with positional relation notation.
impl Decode for il::Rule {
    fn decode_cached(
        arena: &mut Arena,
        cache: &mut Cache,
        value_meta: Value,
    ) -> Result<Self, Error> {
        let span = *arena.value.span(value_meta.span);
        get::matches_string! { arena, &value_meta,
            m::RUL => |[value_id, value_exps, value_prems]| {
                Ok(phrase!(node: il::RuleKind {
                    id: Id::decode_cached(arena, cache, *value_id)?,
                    not_exp: stub::notation(Vec::<il::Exp>::decode_cached(arena, cache, *value_exps)?)?,
                    prems: Vec::<il::Prem>::decode_cached(arena, cache, *value_prems)?,
                }, span: span))
            },
            _ => Err(Error::unexpected(arena, value_meta, "id ':' exp* '-' premIL*")),
        }
    }
}

/// Decodes a rule group with its rules in order.
impl Decode for il::RuleGroup {
    fn decode_cached(
        arena: &mut Arena,
        cache: &mut Cache,
        value_meta: Value,
    ) -> Result<Self, Error> {
        let span = *arena.value.span(value_meta.span);
        get::matches_string! { arena, &value_meta,
            m::RUL_GROUP => |[value_id, value_rules]| {
                Ok(phrase!(node: il::RuleGroupKind {
                    id: Id::decode_cached(arena, cache, *value_id)?,
                    rules: Vec::<il::Rule>::decode_cached(arena, cache, *value_rules)?,
                }, span: span))
            },
            _ => Err(Error::unexpected(arena, value_meta, "id ':' rulIL*")),
        }
    }
}

/// Decodes an otherwise group and restores its keyword annotation.
impl Decode for il::ElseGroup {
    fn decode_cached(
        arena: &mut Arena,
        cache: &mut Cache,
        value_meta: Value,
    ) -> Result<Self, Error> {
        let span = *arena.value.span(value_meta.span);
        get::matches_string! { arena, &value_meta,
            m::ELS_GROUP => |[value_id, value_rule]| {
                Ok(phrase!(node: il::ElseGroupKind {
                    id: Id::decode_cached(arena, cache, *value_id)?,
                    rule: il::Rule::decode_cached(arena, cache, *value_rule)?,
                    otherwise: stub::otherwise(span),
                }, span: span))
            },
            _ => Err(Error::unexpected(arena, value_meta, "id ':' rulIL")),
        }
    }
}

// - Clauses and table rows

/// Decodes a clause; the otherwise marker is restored by its slot.
fn decode_clause(
    arena: &mut Arena,
    cache: &mut Cache,
    value_meta: Value,
    otherwise: bool,
) -> Result<il::Clause, Error> {
    let span = *arena.value.span(value_meta.span);
    get::matches_string! { arena, &value_meta,
        m::CLAUSE => |[value_args, value_exp, value_prems]| {
            Ok(phrase!(node: il::ClauseKind {
                args: Vec::<il::Arg>::decode_cached(arena, cache, *value_args)?,
                exp: il::Exp::decode_cached(arena, cache, *value_exp)?,
                prems: Vec::<il::Prem>::decode_cached(arena, cache, *value_prems)?,
                otherwise_opt: otherwise.then(|| stub::otherwise(span)),
            }, span: span))
        },
        _ => Err(Error::unexpected(arena, value_meta, "arg* '=' exp '-' premIL*")),
    }
}

/// Decodes a table row.
impl Decode for il::TableRow {
    fn decode_cached(
        arena: &mut Arena,
        cache: &mut Cache,
        value_meta: Value,
    ) -> Result<Self, Error> {
        let span = *arena.value.span(value_meta.span);
        get::matches_string! { arena, &value_meta,
            m::TBL_ROW => |[value_args, value_exp]| {
                Ok(phrase!(node: il::TableRowKind {
                    args: Vec::<il::Arg>::decode_cached(arena, cache, *value_args)?,
                    exp: il::Exp::decode_cached(arena, cache, *value_exp)?,
                }, span: span))
            },
            _ => Err(Error::unexpected(arena, value_meta, "arg* '=' exp")),
        }
    }
}

// - Definitions

/// Decodes a definition with canonical notation and empty hints.
impl Decode for il::Def {
    fn decode_cached(
        arena: &mut Arena,
        cache: &mut Cache,
        value_meta: Value,
    ) -> Result<Self, Error> {
        let span = *arena.value.span(value_meta.span);
        let def_kind_il = get::matches_string! { arena, &value_meta,
            m::DEF_EXTTYP => |[value_id]| {
                il::DefKind::Typ(il::TypDef::Extern(il::ExternTyp {
                    id: Id::decode_cached(arena, cache, *value_id)?,
                    hints: vec![],
                }))
            },
            m::DEF_TYP => |[value_id, value_ids, value_typ]| {
                il::DefKind::Typ(il::TypDef::Defined(Box::new(il::DefinedTyp {
                    id: Id::decode_cached(arena, cache, *value_id)?,
                    tparams: Vec::<Id>::decode_cached(arena, cache, *value_ids)?,
                    def_typ: il::DefTyp::decode_cached(arena, cache, *value_typ)?,
                    hints: vec![],
                })))
            },
            m::DEF_EXTREL => |[value_id, value_typs, value_input]| {
                il::DefKind::Rel(il::RelDef::Extern(Box::new(il::ExternRel {
                    id: Id::decode_cached(arena, cache, *value_id)?,
                    not_typ: phrase!(
                        node: stub::notation(Vec::<il::Typ>::decode_cached(arena, cache, *value_typs)?)?,
                        span: span
                    ),
                    input_hint: InputHint::decode_cached(arena, cache, *value_input)?,
                    hints: vec![],
                })))
            },
            m::DEF_REL => |[value_id, value_typs, value_input, value_groups, value_else]| {
                il::DefKind::Rel(il::RelDef::Defined(Box::new(il::DefinedRel {
                    id: Id::decode_cached(arena, cache, *value_id)?,
                    not_typ: phrase!(
                        node: stub::notation(Vec::<il::Typ>::decode_cached(arena, cache, *value_typs)?)?,
                        span: span
                    ),
                    input_hint: InputHint::decode_cached(arena, cache, *value_input)?,
                    rule_groups: Vec::<il::RuleGroup>::decode_cached(arena, cache, *value_groups)?,
                    else_group: Option::<il::ElseGroup>::decode_cached(arena, cache, *value_else)?,
                    hints: vec![],
                })))
            },
            m::DEF_EXTFUNC => |[value_id, value_ids, value_params, value_typ]| {
                il::DefKind::MetaFunc(il::MetaFuncDef::Extern(il::ExternFunc {
                    id: Id::decode_cached(arena, cache, *value_id)?,
                    tparams: Vec::<Id>::decode_cached(arena, cache, *value_ids)?,
                    params: Vec::<il::Param>::decode_cached(arena, cache, *value_params)?,
                    typ: il::Typ::decode_cached(arena, cache, *value_typ)?,
                    hints: vec![],
                }))
            },
            m::DEF_BUILTINFUNC => |[value_id, value_ids, value_params, value_typ]| {
                il::DefKind::MetaFunc(il::MetaFuncDef::Builtin(il::BuiltinFunc {
                    id: Id::decode_cached(arena, cache, *value_id)?,
                    tparams: Vec::<Id>::decode_cached(arena, cache, *value_ids)?,
                    params: Vec::<il::Param>::decode_cached(arena, cache, *value_params)?,
                    typ: il::Typ::decode_cached(arena, cache, *value_typ)?,
                    hints: vec![],
                }))
            },
            m::DEF_TABLEFUNC => |[value_id, value_params, value_typ, value_rows]| {
                il::DefKind::MetaFunc(il::MetaFuncDef::Table(il::TableFunc {
                    id: Id::decode_cached(arena, cache, *value_id)?,
                    params: Vec::<il::Param>::decode_cached(arena, cache, *value_params)?,
                    typ: il::Typ::decode_cached(arena, cache, *value_typ)?,
                    rows: Vec::<il::TableRow>::decode_cached(arena, cache, *value_rows)?,
                    hints: vec![],
                }))
            },
            m::DEF_FUNC => |[value_id, value_ids, value_params, value_typ, value_clauses, value_else]| {
                let mut clauses = Vec::new();
                for value_clause in get::list(arena, value_clauses)?.to_vec() {
                    clauses.push(decode_clause(arena, cache, value_clause, false)?);
                }
                let else_clause = match get::opt(arena, value_else)? {
                    Some(value_clause) => Some(decode_clause(arena, cache, value_clause, true)?),
                    None => None,
                };
                il::DefKind::MetaFunc(il::MetaFuncDef::Defined(Box::new(il::DefinedFunc {
                    id: Id::decode_cached(arena, cache, *value_id)?,
                    tparams: Vec::<Id>::decode_cached(arena, cache, *value_ids)?,
                    params: Vec::<il::Param>::decode_cached(arena, cache, *value_params)?,
                    typ: il::Typ::decode_cached(arena, cache, *value_typ)?,
                    clauses,
                    else_clause,
                    hints: vec![],
                })))
            },
            _ => return Err(Error::unexpected(arena, value_meta, "an IL definition")),
        };
        Ok(phrase!(node: def_kind_il, span: span))
    }
}
