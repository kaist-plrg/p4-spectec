//! Decodes AL syntax and runtime values from meta data
//!
//! Shared syntax is decoded by `meta::shared::decode`.
//! Runtime payload decoding allocates only in the supplied arena.

use super::{mixops as m, stub};
use crate::interface::meta::shared::{Cache, Error, decode::Decode};
use crate::lang::common::prim::num::Number;
use crate::{
    lang::{
        al::ast as al,
        common::{Id, Iter, prim::num},
        data::{
            arena::Arena,
            notation::flat::{Mixfix as FlatMixfix, Mixop as FlatMixop},
            typ::make as make_typ,
            value::flat::{Value, ValueField, get, make},
        },
        il::ast as il,
        traits::eq::SyntaxEq,
    },
    phrase,
};

// == Decoding

// - Mixfixes

/// Decodes a runtime value notation case.
impl Decode for FlatMixfix<Value> {
    fn decode_cached(
        arena: &mut Arena,
        cache: &mut Cache,
        value_meta: Value,
    ) -> Result<Self, Error> {
        get::matches_string! { arena, &value_meta,
            m::VALCASE => |[value_mixop, values_arg]| {
                let mixop = FlatMixop::decode_cached(arena, cache, *value_mixop)?;
                let values = Vec::<Value>::decode_cached(arena, cache, *values_arg)?;
                Ok(FlatMixfix::new(&arena.mixop, mixop, values)?)
            },
            _ => Err(Error::unexpected(arena, value_meta, "mixop elem*")),
        }
    }
}

// - Values

/// Decodes a runtime payload, allocating only in the supplied arena.
impl Decode for Value {
    fn decode_cached(
        arena: &mut Arena,
        cache: &mut Cache,
        value_meta: Value,
    ) -> Result<Self, Error> {
        // Reuse the decoded payload before visiting its children
        let id_value = arena.value.canon_id(value_meta.node);
        if let Some(value_target) = cache.values_decode.get(&id_value) {
            return Ok(*value_target);
        }
        let span = *arena.value.span(value_meta.span);
        let value_target = get::matches_string! { arena, &value_meta,
            m::BOOL => |[value_bool]| {
                let value_bool = get::bool(arena, value_bool)?;
                make::bool(arena, value_bool, span)?
            },
            m::NAT => |[value_num]| {
                let Number::Nat(nat) = get::num(arena, value_num)? else {
                    return Err(Error::unexpected(arena, *value_num, "a natural-number payload"));
                };
                let nat_target = nat.clone();
                make::nat(arena, nat_target, span)?
            },
            m::INT => |[value_num]| {
                let num_target = get::num(arena, value_num)?.clone();
                make::int(arena, num::to_int(&num_target).clone(), span)?
            },
            m::TEXT => |[value_text]| {
                let text_target = get::text(arena, value_text)?.to_owned();
                make::text(arena, text_target, span)?
            },
            m::STR => |[value_fields]| {
                let fields_target = Vec::<ValueField>::decode_cached(arena, cache, *value_fields)?;
                make::structure(arena, stub::value_typ().node.into(), fields_target, span)?
            },
            m::INJ => |[value_case]| {
                let mixfix_target = FlatMixfix::<Value>::decode_cached(arena, cache, *value_case)?;
                make::case_flat(arena, stub::value_typ().node.into(), mixfix_target, span)?
            },
            m::TUP => |[value_values]| {
                let values_target = Vec::<Value>::decode_cached(arena, cache, *value_values)?;
                make::tuple(arena, stub::value_typ().node.into(), values_target, span)?
            },
            m::OPT => |[value_opt]| {
                let value_target = Option::<Value>::decode_cached(arena, cache, *value_opt)?;
                make::opt(arena, make_typ::opt(stub::value_typ()).node.into(), value_target, span)?
            },
            m::LIST => |[value_values]| {
                let values_target = Vec::<Value>::decode_cached(arena, cache, *value_values)?;
                make::list(
                    arena,
                    make_typ::list(stub::value_typ()).node.into(),
                    values_target,
                    span,
                )?
            },
            m::FUNC => |[value_id]| {
                let id_target = Id::decode_cached(arena, cache, *value_id)?;
                let (tparams_target, typs_target, typ_target) = stub::func_typ_params();
                make::func(arena, id_target, tparams_target, typs_target, typ_target, span)?
            },
            m::EXT => |[value_json]| {
                let json_target = get::external(arena, value_json)?.clone();
                make::external(arena, stub::json_typ().node.into(), json_target, span)?
            },
            _ => {
                            return Err(Error::unexpected(
                                arena,
                                value_meta,
                                "an AL value constructor with its declared arguments",
                            ));
                        },
        };
        // Retain successful conversions without seeding the encode cache
        cache.values_decode.insert(id_value, value_target);
        Ok(value_target)
    }
}

/// Decodes one runtime struct field.
impl Decode for ValueField {
    fn decode_cached(
        arena: &mut Arena,
        cache: &mut Cache,
        value_meta: Value,
    ) -> Result<Self, Error> {
        get::matches_string! { arena, &value_meta,
            m::VALFIELD => |[value_atom, value_inner]| {
                Ok(ValueField {
                    atom: il::Atom::decode_cached(arena, cache, *value_atom)?,
                    value: Value::decode_cached(arena, cache, *value_inner)?,
                })
            },
            _ => Err(Error::unexpected(arena, value_meta, "atom val")),
        }
    }
}

// - Premises

/// Decodes an AL premise.
impl Decode for al::Prem {
    fn decode_cached(
        arena: &mut Arena,
        cache: &mut Cache,
        value_meta: Value,
    ) -> Result<Self, Error> {
        let span = *arena.value.span(value_meta.span);
        let prem_kind_al = get::matches_string! { arena, &value_meta,
            m::REL_PREM => |[value_id, value_input, value_output]| {
                let (not_exp, input_hint) = stub::not_exp(
                    Vec::<il::Exp>::decode_cached(arena, cache, *value_input)?,
                    Vec::<il::Exp>::decode_cached(arena, cache, *value_output)?,
                )?;
                al::PremKind::Rule(al::RulePrem {
                    id: Id::decode_cached(arena, cache, *value_id)?,
                    not_exp,
                    input_hint,
                })
            },
            m::IF => |[value_exp]| {
                al::PremKind::If(al::IfPrem { exp: il::Exp::decode_cached(arena, cache, *value_exp)? })
            },
            m::IFHOLD => |[value_id, value_exps]| {
                let (not_exp, _) =
                    stub::not_exp(Vec::<il::Exp>::decode_cached(arena, cache, *value_exps)?, vec![])?;
                al::PremKind::IfHold(al::IfHoldPrem { id: Id::decode_cached(arena, cache, *value_id)?, not_exp })
            },
            m::IFNOTHOLD => |[value_id, value_exps]| {
                let (not_exp, _) =
                    stub::not_exp(Vec::<il::Exp>::decode_cached(arena, cache, *value_exps)?, vec![])?;
                al::PremKind::IfNotHold(al::IfNotHoldPrem {
                    id: Id::decode_cached(arena, cache, *value_id)?,
                    not_exp,
                })
            },
            m::LET => |[value_l, value_r]| {
                al::PremKind::Let(al::LetPrem {
                    exp_l: il::Exp::decode_cached(arena, cache, *value_l)?,
                    exp_r: il::Exp::decode_cached(arena, cache, *value_r)?,
                })
            },
            m::ITER_PREM => |[value_prem, value_iter]| {
                al::PremKind::Iter(al::IterPrem {
                    prem: Box::new(al::Prem::decode_cached(arena, cache, *value_prem)?),
                    prem_iter: al::PremIter::decode_cached(arena, cache, *value_iter)?,
                })
            },
            m::DEBUG => |[value_exp]| {
                al::PremKind::Debug(al::DebugPrem { exp: il::Exp::decode_cached(arena, cache, *value_exp)? })
            },
            _ => return Err(Error::unexpected(arena, value_meta, "an AL premise")),
        };
        Ok(phrase!(node: prem_kind_al, span: span))
    }
}

// - Iteration premises

/// Decodes bound and bound-by-iteration variable lists in order.
impl Decode for al::PremIter {
    fn decode_cached(
        arena: &mut Arena,
        cache: &mut Cache,
        value_meta: Value,
    ) -> Result<Self, Error> {
        get::matches_string! { arena, &value_meta,
            m::ITERPREM => |[value_iter, value_bound, value_bind]| {
                Ok(al::PremIter {
                    iter: Iter::decode_cached(arena, cache, *value_iter)?,
                    vars_bound: Vec::<al::Var>::decode_cached(arena, cache, *value_bound)?,
                    vars_bind: Vec::<al::Var>::decode_cached(arena, cache, *value_bind)?,
                })
            },
            _ => Err(Error::unexpected(arena, value_meta, "iter '<-' vari* '->' vari*")),
        }
    }
}

// - Rules

/// Decodes a rule match without its printing signature.
impl Decode for al::RuleMatch {
    fn decode_cached(
        arena: &mut Arena,
        cache: &mut Cache,
        value_meta: Value,
    ) -> Result<Self, Error> {
        get::matches_string! { arena, &value_meta,
            m::RULMATCH => |[value_exps, value_prems]| {
                let exps_input = Vec::<il::Exp>::decode_cached(arena, cache, *value_exps)?;
                Ok(al::RuleMatch {
                    exps_signature: stub::rule_match_signature(&exps_input),
                    exps_input,
                    prems: Vec::<al::Prem>::decode_cached(arena, cache, *value_prems)?,
                })
            },
            _ => Err(Error::unexpected(arena, value_meta, "exp* '-' prem*")),
        }
    }
}

/// Decodes a rule path and its outputs.
impl Decode for al::RulePath {
    fn decode_cached(
        arena: &mut Arena,
        cache: &mut Cache,
        value_meta: Value,
    ) -> Result<Self, Error> {
        get::matches_string! { arena, &value_meta,
            m::RULPATH => |[value_id, value_exps, value_prems]| {
                Ok(al::RulePath {
                    id: Id::decode_cached(arena, cache, *value_id)?,
                    exps_output: Vec::<il::Exp>::decode_cached(arena, cache, *value_exps)?,
                    prems: Vec::<al::Prem>::decode_cached(arena, cache, *value_prems)?,
                })
            },
            _ => Err(Error::unexpected(arena, value_meta, "id '=' exp* '-' prem*")),
        }
    }
}

/// Decodes one rule as its match followed by its path.
impl Decode for (al::RuleMatch, al::RulePath) {
    fn decode_cached(
        arena: &mut Arena,
        cache: &mut Cache,
        value_meta: Value,
    ) -> Result<Self, Error> {
        get::matches_string! { arena, &value_meta,
            m::RUL => |[value_match, value_path]| {
                Ok((
                    al::RuleMatch::decode_cached(arena, cache, *value_match)?,
                    al::RulePath::decode_cached(arena, cache, *value_path)?,
                ))
            },
            _ => Err(Error::unexpected(arena, value_meta, "rulMatch '->' rulPath")),
        }
    }
}

/// Decodes rule groups, sharing a native match among consecutive equal matches.
///
/// Encoding copies a native group's shared match into each of its rules;
/// decoding merges such runs back into one group.
/// Rules with different matches become separate groups with the same identifier,
/// which the native interpreter evaluates as the same (group, path) candidates.
fn decode_rule_groups(
    arena: &mut Arena,
    cache: &mut Cache,
    value_meta: Value,
) -> Result<Vec<al::RuleGroup>, Error> {
    let mut rule_groups: Vec<al::RuleGroup> = Vec::new();
    for value_group in get::list(arena, &value_meta)?.to_vec() {
        let span = *arena.value.span(value_group.span);
        let (id, rules) = get::matches_string! { arena, &value_group,
            m::RULGROUP => |[value_id, value_rules]| (
                Id::decode_cached(arena, cache, *value_id)?,
                Vec::<(al::RuleMatch, al::RulePath)>::decode_cached(arena, cache, *value_rules)?,
            ),
            _ => return Err(Error::unexpected(arena, value_group, "id ':' rul*")),
        };
        let idx_first = rule_groups.len();
        for (rule_match, rule_path) in rules {
            // Extend the previous group of this meta group when the match repeats
            match rule_groups[idx_first..].last_mut() {
                Some(rule_group) if rule_group.node.rule_match.syntax_eq(&rule_match) => {
                    rule_group.node.rule_paths.push(rule_path);
                }
                _ => rule_groups.push(phrase!(node: al::RuleGroupKind {
                    id: id.clone(),
                    rule_match,
                    rule_paths: vec![rule_path],
                }, span: span)),
            }
        }
    }
    Ok(rule_groups)
}

/// Decodes an otherwise group with one rule.
impl Decode for al::ElseGroup {
    fn decode_cached(
        arena: &mut Arena,
        cache: &mut Cache,
        value_meta: Value,
    ) -> Result<Self, Error> {
        let span = *arena.value.span(value_meta.span);
        get::matches_string! { arena, &value_meta,
            m::ELSGROUP => |[value_id, value_rule]| {
                let (rule_match, rule_path) =
                    <(al::RuleMatch, al::RulePath)>::decode_cached(arena, cache, *value_rule)?;
                Ok(phrase!(node: al::ElseGroupKind {
                    otherwise: phrase!(node: il::OtherwiseKind, span: span),
                    id: Id::decode_cached(arena, cache, *value_id)?,
                    rule_match,
                    rule_path,
                }, span: span))
            },
            _ => Err(Error::unexpected(arena, value_meta, "id ':' rul")),
        }
    }
}

// - Clauses

/// Decodes a function clause.
impl Decode for al::Clause {
    fn decode_cached(
        arena: &mut Arena,
        cache: &mut Cache,
        value_meta: Value,
    ) -> Result<Self, Error> {
        let span = *arena.value.span(value_meta.span);
        get::matches_string! { arena, &value_meta,
            m::CLAUSE => |[value_args, value_exp, value_prems]| {
                Ok(phrase!(node: al::ClauseKind {
                    args: Vec::<il::Arg>::decode_cached(arena, cache, *value_args)?,
                    exp: il::Exp::decode_cached(arena, cache, *value_exp)?,
                    prems: Vec::<al::Prem>::decode_cached(arena, cache, *value_prems)?,
                }, span: span))
            },
            _ => Err(Error::unexpected(arena, value_meta, "arg* '=' exp '-' prem*")),
        }
    }
}

/// Decodes an otherwise clause, using its span for the omitted keyword.
impl Decode for al::ElseClause {
    fn decode_cached(
        arena: &mut Arena,
        cache: &mut Cache,
        value_meta: Value,
    ) -> Result<Self, Error> {
        let clause_al = al::Clause::decode_cached(arena, cache, value_meta)?;
        Ok(phrase!(node: al::ElseClauseKind {
            otherwise: phrase!(node: il::OtherwiseKind, span: clause_al.span),
            clause: clause_al.node,
        }, span: clause_al.span))
    }
}

// - Table rows

/// Decodes a table row without its printing signature.
impl Decode for al::TableRow {
    fn decode_cached(
        arena: &mut Arena,
        cache: &mut Cache,
        value_meta: Value,
    ) -> Result<Self, Error> {
        let clause_al = al::Clause::decode_cached(arena, cache, value_meta)?;
        let al::ClauseKind { args: args_al, exp: exp_al, prems: prems_al } = clause_al.node;
        Ok(phrase!(node: al::TableRowKind {
            exps_signature: stub::table_row_signature(),
            args: args_al,
            exp: exp_al,
            prems: prems_al,
        }, span: clause_al.span))
    }
}

// - Definitions

/// Decodes one definition without inventing omitted general hints.
impl Decode for al::Def {
    fn decode_cached(
        arena: &mut Arena,
        cache: &mut Cache,
        value_meta: Value,
    ) -> Result<Self, Error> {
        let span = *arena.value.span(value_meta.span);
        let def_kind_al = get::matches_string! { arena, &value_meta,
            m::EXTTYP => |[value_id]| {
                al::DefKind::Typ(al::TypDef::Extern(al::ExternTyp {
                    id: Id::decode_cached(arena, cache, *value_id)?,
                    hints: stub::hints(),
                }))
            },
            m::TYP_DEF => |[value_id, value_ids, value_typ]| {
                al::DefKind::Typ(al::TypDef::Defined(Box::new(al::DefinedTyp {
                    id: Id::decode_cached(arena, cache, *value_id)?,
                    tparams: Vec::<Id>::decode_cached(arena, cache, *value_ids)?,
                    def_typ: il::DefTyp::decode_cached(arena, cache, *value_typ)?,
                    hints: stub::hints(),
                })))
            },
            m::EXTREL => |[value_id, value_input, value_output]| {
                let (not_typ, input_hint) = stub::not_typ(
                    Vec::<il::Typ>::decode_cached(arena, cache, *value_input)?,
                    Vec::<il::Typ>::decode_cached(arena, cache, *value_output)?,
                )?;
                al::DefKind::Rel(al::RelDef::Extern(Box::new(al::ExternRel {
                    id: Id::decode_cached(arena, cache, *value_id)?,
                    not_typ,
                    input_hint,
                    hints: stub::hints(),
                })))
            },
            m::REL_DEF => |[value_id, value_input, value_output, value_groups, value_else]| {
                let (not_typ, input_hint) = stub::not_typ(
                    Vec::<il::Typ>::decode_cached(arena, cache, *value_input)?,
                    Vec::<il::Typ>::decode_cached(arena, cache, *value_output)?,
                )?;
                al::DefKind::Rel(al::RelDef::Defined(Box::new(al::DefinedRel {
                    id: Id::decode_cached(arena, cache, *value_id)?,
                    not_typ,
                    input_hint,
                    rule_groups: decode_rule_groups(arena, cache, *value_groups)?,
                    else_group: Option::<al::ElseGroup>::decode_cached(arena, cache, *value_else)?,
                    hints: stub::hints(),
                })))
            },
            m::EXTFUNC => |[value_id, value_ids, value_params, value_typ]| {
                al::DefKind::MetaFunc(al::MetaFuncDef::Extern(al::ExternFunc {
                    id: Id::decode_cached(arena, cache, *value_id)?,
                    tparams: Vec::<Id>::decode_cached(arena, cache, *value_ids)?,
                    params: Vec::<al::Param>::decode_cached(arena, cache, *value_params)?,
                    typ: il::Typ::decode_cached(arena, cache, *value_typ)?,
                    hints: stub::hints(),
                }))
            },
            m::BUILTINFUNC => |[value_id, value_ids, value_params, value_typ]| {
                al::DefKind::MetaFunc(al::MetaFuncDef::Builtin(al::BuiltinFunc {
                    id: Id::decode_cached(arena, cache, *value_id)?,
                    tparams: Vec::<Id>::decode_cached(arena, cache, *value_ids)?,
                    params: Vec::<al::Param>::decode_cached(arena, cache, *value_params)?,
                    typ: il::Typ::decode_cached(arena, cache, *value_typ)?,
                    hints: stub::hints(),
                }))
            },
            m::TABLEFUNC => |[value_id, value_params, value_typ, value_rows]| {
                al::DefKind::MetaFunc(al::MetaFuncDef::Table(al::TableFunc {
                    id: Id::decode_cached(arena, cache, *value_id)?,
                    params: Vec::<al::Param>::decode_cached(arena, cache, *value_params)?,
                    typ: il::Typ::decode_cached(arena, cache, *value_typ)?,
                    table_rows: Vec::<al::TableRow>::decode_cached(arena, cache, *value_rows)?,
                    hints: stub::hints(),
                }))
            },
            m::FUNC_DEF => |[value_id, value_ids, value_params, value_typ, value_clauses, value_else]| {
                al::DefKind::MetaFunc(al::MetaFuncDef::Defined(Box::new(al::DefinedFunc {
                    id: Id::decode_cached(arena, cache, *value_id)?,
                    tparams: Vec::<Id>::decode_cached(arena, cache, *value_ids)?,
                    params: Vec::<al::Param>::decode_cached(arena, cache, *value_params)?,
                    typ: il::Typ::decode_cached(arena, cache, *value_typ)?,
                    clauses: Vec::<al::Clause>::decode_cached(arena, cache, *value_clauses)?,
                    else_clause: Option::<al::ElseClause>::decode_cached(arena, cache, *value_else)?,
                    hints: stub::hints(),
                })))
            },
            _ => {
                            return Err(Error::unexpected(
                                arena,
                                value_meta,
                                "an AL definition constructor with its declared arguments",
                            ));
                        },
        };
        Ok(phrase!(node: def_kind_al, span: span))
    }
}
