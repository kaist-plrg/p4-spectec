//! Encoding of AL premises, definitions, and complete scripts
//!
//! Relation arguments split by their input hint and keep source order within
//! each part. Var definitions, general hints, and printing-only signatures
//! are omitted because the meta syntax has no corresponding fields.

use super::super::{mixops as m, typs as meta_typs};
use super::exp::args;
use super::{case, deftyp, exp, exps, id, iter, list, opt, typ, typs, var};
use crate::{
    lang::{
        al::ast,
        common::source::Span,
        data::{
            arena::Arena,
            value::flat::{Value, make},
        },
        hints::input,
    },
    metacirc::Error,
};

/// Encodes a whole source AL script, omitting only variable definitions.
pub fn spec(arena: &mut Arena, spec_al: &ast::Spec) -> Result<Value, Error> {
    // Preserve definition order while filtering the explicitly omitted form
    let mut values = Vec::with_capacity(spec_al.len());
    for def_al in spec_al {
        if let Some(value) = def(arena, def_al)? {
            values.push(value);
        }
    }
    Ok(make::list(arena, meta_typs::named("script"), values, Span::default())?)
}

/// Encodes a definition; only a Var definition returns None.
pub fn def(arena: &mut Arena, def_al: &ast::Def) -> Result<Option<Value>, Error> {
    use ast::{DefKind as D, MetaFuncDef as F, RelDef as R, TypDef as T};
    let (shape, values) = match &def_al.node {
        D::Var(_) => return Ok(None),
        D::Typ(T::Extern(typ_def)) => (m::EXTTYP, vec![id(arena, &typ_def.id)?]),
        D::Typ(T::Defined(typ_def)) => (
            m::TYP_DEF,
            vec![
                id(arena, &typ_def.id)?,
                list(arena, "id", &typ_def.tparams, id)?,
                deftyp(arena, &typ_def.def_typ)?,
            ],
        ),
        D::Rel(R::Extern(rel_def)) => {
            let (typs_input, typs_output) =
                input::split(&rel_def.input_hint, rel_def.not_typ.node.args().to_vec())?;
            (
                m::EXTREL,
                vec![
                    id(arena, &rel_def.id)?,
                    typs(arena, &typs_input)?,
                    typs(arena, &typs_output)?,
                ],
            )
        }
        D::Rel(R::Defined(rel_def)) => {
            let (typs_input, typs_output) =
                input::split(&rel_def.input_hint, rel_def.not_typ.node.args().to_vec())?;
            (
                m::REL_DEF,
                vec![
                    id(arena, &rel_def.id)?,
                    typs(arena, &typs_input)?,
                    typs(arena, &typs_output)?,
                    list(arena, "rulgroup", &rel_def.rule_groups, rulegroup)?,
                    opt(arena, "elsgroup", rel_def.else_group.as_ref(), elsegroup)?,
                ],
            )
        }
        D::MetaFunc(F::Extern(func)) => (
            m::EXTFUNC,
            vec![
                id(arena, &func.id)?,
                list(arena, "id", &func.tparams, id)?,
                list(arena, "param", &func.params, param)?,
                typ(arena, &func.typ)?,
            ],
        ),
        D::MetaFunc(F::Builtin(func)) => (
            m::BUILTINFUNC,
            vec![
                id(arena, &func.id)?,
                list(arena, "id", &func.tparams, id)?,
                list(arena, "param", &func.params, param)?,
                typ(arena, &func.typ)?,
            ],
        ),
        D::MetaFunc(F::Table(func)) => (
            m::TABLEFUNC,
            vec![
                id(arena, &func.id)?,
                list(arena, "param", &func.params, param)?,
                typ(arena, &func.typ)?,
                list(arena, "tblrow", &func.table_rows, tablerow)?,
            ],
        ),
        D::MetaFunc(F::Defined(func)) => (
            m::FUNC_DEF,
            vec![
                id(arena, &func.id)?,
                list(arena, "id", &func.tparams, id)?,
                list(arena, "param", &func.params, param)?,
                typ(arena, &func.typ)?,
                list(arena, "clause", &func.clauses, clause)?,
                opt(arena, "elsclause", func.else_clause.as_ref(), clause)?,
            ],
        ),
    };
    Ok(Some(case(arena, shape, "defn", values, def_al.span)?))
}

fn param(arena: &mut Arena, param_al: &ast::Param) -> Result<Value, Error> {
    let (shape, values) = match &param_al.node {
        ast::ParamKind::Exp(typ_al) => (m::EXP_PARAM, vec![typ(arena, typ_al)?]),
        ast::ParamKind::Def(id_al, tparams, params, typ_al) => (
            m::FUN_PARAM,
            vec![
                id(arena, id_al)?,
                list(arena, "id", tparams, id)?,
                list(arena, "param", params, param)?,
                typ(arena, typ_al)?,
            ],
        ),
    };
    case(arena, shape, "param", values, param_al.span)
}

fn prem(arena: &mut Arena, prem_al: &ast::Prem) -> Result<Value, Error> {
    use ast::PremKind as P;
    let (shape, values) = match &prem_al.node {
        P::Rule(prem) => {
            let (exps_input, exps_output) =
                input::split(&prem.input_hint, prem.not_exp.args().to_vec())?;
            (
                m::REL_PREM,
                vec![id(arena, &prem.id)?, exps(arena, &exps_input)?, exps(arena, &exps_output)?],
            )
        }
        P::If(prem) => (m::IF, vec![exp(arena, &prem.exp)?]),
        P::IfHold(prem) => {
            (m::IFHOLD, vec![id(arena, &prem.id)?, exps(arena, prem.not_exp.args())?])
        }
        P::IfNotHold(prem) => {
            (m::IFNOTHOLD, vec![id(arena, &prem.id)?, exps(arena, prem.not_exp.args())?])
        }
        P::Let(prem) => (m::LET, vec![exp(arena, &prem.exp_l)?, exp(arena, &prem.exp_r)?]),
        P::Iter(prem_iter) => (
            m::ITER_PREM,
            vec![prem(arena, &prem_iter.prem)?, iterprem(arena, &prem_iter.prem_iter)?],
        ),
        P::Debug(prem) => (m::DEBUG, vec![exp(arena, &prem.exp)?]),
    };
    case(arena, shape, "prem", values, prem_al.span)
}

fn iterprem(arena: &mut Arena, prem_iter: &ast::PremIter) -> Result<Value, Error> {
    let values = vec![
        iter(arena, &prem_iter.iter)?,
        list(arena, "vari", &prem_iter.vars_bound, var)?,
        list(arena, "vari", &prem_iter.vars_bind, var)?,
    ];
    case(arena, m::ITERPREM, "iterprem", values, Span::default())
}

fn rulematch(arena: &mut Arena, rule_match: &ast::RuleMatch) -> Result<Value, Error> {
    let values =
        vec![exps(arena, &rule_match.exps_input)?, list(arena, "prem", &rule_match.prems, prem)?];
    case(arena, m::RULMATCH, "rulmatch", values, Span::default())
}

fn rulepath(arena: &mut Arena, rule_path: &ast::RulePath) -> Result<Value, Error> {
    let values = vec![
        id(arena, &rule_path.id)?,
        exps(arena, &rule_path.exps_output)?,
        list(arena, "prem", &rule_path.prems, prem)?,
    ];
    case(arena, m::RULPATH, "rulpath", values, rule_path.id.span)
}

fn rulegroup(arena: &mut Arena, rule_group: &ast::RuleGroup) -> Result<Value, Error> {
    let values = vec![
        id(arena, &rule_group.node.id)?,
        rulematch(arena, &rule_group.node.rule_match)?,
        list(arena, "rulpath", &rule_group.node.rule_paths, rulepath)?,
    ];
    case(arena, m::RULGROUP, "rulgroup", values, rule_group.span)
}

fn elsegroup(arena: &mut Arena, group_else: &ast::ElseGroup) -> Result<Value, Error> {
    let values = vec![
        id(arena, &group_else.node.id)?,
        rulematch(arena, &group_else.node.rule_match)?,
        rulepath(arena, &group_else.node.rule_path)?,
    ];
    case(arena, m::ELSGROUP, "elsgroup", values, group_else.span)
}

fn clause(arena: &mut Arena, clause_al: &ast::Clause) -> Result<Value, Error> {
    let values = vec![
        args(arena, &clause_al.node.args)?,
        exp(arena, &clause_al.node.exp)?,
        list(arena, "prem", &clause_al.node.prems, prem)?,
    ];
    case(arena, m::CLAUSE, "clause", values, clause_al.span)
}

fn tablerow(arena: &mut Arena, row_al: &ast::TableRow) -> Result<Value, Error> {
    let values = vec![
        args(arena, &row_al.node.args)?,
        exp(arena, &row_al.node.exp)?,
        list(arena, "prem", &row_al.node.prems, prem)?,
    ];
    case(arena, m::CLAUSE, "tblrow", values, row_al.span)
}
