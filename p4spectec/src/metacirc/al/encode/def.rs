//! Encoding of AL premises, definitions, and complete scripts
//!
//! `def` and `spec` split relation arguments according to their input hint,
//! preserving source order within each part
//! Variable definitions, general hints, and printing signatures are omitted
//! because the meta syntax has no corresponding fields

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
    let mut values_meta = Vec::with_capacity(spec_al.len());
    for def_al in spec_al {
        if let Some(value) = def(arena, def_al)? {
            values_meta.push(value);
        }
    }
    Ok(make::list(arena, meta_typs::named("script"), values_meta, Span::default())?)
}

/// Encodes a definition; only a Var definition returns None.
pub fn def(arena: &mut Arena, def_al: &ast::Def) -> Result<Option<Value>, Error> {
    let span = def_al.span;
    let value_meta = match &def_al.node {
        ast::DefKind::Var(_) => return Ok(None),
        ast::DefKind::Typ(ast::TypDef::Extern(typdef_al)) => {
            encode_extern_typ_def(arena, typdef_al, span)?
        }
        ast::DefKind::Typ(ast::TypDef::Defined(typdef_al)) => {
            encode_defined_typ_def(arena, typdef_al, span)?
        }
        ast::DefKind::Rel(ast::RelDef::Extern(rel_al)) => {
            encode_extern_rel_def(arena, rel_al, span)?
        }
        ast::DefKind::Rel(ast::RelDef::Defined(rel_al)) => {
            encode_defined_rel_def(arena, rel_al, span)?
        }
        ast::DefKind::MetaFunc(ast::MetaFuncDef::Extern(func_al)) => {
            encode_extern_func_def(arena, func_al, span)?
        }
        ast::DefKind::MetaFunc(ast::MetaFuncDef::Builtin(func_al)) => {
            encode_builtin_func_def(arena, func_al, span)?
        }
        ast::DefKind::MetaFunc(ast::MetaFuncDef::Table(func_al)) => {
            encode_table_func_def(arena, func_al, span)?
        }
        ast::DefKind::MetaFunc(ast::MetaFuncDef::Defined(func_al)) => {
            encode_defined_func_def(arena, func_al, span)?
        }
    };
    Ok(Some(value_meta))
}

/// Encodes an external type declaration.
fn encode_extern_typ_def(
    arena: &mut Arena,
    typdef_al: &ast::ExternTyp,
    span: Span,
) -> Result<Value, Error> {
    let (shape, values_meta) = (m::EXTTYP, vec![id(arena, &typdef_al.id)?]);
    case(arena, shape, "defn", values_meta, span)
}

/// Encodes a type definition with its parameters.
fn encode_defined_typ_def(
    arena: &mut Arena,
    typdef_al: &ast::DefinedTyp,
    span: Span,
) -> Result<Value, Error> {
    let (shape, values_meta) = (
        m::TYP_DEF,
        vec![
            id(arena, &typdef_al.id)?,
            list(arena, "id", &typdef_al.tparams, id)?,
            deftyp(arena, &typdef_al.def_typ)?,
        ],
    );
    case(arena, shape, "defn", values_meta, span)
}

/// Encodes an external relation signature.
fn encode_extern_rel_def(
    arena: &mut Arena,
    rel_al: &ast::ExternRel,
    span: Span,
) -> Result<Value, Error> {
    let (shape, values_meta) = {
        // Split source arguments according to the operational input hint
        let (typs_input_al, typs_output_al) =
            input::split(&rel_al.input_hint, rel_al.not_typ.node.args().to_vec())?;
        // Encode input types before output types in the meta signature
        (
            m::EXTREL,
            vec![
                id(arena, &rel_al.id)?,
                typs(arena, &typs_input_al)?,
                typs(arena, &typs_output_al)?,
            ],
        )
    };
    // Retain the definition span on the enclosing meta constructor
    case(arena, shape, "defn", values_meta, span)
}

/// Encodes a relation signature and its rule groups.
fn encode_defined_rel_def(
    arena: &mut Arena,
    rel_al: &ast::DefinedRel,
    span: Span,
) -> Result<Value, Error> {
    let (shape, values_meta) = {
        // Split source arguments according to the operational input hint
        let (typs_input_al, typs_output_al) =
            input::split(&rel_al.input_hint, rel_al.not_typ.node.args().to_vec())?;
        // Encode the canonical signature before the rule groups
        (
            m::REL_DEF,
            vec![
                id(arena, &rel_al.id)?,
                typs(arena, &typs_input_al)?,
                typs(arena, &typs_output_al)?,
                list(arena, "rulgroup", &rel_al.rule_groups, rulegroup)?,
                opt(arena, "elsgroup", rel_al.else_group.as_ref(), elsegroup)?,
            ],
        )
    };
    // Retain the definition span on the enclosing meta constructor
    case(arena, shape, "defn", values_meta, span)
}

/// Encodes an external function signature.
fn encode_extern_func_def(
    arena: &mut Arena,
    func_al: &ast::ExternFunc,
    span: Span,
) -> Result<Value, Error> {
    let (shape, values_meta) = (
        m::EXTFUNC,
        vec![
            id(arena, &func_al.id)?,
            list(arena, "id", &func_al.tparams, id)?,
            list(arena, "param", &func_al.params, param)?,
            typ(arena, &func_al.typ)?,
        ],
    );
    case(arena, shape, "defn", values_meta, span)
}

/// Encodes a builtin function signature.
fn encode_builtin_func_def(
    arena: &mut Arena,
    func_al: &ast::BuiltinFunc,
    span: Span,
) -> Result<Value, Error> {
    let (shape, values_meta) = (
        m::BUILTINFUNC,
        vec![
            id(arena, &func_al.id)?,
            list(arena, "id", &func_al.tparams, id)?,
            list(arena, "param", &func_al.params, param)?,
            typ(arena, &func_al.typ)?,
        ],
    );
    case(arena, shape, "defn", values_meta, span)
}

/// Encodes a table function and its rows.
fn encode_table_func_def(
    arena: &mut Arena,
    func_al: &ast::TableFunc,
    span: Span,
) -> Result<Value, Error> {
    let (shape, values_meta) = (
        m::TABLEFUNC,
        vec![
            id(arena, &func_al.id)?,
            list(arena, "param", &func_al.params, param)?,
            typ(arena, &func_al.typ)?,
            list(arena, "tblrow", &func_al.table_rows, tablerow)?,
        ],
    );
    case(arena, shape, "defn", values_meta, span)
}

/// Encodes a function signature and its clauses.
fn encode_defined_func_def(
    arena: &mut Arena,
    func_al: &ast::DefinedFunc,
    span: Span,
) -> Result<Value, Error> {
    let (shape, values_meta) = (
        m::FUNC_DEF,
        vec![
            id(arena, &func_al.id)?,
            list(arena, "id", &func_al.tparams, id)?,
            list(arena, "param", &func_al.params, param)?,
            typ(arena, &func_al.typ)?,
            list(arena, "clause", &func_al.clauses, clause)?,
            opt(arena, "elsclause", func_al.else_clause.as_ref(), clause)?,
        ],
    );
    case(arena, shape, "defn", values_meta, span)
}

/// Encodes a value or higher-order function parameter.
fn param(arena: &mut Arena, param_al: &ast::Param) -> Result<Value, Error> {
    match &param_al.node {
        ast::ParamKind::Exp(typ_al) => encode_exp_param(arena, typ_al, param_al.span),
        ast::ParamKind::Def(id_al, tparams_al, params_al, typ_al) => {
            encode_func_param(arena, id_al, tparams_al, params_al, typ_al, param_al.span)
        }
    }
}

fn encode_exp_param(arena: &mut Arena, typ_al: &ast::Typ, span: Span) -> Result<Value, Error> {
    let values_meta = vec![typ(arena, typ_al)?];
    case(arena, m::EXP_PARAM, "param", values_meta, span)
}

/// Encodes a higher-order parameter and its complete function signature.
fn encode_func_param(
    arena: &mut Arena,
    id_al: &ast::Id,
    tparams_al: &[ast::Id],
    params_al: &[ast::Param],
    typ_al: &ast::Typ,
    span: Span,
) -> Result<Value, Error> {
    let values_meta = vec![
        id(arena, id_al)?,
        list(arena, "id", tparams_al, id)?,
        list(arena, "param", params_al, param)?,
        typ(arena, typ_al)?,
    ];
    case(arena, m::FUN_PARAM, "param", values_meta, span)
}

/// Encodes an AL premise.
fn prem(arena: &mut Arena, prem_al: &ast::Prem) -> Result<Value, Error> {
    let span = prem_al.span;
    match &prem_al.node {
        ast::PremKind::Rule(prem_inner_al) => encode_rule_prem(arena, prem_inner_al, span),
        ast::PremKind::If(prem_inner_al) => encode_if_prem(arena, prem_inner_al, span),
        ast::PremKind::IfHold(prem_inner_al) => encode_ifhold_prem(arena, prem_inner_al, span),
        ast::PremKind::IfNotHold(prem_inner_al) => {
            encode_ifnothold_prem(arena, prem_inner_al, span)
        }
        ast::PremKind::Let(prem_inner_al) => encode_let_prem(arena, prem_inner_al, span),
        ast::PremKind::Iter(prem_inner_al) => encode_iter_prem(arena, prem_inner_al, span),
        ast::PremKind::Debug(prem_inner_al) => encode_debug_prem(arena, prem_inner_al, span),
    }
}

/// Encodes a boolean condition premise.
fn encode_if_prem(
    arena: &mut Arena,
    prem_inner_al: &ast::IfPrem,
    span: Span,
) -> Result<Value, Error> {
    let values_meta = vec![exp(arena, &prem_inner_al.exp)?];
    case(arena, m::IF, "prem", values_meta, span)
}

/// Encodes a positive relation test.
fn encode_ifhold_prem(
    arena: &mut Arena,
    prem_inner_al: &ast::IfHoldPrem,
    span: Span,
) -> Result<Value, Error> {
    let values_meta =
        vec![id(arena, &prem_inner_al.id)?, exps(arena, prem_inner_al.not_exp.args())?];
    case(arena, m::IFHOLD, "prem", values_meta, span)
}

/// Encodes a negative relation test.
fn encode_ifnothold_prem(
    arena: &mut Arena,
    prem_inner_al: &ast::IfNotHoldPrem,
    span: Span,
) -> Result<Value, Error> {
    let values_meta =
        vec![id(arena, &prem_inner_al.id)?, exps(arena, prem_inner_al.not_exp.args())?];
    case(arena, m::IFNOTHOLD, "prem", values_meta, span)
}

/// Encodes the two expressions of a binding premise.
fn encode_let_prem(
    arena: &mut Arena,
    prem_inner_al: &ast::LetPrem,
    span: Span,
) -> Result<Value, Error> {
    let values_meta = vec![exp(arena, &prem_inner_al.exp_l)?, exp(arena, &prem_inner_al.exp_r)?];
    case(arena, m::LET, "prem", values_meta, span)
}

/// Encodes an iterated premise and its bound variables.
fn encode_iter_prem(
    arena: &mut Arena,
    prem_inner_al: &ast::IterPrem,
    span: Span,
) -> Result<Value, Error> {
    let values_meta =
        vec![prem(arena, &prem_inner_al.prem)?, iterprem(arena, &prem_inner_al.prem_iter)?];
    case(arena, m::ITER_PREM, "prem", values_meta, span)
}

/// Encodes the expression of a debug premise.
fn encode_debug_prem(
    arena: &mut Arena,
    prem_inner_al: &ast::DebugPrem,
    span: Span,
) -> Result<Value, Error> {
    let values_meta = vec![exp(arena, &prem_inner_al.exp)?];
    case(arena, m::DEBUG, "prem", values_meta, span)
}

/// Encodes bound and bound-by-iteration variable lists in order.
fn iterprem(arena: &mut Arena, prem_iter_al: &ast::PremIter) -> Result<Value, Error> {
    let values_meta = vec![
        iter(arena, &prem_iter_al.iter)?,
        list(arena, "vari", &prem_iter_al.vars_bound, var)?,
        list(arena, "vari", &prem_iter_al.vars_bind, var)?,
    ];
    case(arena, m::ITERPREM, "iterprem", values_meta, Span::default())
}

/// Encodes a rule match without its printing signature.
fn rulematch(arena: &mut Arena, rule_match_al: &ast::RuleMatch) -> Result<Value, Error> {
    let values_meta = vec![
        exps(arena, &rule_match_al.exps_input)?,
        list(arena, "prem", &rule_match_al.prems, prem)?,
    ];
    case(arena, m::RULMATCH, "rulmatch", values_meta, Span::default())
}

/// Encodes a rule path and its outputs.
fn rulepath(arena: &mut Arena, rule_path_al: &ast::RulePath) -> Result<Value, Error> {
    let values_meta = vec![
        id(arena, &rule_path_al.id)?,
        exps(arena, &rule_path_al.exps_output)?,
        list(arena, "prem", &rule_path_al.prems, prem)?,
    ];
    case(arena, m::RULPATH, "rulpath", values_meta, rule_path_al.id.span)
}

/// Encodes a rule group with its shared match.
fn rulegroup(arena: &mut Arena, rule_group_al: &ast::RuleGroup) -> Result<Value, Error> {
    let values_meta = vec![
        id(arena, &rule_group_al.node.id)?,
        rulematch(arena, &rule_group_al.node.rule_match)?,
        list(arena, "rulpath", &rule_group_al.node.rule_paths, rulepath)?,
    ];
    case(arena, m::RULGROUP, "rulgroup", values_meta, rule_group_al.span)
}

/// Encodes an otherwise group with one rule path.
fn elsegroup(arena: &mut Arena, group_else_al: &ast::ElseGroup) -> Result<Value, Error> {
    let values_meta = vec![
        id(arena, &group_else_al.node.id)?,
        rulematch(arena, &group_else_al.node.rule_match)?,
        rulepath(arena, &group_else_al.node.rule_path)?,
    ];
    case(arena, m::ELSGROUP, "elsgroup", values_meta, group_else_al.span)
}

/// Encodes a function clause.
fn clause(arena: &mut Arena, clause_al: &ast::Clause) -> Result<Value, Error> {
    let values_meta = vec![
        args(arena, &clause_al.node.args)?,
        exp(arena, &clause_al.node.exp)?,
        list(arena, "prem", &clause_al.node.prems, prem)?,
    ];
    case(arena, m::CLAUSE, "clause", values_meta, clause_al.span)
}

/// Encodes a table row without its printing signature.
fn tablerow(arena: &mut Arena, row_al: &ast::TableRow) -> Result<Value, Error> {
    let values_meta = vec![
        args(arena, &row_al.node.args)?,
        exp(arena, &row_al.node.exp)?,
        list(arena, "prem", &row_al.node.prems, prem)?,
    ];
    case(arena, m::CLAUSE, "tblrow", values_meta, row_al.span)
}

/// Encodes a relation premise after splitting its operational input positions.
fn encode_rule_prem(
    arena: &mut Arena,
    prem_rule_al: &ast::RulePrem,
    span: Span,
) -> Result<Value, Error> {
    // Preserve source order within each input and output group
    let (exps_input_al, exps_output_al) =
        input::split(&prem_rule_al.input_hint, prem_rule_al.not_exp.args().to_vec())?;
    let values_meta = vec![
        id(arena, &prem_rule_al.id)?,
        exps(arena, &exps_input_al)?,
        exps(arena, &exps_output_al)?,
    ];
    case(arena, m::REL_PREM, "prem", values_meta, span)
}
