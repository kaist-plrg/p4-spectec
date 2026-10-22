//! Encodes elaborated IL specifications as meta data
//!
//! Expressions, types, paths, arguments, and parameters use the AL encoders,
//! since IL and AL share those forms in the meta syntax.
//! Relation arguments stay in notation order next to their input positions.
//! Variable declarations and general hints are omitted.

use super::mixops as m;
use crate::{
    interface::al::{
        Cache, Error,
        encode::{Encode, case, list, opt},
    },
    lang::{
        common::source::Span,
        data::{
            arena::Arena,
            typ::make as make_typ,
            value::flat::{Value, make},
        },
        hints::input::InputHint,
        il::ast as il,
    },
    phrase,
};

// - Input positions

/// Encodes input positions in their declared order.
impl Encode for InputHint {
    type Output = Value;

    fn encode_cached(&self, arena: &mut Arena, _cache: &mut Cache) -> Result<Self::Output, Error> {
        let values_meta = self
            .indices()
            .iter()
            .map(|idx| make::nat(arena, (idx.node as u64).into(), idx.span))
            .collect::<Result<Vec<_>, _>>()?;
        Ok(make::list(
            arena,
            make_typ::var(phrase!(node: "inputHint".into(), span: Span::default()), vec![])
                .node
                .into(),
            values_meta,
            Span::default(),
        )?)
    }
}

// - Premises

/// Encodes an IL premise; relation arguments keep their notation order.
impl Encode for il::Prem {
    type Output = Value;

    fn encode_cached(&self, arena: &mut Arena, cache: &mut Cache) -> Result<Self::Output, Error> {
        let (shape, values_meta) = match &self.node {
            il::PremKind::Rule(prem_rule_il) => (
                m::REL_PREM,
                vec![
                    prem_rule_il.id.encode_cached(arena, cache)?,
                    list(arena, cache, "exp", prem_rule_il.not_exp.args())?,
                    prem_rule_il.input_hint.encode_cached(arena, cache)?,
                ],
            ),
            il::PremKind::If(prem_if_il) => {
                (m::IF, vec![prem_if_il.exp.encode_cached(arena, cache)?])
            }
            il::PremKind::IfHold(prem_hold_il) => (
                m::IFHOLD,
                vec![
                    prem_hold_il.id.encode_cached(arena, cache)?,
                    list(arena, cache, "exp", prem_hold_il.not_exp.args())?,
                ],
            ),
            il::PremKind::IfNotHold(prem_hold_il) => (
                m::IFNOTHOLD,
                vec![
                    prem_hold_il.id.encode_cached(arena, cache)?,
                    list(arena, cache, "exp", prem_hold_il.not_exp.args())?,
                ],
            ),
            il::PremKind::Iter(prem_iter_il) => (
                m::ITER_PREM,
                vec![
                    prem_iter_il.prem.encode_cached(arena, cache)?,
                    prem_iter_il.prem_iter.encode_cached(arena, cache)?,
                ],
            ),
            il::PremKind::Debug(prem_debug_il) => {
                (m::DEBUG, vec![prem_debug_il.exp.encode_cached(arena, cache)?])
            }
        };
        case(arena, shape, "premIL", values_meta, self.span)
    }
}

// - Rules

/// Encodes a rule with its arguments in notation order.
impl Encode for il::Rule {
    type Output = Value;

    fn encode_cached(&self, arena: &mut Arena, cache: &mut Cache) -> Result<Self::Output, Error> {
        let values_meta = vec![
            self.node.id.encode_cached(arena, cache)?,
            list(arena, cache, "exp", self.node.not_exp.args())?,
            list(arena, cache, "premIL", &self.node.prems)?,
        ];
        case(arena, m::RUL, "rulIL", values_meta, self.span)
    }
}

/// Encodes a rule group with its rules in order.
impl Encode for il::RuleGroup {
    type Output = Value;

    fn encode_cached(&self, arena: &mut Arena, cache: &mut Cache) -> Result<Self::Output, Error> {
        let values_meta = vec![
            self.node.id.encode_cached(arena, cache)?,
            list(arena, cache, "rulIL", &self.node.rules)?,
        ];
        case(arena, m::RULGROUP, "rulGroupIL", values_meta, self.span)
    }
}

/// Encodes an otherwise group without its keyword annotation.
impl Encode for il::ElseGroup {
    type Output = Value;

    fn encode_cached(&self, arena: &mut Arena, cache: &mut Cache) -> Result<Self::Output, Error> {
        let values_meta = vec![
            self.node.id.encode_cached(arena, cache)?,
            self.node.rule.encode_cached(arena, cache)?,
        ];
        case(arena, m::ELSGROUP, "elsGroupIL", values_meta, self.span)
    }
}

// - Clauses and table rows

/// Encodes a function clause; an otherwise clause occupies its own slot.
impl Encode for il::Clause {
    type Output = Value;

    fn encode_cached(&self, arena: &mut Arena, cache: &mut Cache) -> Result<Self::Output, Error> {
        let values_meta = vec![
            list(arena, cache, "arg", &self.node.args)?,
            self.node.exp.encode_cached(arena, cache)?,
            list(arena, cache, "premIL", &self.node.prems)?,
        ];
        case(arena, m::CLAUSE, "clauseIL", values_meta, self.span)
    }
}

/// Encodes a table row.
impl Encode for il::TableRow {
    type Output = Value;

    fn encode_cached(&self, arena: &mut Arena, cache: &mut Cache) -> Result<Self::Output, Error> {
        let values_meta = vec![
            list(arena, cache, "arg", &self.node.args)?,
            self.node.exp.encode_cached(arena, cache)?,
        ];
        case(arena, m::TBLROW, "tblRowIL", values_meta, self.span)
    }
}

// - Definitions

/// Encodes a definition; only a variable declaration returns None.
impl Encode for il::Def {
    type Output = Option<Value>;

    fn encode_cached(&self, arena: &mut Arena, cache: &mut Cache) -> Result<Self::Output, Error> {
        let (shape, values_meta) = match &self.node {
            il::DefKind::Var(_) => return Ok(None),
            il::DefKind::Typ(il::TypDef::Extern(typdef_il)) => {
                (m::EXTTYP, vec![typdef_il.id.encode_cached(arena, cache)?])
            }
            il::DefKind::Typ(il::TypDef::Defined(typdef_il)) => (
                m::TYP_DEF,
                vec![
                    typdef_il.id.encode_cached(arena, cache)?,
                    list(arena, cache, "id", &typdef_il.tparams)?,
                    typdef_il.def_typ.encode_cached(arena, cache)?,
                ],
            ),
            il::DefKind::Rel(il::RelDef::Extern(rel_il)) => (
                m::EXTREL,
                vec![
                    rel_il.id.encode_cached(arena, cache)?,
                    list(arena, cache, "typ", rel_il.not_typ.node.args())?,
                    rel_il.input_hint.encode_cached(arena, cache)?,
                ],
            ),
            il::DefKind::Rel(il::RelDef::Defined(rel_il)) => (
                m::REL_DEF,
                vec![
                    rel_il.id.encode_cached(arena, cache)?,
                    list(arena, cache, "typ", rel_il.not_typ.node.args())?,
                    rel_il.input_hint.encode_cached(arena, cache)?,
                    list(arena, cache, "rulGroupIL", &rel_il.rule_groups)?,
                    opt(arena, cache, "elsGroupIL", rel_il.else_group.as_ref())?,
                ],
            ),
            il::DefKind::MetaFunc(il::MetaFuncDef::Extern(func_il)) => (
                m::EXTFUNC,
                vec![
                    func_il.id.encode_cached(arena, cache)?,
                    list(arena, cache, "id", &func_il.tparams)?,
                    list(arena, cache, "param", &func_il.params)?,
                    func_il.typ.encode_cached(arena, cache)?,
                ],
            ),
            il::DefKind::MetaFunc(il::MetaFuncDef::Builtin(func_il)) => (
                m::BUILTINFUNC,
                vec![
                    func_il.id.encode_cached(arena, cache)?,
                    list(arena, cache, "id", &func_il.tparams)?,
                    list(arena, cache, "param", &func_il.params)?,
                    func_il.typ.encode_cached(arena, cache)?,
                ],
            ),
            il::DefKind::MetaFunc(il::MetaFuncDef::Table(func_il)) => (
                m::TABLEFUNC,
                vec![
                    func_il.id.encode_cached(arena, cache)?,
                    list(arena, cache, "param", &func_il.params)?,
                    func_il.typ.encode_cached(arena, cache)?,
                    list(arena, cache, "tblRowIL", &func_il.rows)?,
                ],
            ),
            il::DefKind::MetaFunc(il::MetaFuncDef::Defined(func_il)) => (
                m::FUNC_DEF,
                vec![
                    func_il.id.encode_cached(arena, cache)?,
                    list(arena, cache, "id", &func_il.tparams)?,
                    list(arena, cache, "param", &func_il.params)?,
                    func_il.typ.encode_cached(arena, cache)?,
                    list(arena, cache, "clauseIL", &func_il.clauses)?,
                    opt(arena, cache, "elsClauseIL", func_il.else_clause.as_ref())?,
                ],
            ),
        };
        Ok(Some(case(arena, shape, "defnIL", values_meta, self.span)?))
    }
}

// - Specifications

/// Encodes a whole IL specification, omitting only variable declarations.
impl Encode for il::Spec {
    type Output = Value;

    fn encode_cached(&self, arena: &mut Arena, cache: &mut Cache) -> Result<Self::Output, Error> {
        // Preserve definition order while filtering the explicitly omitted form
        let mut values_meta = Vec::with_capacity(self.len());
        for def_il in self {
            if let Some(value) = def_il.encode_cached(arena, cache)? {
                values_meta.push(value);
            }
        }
        Ok(make::list(
            arena,
            make_typ::var(phrase!(node: "specIL".into(), span: Span::default()), vec![])
                .node
                .into(),
            values_meta,
            Span::default(),
        )?)
    }
}
