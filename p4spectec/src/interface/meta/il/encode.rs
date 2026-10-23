//! Encodes elaborated IL specifications as meta data
//!
//! Expressions, types, paths, arguments, and parameters use the encoders of
//! `meta::shared::encode`, since IL and AL share those forms in the meta syntax.
//! Relation arguments stay in notation order next to their input positions.
//! Variable declarations and general hints are omitted.

use super::mixops as m;
use crate::{
    interface::meta::shared::{
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

// == Encoding

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
                m::PREM_REL,
                vec![
                    prem_rule_il.id.encode_cached(arena, cache)?,
                    list(arena, cache, "typedExp", prem_rule_il.not_exp.args())?,
                    prem_rule_il.input_hint.encode_cached(arena, cache)?,
                ],
            ),
            il::PremKind::If(prem_if_il) => {
                (m::PREM_IF, vec![prem_if_il.exp.encode_cached(arena, cache)?])
            }
            il::PremKind::IfHold(prem_hold_il) => (
                m::PREM_IFHOLD,
                vec![
                    prem_hold_il.id.encode_cached(arena, cache)?,
                    list(arena, cache, "typedExp", prem_hold_il.not_exp.args())?,
                ],
            ),
            il::PremKind::IfNotHold(prem_hold_il) => (
                m::PREM_IFNOTHOLD,
                vec![
                    prem_hold_il.id.encode_cached(arena, cache)?,
                    list(arena, cache, "typedExp", prem_hold_il.not_exp.args())?,
                ],
            ),
            il::PremKind::Iter(prem_iter_il) => (
                m::PREM_ITER,
                vec![
                    prem_iter_il.prem.encode_cached(arena, cache)?,
                    PremIterIl(&prem_iter_il.prem_iter).encode_cached(arena, cache)?,
                ],
            ),
            il::PremKind::Debug(prem_debug_il) => {
                (m::PREM_DEBUG, vec![prem_debug_il.exp.encode_cached(arena, cache)?])
            }
        };
        case(arena, shape, "premIL", values_meta, self.span)
    }
}

/// An IL iteration premise, as distinct from the AL form of the same type.
struct PremIterIl<'a>(&'a il::PremIter);

/// Encodes an iteration premise, which elaboration leaves without binders.
impl Encode for PremIterIl<'_> {
    type Output = Value;

    fn encode_cached(&self, arena: &mut Arena, cache: &mut Cache) -> Result<Self::Output, Error> {
        let Self(prem_iter_il) = self;
        // Binders are decided by the IL-to-AL specification
        if !prem_iter_il.vars_bind.is_empty() {
            return Err(Error::malformed(
                Span::default(),
                "an IL iteration premise without binders",
                format!("{} binders", prem_iter_il.vars_bind.len()),
            ));
        }
        let values_meta = vec![
            prem_iter_il.iter.encode_cached(arena, cache)?,
            list(arena, cache, "vari", &prem_iter_il.vars_bound)?,
        ];
        case(arena, m::ITER_PREM, "iterPremIL", values_meta, Span::default())
    }
}

// - Rules

/// Encodes a rule with its arguments in notation order.
impl Encode for il::Rule {
    type Output = Value;

    fn encode_cached(&self, arena: &mut Arena, cache: &mut Cache) -> Result<Self::Output, Error> {
        let values_meta = vec![
            self.node.id.encode_cached(arena, cache)?,
            list(arena, cache, "typedExp", self.node.not_exp.args())?,
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
        case(arena, m::RUL_GROUP, "rulGroupIL", values_meta, self.span)
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
        case(arena, m::ELS_GROUP, "elsGroupIL", values_meta, self.span)
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
        case(arena, m::TBL_ROW, "tblRowIL", values_meta, self.span)
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
                (m::DEF_EXTTYP, vec![typdef_il.id.encode_cached(arena, cache)?])
            }
            il::DefKind::Typ(il::TypDef::Defined(typdef_il)) => (
                m::DEF_TYP,
                vec![
                    typdef_il.id.encode_cached(arena, cache)?,
                    list(arena, cache, "id", &typdef_il.tparams)?,
                    typdef_il.def_typ.encode_cached(arena, cache)?,
                ],
            ),
            il::DefKind::Rel(il::RelDef::Extern(rel_il)) => (
                m::DEF_EXTREL,
                vec![
                    rel_il.id.encode_cached(arena, cache)?,
                    list(arena, cache, "typ", rel_il.not_typ.node.args())?,
                    rel_il.input_hint.encode_cached(arena, cache)?,
                ],
            ),
            il::DefKind::Rel(il::RelDef::Defined(rel_il)) => (
                m::DEF_REL,
                vec![
                    rel_il.id.encode_cached(arena, cache)?,
                    list(arena, cache, "typ", rel_il.not_typ.node.args())?,
                    rel_il.input_hint.encode_cached(arena, cache)?,
                    list(arena, cache, "rulGroupIL", &rel_il.rule_groups)?,
                    opt(arena, cache, "elsGroupIL", rel_il.else_group.as_ref())?,
                ],
            ),
            il::DefKind::MetaFunc(il::MetaFuncDef::Extern(func_il)) => (
                m::DEF_EXTFUNC,
                vec![
                    func_il.id.encode_cached(arena, cache)?,
                    list(arena, cache, "id", &func_il.tparams)?,
                    list(arena, cache, "param", &func_il.params)?,
                    func_il.typ.encode_cached(arena, cache)?,
                ],
            ),
            il::DefKind::MetaFunc(il::MetaFuncDef::Builtin(func_il)) => (
                m::DEF_BUILTINFUNC,
                vec![
                    func_il.id.encode_cached(arena, cache)?,
                    list(arena, cache, "id", &func_il.tparams)?,
                    list(arena, cache, "param", &func_il.params)?,
                    func_il.typ.encode_cached(arena, cache)?,
                ],
            ),
            il::DefKind::MetaFunc(il::MetaFuncDef::Table(func_il)) => (
                m::DEF_TABLEFUNC,
                vec![
                    func_il.id.encode_cached(arena, cache)?,
                    list(arena, cache, "param", &func_il.params)?,
                    func_il.typ.encode_cached(arena, cache)?,
                    list(arena, cache, "tblRowIL", &func_il.rows)?,
                ],
            ),
            il::DefKind::MetaFunc(il::MetaFuncDef::Defined(func_il)) => (
                m::DEF_FUNC,
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
