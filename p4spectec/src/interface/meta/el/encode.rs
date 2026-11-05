//! Encodes parsed EL specifications as meta data
//!
//! Identifiers, atoms, iterators, and operators use the encoders of
//! `meta::shared::encode`, since EL shares those forms in the meta syntax.
//! Spans of nested forms, literal radixes, separators, and hints other than
//! `input` are omitted.

use super::mixops as m;
use crate::{
    interface::meta::shared::{
        Cache, Error,
        encode::{Encode, case, list},
    },
    lang::{
        common::{prim::num::Number, source::Span},
        data::{
            arena::Arena,
            typ::make as make_typ,
            value::flat::{Value, make},
        },
        el::ast as el,
    },
    phrase,
};

// == Encoding

// - Types

/// Encodes a plain type, keeping its parentheses.
impl Encode for el::PlainTyp {
    type Output = Value;

    fn encode_cached(&self, arena: &mut Arena, cache: &mut Cache) -> Result<Self::Output, Error> {
        let (shape, values_meta) = match &self.node {
            el::PlainTypKind::Bool => (m::PLAIN_TYP_BOOL, vec![]),
            el::PlainTypKind::Num(num_typ) => {
                let shape = match num_typ {
                    crate::lang::common::prim::num::Typ::Nat => m::PLAIN_TYP_NAT,
                    crate::lang::common::prim::num::Typ::Int => m::PLAIN_TYP_INT,
                };
                return case(arena, shape, "numtyp", vec![], self.span);
            }
            el::PlainTypKind::Text => (m::PLAIN_TYP_TEXT, vec![]),
            el::PlainTypKind::Var(id, targs) => (
                m::PLAIN_TYP_VAR,
                vec![id.encode_cached(arena, cache)?, list(arena, cache, "plainTypEL", targs)?],
            ),
            el::PlainTypKind::Paren(plain_typ) => {
                (m::PLAIN_TYP_PAREN, vec![plain_typ.as_ref().encode_cached(arena, cache)?])
            }
            el::PlainTypKind::Tuple(plain_typs) => {
                (m::PLAIN_TYP_TUP, vec![list(arena, cache, "plainTypEL", plain_typs)?])
            }
            el::PlainTypKind::Iter(plain_typ, iter) => (
                m::PLAIN_TYP_ITER,
                vec![
                    plain_typ.as_ref().encode_cached(arena, cache)?,
                    iter.encode_cached(arena, cache)?,
                ],
            ),
        };
        case(arena, shape, "plainTypEL", values_meta, self.span)
    }
}

/// Encodes a plain or notation type.
impl Encode for el::Typ {
    type Output = Value;

    fn encode_cached(&self, arena: &mut Arena, cache: &mut Cache) -> Result<Self::Output, Error> {
        let (shape, value_meta, span) = match self {
            el::Typ::Plain(plain_typ) => {
                (m::TYP_PLAIN, plain_typ.encode_cached(arena, cache)?, plain_typ.span)
            }
            el::Typ::Notation(not_typ) => {
                (m::TYP_NOTATION, not_typ.encode_cached(arena, cache)?, not_typ.span)
            }
        };
        case(arena, shape, "typEL", vec![value_meta], span)
    }
}

/// Encodes a notation type by its tree shape.
impl Encode for el::NotTyp {
    type Output = Value;

    fn encode_cached(&self, arena: &mut Arena, cache: &mut Cache) -> Result<Self::Output, Error> {
        let (shape, values_meta) = match &self.node {
            el::NotTypKind::Atom(atom) => {
                (m::NOT_TYP_ATOM, vec![atom.encode_cached(arena, cache)?])
            }
            el::NotTypKind::Seq(typs) => (m::NOT_TYP_SEQ, vec![list(arena, cache, "typEL", typs)?]),
            el::NotTypKind::Infix(typ_l, atom, typ_r) => (
                m::NOT_TYP_INFIX,
                vec![
                    typ_l.as_ref().encode_cached(arena, cache)?,
                    atom.encode_cached(arena, cache)?,
                    typ_r.as_ref().encode_cached(arena, cache)?,
                ],
            ),
            el::NotTypKind::Brack(atom_l, typ, atom_r) => (
                m::NOT_TYP_BRACK,
                vec![
                    atom_l.encode_cached(arena, cache)?,
                    typ.as_ref().encode_cached(arena, cache)?,
                    atom_r.encode_cached(arena, cache)?,
                ],
            ),
        };
        case(arena, shape, "notTypEL", values_meta, self.span)
    }
}

/// Encodes a struct field without its hints.
impl Encode for el::TypField {
    type Output = Value;

    fn encode_cached(&self, arena: &mut Arena, cache: &mut Cache) -> Result<Self::Output, Error> {
        let values_meta =
            vec![self.atom.encode_cached(arena, cache)?, self.typ.encode_cached(arena, cache)?];
        case(arena, m::TYP_FIELD, "typFieldEL", values_meta, self.atom.span)
    }
}

/// Encodes a variant case as its type, without its hints.
impl Encode for el::TypCase {
    type Output = Value;

    fn encode_cached(&self, arena: &mut Arena, cache: &mut Cache) -> Result<Self::Output, Error> {
        self.typ.encode_cached(arena, cache)
    }
}

/// Encodes the body of a type definition.
impl Encode for el::DefTyp {
    type Output = Value;

    fn encode_cached(&self, arena: &mut Arena, cache: &mut Cache) -> Result<Self::Output, Error> {
        let (shape, value_meta) = match &self.node {
            el::DefTypKind::Plain(plain_typ) => {
                (m::DEF_TYP_ALIAS, plain_typ.encode_cached(arena, cache)?)
            }
            el::DefTypKind::Struct(fields) => {
                (m::DEF_TYP_STRUCT, list(arena, cache, "typFieldEL", fields)?)
            }
            el::DefTypKind::Variant(cases) => {
                (m::DEF_TYP_VARIANT, list(arena, cache, "typEL", cases)?)
            }
        };
        case(arena, shape, "defTypEL", vec![value_meta], self.span)
    }
}

// - Expressions

/// Encodes a struct expression field.
impl Encode for (el::Atom, el::Exp) {
    type Output = Value;

    fn encode_cached(&self, arena: &mut Arena, cache: &mut Cache) -> Result<Self::Output, Error> {
        let (atom, exp) = self;
        let values_meta = vec![atom.encode_cached(arena, cache)?, exp.encode_cached(arena, cache)?];
        case(arena, m::EXP_FIELD, "expFieldEL", values_meta, atom.span)
    }
}

/// Encodes a hint placeholder.
impl Encode for el::Hole {
    type Output = Value;

    fn encode_cached(&self, arena: &mut Arena, _cache: &mut Cache) -> Result<Self::Output, Error> {
        let (shape, values_meta) = match self {
            el::Hole::Num(idx) => {
                (m::HOLE_NTH, vec![make::nat(arena, (*idx as u64).into(), Span::default())?])
            }
            el::Hole::Next => (m::HOLE_NEXT, vec![]),
            el::Hole::Rest => (m::HOLE_REST, vec![]),
            el::Hole::None => (m::HOLE_NONE, vec![]),
        };
        case(arena, shape, "holeEL", values_meta, Span::default())
    }
}

/// Encodes an expression as written, including notation and hint-only forms.
impl Encode for el::Exp {
    type Output = Value;

    fn encode_cached(&self, arena: &mut Arena, cache: &mut Cache) -> Result<Self::Output, Error> {
        let span = self.span;
        let (shape, values_meta) = match &self.node {
            el::ExpKind::Bool(value_bool) => {
                (m::EXP_BOOL, vec![make::bool(arena, *value_bool, span)?])
            }
            el::ExpKind::Num(_, num) => {
                let shape = match num {
                    Number::Nat(_) => m::EXP_NAT,
                    Number::Int(_) => m::EXP_INT,
                };
                let value_num = make::num(arena, num.clone(), span)?;
                return case(arena, shape, "num", vec![value_num], span);
            }
            el::ExpKind::Text(text) => (m::EXP_TEXT, vec![make::text(arena, text.clone(), span)?]),
            el::ExpKind::Id(id) => (m::EXP_ID, vec![id.encode_cached(arena, cache)?]),
            el::ExpKind::Un(op, exp) => (
                m::EXP_UN,
                vec![op.encode_cached(arena, cache)?, exp.as_ref().encode_cached(arena, cache)?],
            ),
            el::ExpKind::Bin(exp_l, op, exp_r) => (
                m::EXP_BIN,
                vec![
                    exp_l.as_ref().encode_cached(arena, cache)?,
                    op.encode_cached(arena, cache)?,
                    exp_r.as_ref().encode_cached(arena, cache)?,
                ],
            ),
            el::ExpKind::Cmp(exp_l, op, exp_r) => (
                m::EXP_CMP,
                vec![
                    exp_l.as_ref().encode_cached(arena, cache)?,
                    op.encode_cached(arena, cache)?,
                    exp_r.as_ref().encode_cached(arena, cache)?,
                ],
            ),
            el::ExpKind::Arith(exp) => {
                (m::EXP_ARITH, vec![exp.as_ref().encode_cached(arena, cache)?])
            }
            el::ExpKind::Eps => (m::EXP_EPS, vec![]),
            el::ExpKind::List(exps) => (m::EXP_LIST, vec![list(arena, cache, "expEL", exps)?]),
            el::ExpKind::Cons(exp_head, exp_tail) => (
                m::EXP_CONS,
                vec![
                    exp_head.as_ref().encode_cached(arena, cache)?,
                    exp_tail.as_ref().encode_cached(arena, cache)?,
                ],
            ),
            el::ExpKind::Cat(exp_l, exp_r) => (
                m::EXP_CAT,
                vec![
                    exp_l.as_ref().encode_cached(arena, cache)?,
                    exp_r.as_ref().encode_cached(arena, cache)?,
                ],
            ),
            el::ExpKind::Idx(exp_base, exp_idx) => (
                m::EXP_IDX,
                vec![
                    exp_base.as_ref().encode_cached(arena, cache)?,
                    exp_idx.as_ref().encode_cached(arena, cache)?,
                ],
            ),
            el::ExpKind::Slice(exp_base, exp_idx, exp_len) => (
                m::EXP_SLICE,
                vec![
                    exp_base.as_ref().encode_cached(arena, cache)?,
                    exp_idx.as_ref().encode_cached(arena, cache)?,
                    exp_len.as_ref().encode_cached(arena, cache)?,
                ],
            ),
            el::ExpKind::Len(exp) => (m::EXP_LEN, vec![exp.as_ref().encode_cached(arena, cache)?]),
            el::ExpKind::Mem(exp_elem, exp_set) => (
                m::EXP_MEM,
                vec![
                    exp_elem.as_ref().encode_cached(arena, cache)?,
                    exp_set.as_ref().encode_cached(arena, cache)?,
                ],
            ),
            el::ExpKind::Str(fields) => {
                (m::EXP_STR, vec![list(arena, cache, "expFieldEL", fields)?])
            }
            el::ExpKind::Dot(exp, atom) => (
                m::EXP_DOT,
                vec![exp.as_ref().encode_cached(arena, cache)?, atom.encode_cached(arena, cache)?],
            ),
            el::ExpKind::Upd(exp_base, path, exp_field) => (
                m::EXP_UPD,
                vec![
                    exp_base.as_ref().encode_cached(arena, cache)?,
                    path.encode_cached(arena, cache)?,
                    exp_field.as_ref().encode_cached(arena, cache)?,
                ],
            ),
            el::ExpKind::Paren(exp) => {
                (m::EXP_PAREN, vec![exp.as_ref().encode_cached(arena, cache)?])
            }
            el::ExpKind::Tuple(exps) => (m::EXP_TUP, vec![list(arena, cache, "expEL", exps)?]),
            el::ExpKind::Call(id, targs, args) => (
                m::EXP_CALL,
                vec![
                    id.encode_cached(arena, cache)?,
                    list(arena, cache, "plainTypEL", targs)?,
                    list(arena, cache, "argEL", args)?,
                ],
            ),
            el::ExpKind::Iter(exp, iter) => (
                m::EXP_ITER,
                vec![exp.as_ref().encode_cached(arena, cache)?, iter.encode_cached(arena, cache)?],
            ),
            el::ExpKind::Sub(exp, plain_typ) => (
                m::EXP_SUB,
                vec![
                    exp.as_ref().encode_cached(arena, cache)?,
                    plain_typ.encode_cached(arena, cache)?,
                ],
            ),
            el::ExpKind::Atom(atom) => (m::EXP_ATOM, vec![atom.encode_cached(arena, cache)?]),
            el::ExpKind::Seq(exps) => (m::EXP_SEQ, vec![list(arena, cache, "expEL", exps)?]),
            el::ExpKind::Infix(exp_l, atom, exp_r) => (
                m::EXP_INFIX,
                vec![
                    exp_l.as_ref().encode_cached(arena, cache)?,
                    atom.encode_cached(arena, cache)?,
                    exp_r.as_ref().encode_cached(arena, cache)?,
                ],
            ),
            el::ExpKind::Brack(atom_l, exp, atom_r) => (
                m::EXP_BRACK,
                vec![
                    atom_l.encode_cached(arena, cache)?,
                    exp.as_ref().encode_cached(arena, cache)?,
                    atom_r.encode_cached(arena, cache)?,
                ],
            ),
            el::ExpKind::Hole(hole) => (m::EXP_HOLE, vec![hole.encode_cached(arena, cache)?]),
            el::ExpKind::Fuse(exp_l, _, exp_r) => (
                m::EXP_FUSE,
                vec![
                    exp_l.as_ref().encode_cached(arena, cache)?,
                    exp_r.as_ref().encode_cached(arena, cache)?,
                ],
            ),
            el::ExpKind::Unparen(exp) => {
                (m::EXP_UNPAREN, vec![exp.as_ref().encode_cached(arena, cache)?])
            }
            el::ExpKind::Latex(text) => {
                (m::EXP_LATEX, vec![make::text(arena, text.clone(), span)?])
            }
        };
        case(arena, shape, "expEL", values_meta, span)
    }
}

// - Paths

/// Encodes a path from the root outward.
impl Encode for el::Path {
    type Output = Value;

    fn encode_cached(&self, arena: &mut Arena, cache: &mut Cache) -> Result<Self::Output, Error> {
        let (shape, values_meta) = match &self.node {
            el::PathKind::Root => (m::PATH_ROOT, vec![]),
            el::PathKind::Idx(path, exp_idx) => (
                m::PATH_IDX,
                vec![
                    path.as_ref().encode_cached(arena, cache)?,
                    exp_idx.as_ref().encode_cached(arena, cache)?,
                ],
            ),
            el::PathKind::Slice(path, exp_idx, exp_len) => (
                m::PATH_SLICE,
                vec![
                    path.as_ref().encode_cached(arena, cache)?,
                    exp_idx.as_ref().encode_cached(arena, cache)?,
                    exp_len.as_ref().encode_cached(arena, cache)?,
                ],
            ),
            el::PathKind::Dot(path, atom) => (
                m::PATH_DOT,
                vec![path.as_ref().encode_cached(arena, cache)?, atom.encode_cached(arena, cache)?],
            ),
        };
        case(arena, shape, "pathEL", values_meta, self.span)
    }
}

// - Arguments and parameters

/// Encodes an expression or function argument.
impl Encode for el::Arg {
    type Output = Value;

    fn encode_cached(&self, arena: &mut Arena, cache: &mut Cache) -> Result<Self::Output, Error> {
        let (shape, value_meta) = match &self.node {
            el::ArgKind::Exp(exp) => (m::ARG_EXP, exp.as_ref().encode_cached(arena, cache)?),
            el::ArgKind::Def(id) => (m::ARG_FUN, id.encode_cached(arena, cache)?),
        };
        case(arena, shape, "argEL", vec![value_meta], self.span)
    }
}

/// Encodes a value or function parameter.
impl Encode for el::Param {
    type Output = Value;

    fn encode_cached(&self, arena: &mut Arena, cache: &mut Cache) -> Result<Self::Output, Error> {
        let (shape, values_meta) = match &self.node {
            el::ParamKind::Exp(plain_typ) => {
                (m::PARAM_EXP, vec![plain_typ.encode_cached(arena, cache)?])
            }
            el::ParamKind::Def(id, tparams, params, plain_typ) => (
                m::PARAM_FUN,
                vec![
                    id.encode_cached(arena, cache)?,
                    list(arena, cache, "tparam", tparams)?,
                    list(arena, cache, "paramEL", params)?,
                    plain_typ.encode_cached(arena, cache)?,
                ],
            ),
        };
        case(arena, shape, "paramEL", values_meta, self.span)
    }
}

// - Premises

/// Encodes a premise, including variable and otherwise premises.
impl Encode for el::Prem {
    type Output = Value;

    fn encode_cached(&self, arena: &mut Arena, cache: &mut Cache) -> Result<Self::Output, Error> {
        let (shape, values_meta) = match &self.node {
            el::PremKind::Var(prem) => (
                m::PREM_VAR,
                vec![
                    prem.id.encode_cached(arena, cache)?,
                    prem.plain_typ.encode_cached(arena, cache)?,
                ],
            ),
            el::PremKind::Rule(prem) => (
                m::PREM_REL,
                vec![prem.id.encode_cached(arena, cache)?, prem.exp.encode_cached(arena, cache)?],
            ),
            el::PremKind::RuleNot(prem) => (
                m::PREM_RELNOT,
                vec![prem.id.encode_cached(arena, cache)?, prem.exp.encode_cached(arena, cache)?],
            ),
            el::PremKind::If(prem) => (m::PREM_IF, vec![prem.exp.encode_cached(arena, cache)?]),
            el::PremKind::Else => (m::PREM_ELSE, vec![]),
            el::PremKind::Iter(prem) => (
                m::PREM_ITER,
                vec![
                    prem.prem.as_ref().encode_cached(arena, cache)?,
                    prem.iter.encode_cached(arena, cache)?,
                ],
            ),
            el::PremKind::Debug(prem) => {
                (m::PREM_DEBUG, vec![prem.exp.encode_cached(arena, cache)?])
            }
        };
        case(arena, shape, "premEL", values_meta, self.span)
    }
}

// - Hints, rules, and table rows

/// Encodes a hint by its name and expression.
impl Encode for el::Hint {
    type Output = Value;

    fn encode_cached(&self, arena: &mut Arena, cache: &mut Cache) -> Result<Self::Output, Error> {
        let values_meta =
            vec![self.id.encode_cached(arena, cache)?, self.exp.encode_cached(arena, cache)?];
        case(arena, m::HINT, "hintEL", values_meta, self.id.span)
    }
}

/// Encodes a rule with both its relation and rule names.
impl Encode for el::Rule {
    type Output = Value;

    fn encode_cached(&self, arena: &mut Arena, cache: &mut Cache) -> Result<Self::Output, Error> {
        let values_meta = vec![
            self.node.id_rel.encode_cached(arena, cache)?,
            self.node.id_rule.encode_cached(arena, cache)?,
            self.node.exp.encode_cached(arena, cache)?,
            list(arena, cache, "premEL", &self.node.prems)?,
        ];
        case(arena, m::RUL, "rulEL", values_meta, self.span)
    }
}

/// Encodes a table row as its pattern and body.
impl Encode for el::TableRow {
    type Output = Value;

    fn encode_cached(&self, arena: &mut Arena, cache: &mut Cache) -> Result<Self::Output, Error> {
        let values_meta = vec![
            self.node.exp_pattern.encode_cached(arena, cache)?,
            self.node.exp_body.encode_cached(arena, cache)?,
        ];
        case(arena, m::TBL_ROW, "tblRowEL", values_meta, self.span)
    }
}

/// Encodes a forward-declared type name with its type parameters.
impl Encode for el::SyntaxDefEntry {
    type Output = Value;

    fn encode_cached(&self, arena: &mut Arena, cache: &mut Cache) -> Result<Self::Output, Error> {
        let values_meta = vec![
            self.id.encode_cached(arena, cache)?,
            list(arena, cache, "tparam", &self.tparams)?,
        ];
        case(arena, m::SYNTAX_ENTRY, "syntaxEntryEL", values_meta, self.id.span)
    }
}

// - Definitions

/// Keeps only the `input` hints, which elaboration reads.
fn hints_input(hints: &[el::Hint]) -> Vec<&el::Hint> {
    hints
        .iter()
        .filter(|hint| hint.id.node.as_ref() == "input")
        .collect()
}

/// Encodes a definition; only a separator returns None.
impl Encode for el::Def {
    type Output = Option<Value>;

    fn encode_cached(&self, arena: &mut Arena, cache: &mut Cache) -> Result<Self::Output, Error> {
        let (shape, values_meta) = match &self.node {
            el::DefKind::Sep => return Ok(None),
            el::DefKind::ExternSyntax(def) => {
                (m::DEF_EXTSYNTAX, vec![def.id.encode_cached(arena, cache)?])
            }
            el::DefKind::Syntax(def) => {
                (m::DEF_SYNTAX, vec![list(arena, cache, "syntaxEntryEL", &def.entries)?])
            }
            el::DefKind::Typ(def) => (
                m::DEF_TYP,
                vec![
                    def.id.encode_cached(arena, cache)?,
                    list(arena, cache, "tparam", &def.tparams)?,
                    def.def_typ.encode_cached(arena, cache)?,
                ],
            ),
            el::DefKind::Var(def) => (
                m::DEF_VAR,
                vec![
                    def.id.encode_cached(arena, cache)?,
                    def.plain_typ.encode_cached(arena, cache)?,
                ],
            ),
            el::DefKind::ExternRel(def) => (
                m::DEF_EXTREL,
                vec![
                    def.id.encode_cached(arena, cache)?,
                    def.not_typ.encode_cached(arena, cache)?,
                    list(arena, cache, "hintEL", &hints_input(&def.hints))?,
                ],
            ),
            el::DefKind::Rel(def) => (
                m::DEF_REL,
                vec![
                    def.id.encode_cached(arena, cache)?,
                    def.not_typ.encode_cached(arena, cache)?,
                    list(arena, cache, "hintEL", &hints_input(&def.hints))?,
                ],
            ),
            el::DefKind::RuleGroup(def) => (
                m::DEF_RULEGROUP,
                vec![
                    def.relid.encode_cached(arena, cache)?,
                    def.groupid.encode_cached(arena, cache)?,
                    list(arena, cache, "rulEL", &def.rules)?,
                ],
            ),
            el::DefKind::ExternDec(def) => (
                m::DEF_EXTDEC,
                vec![
                    def.id.encode_cached(arena, cache)?,
                    list(arena, cache, "tparam", &def.tparams)?,
                    list(arena, cache, "paramEL", &def.params)?,
                    def.plain_typ.encode_cached(arena, cache)?,
                ],
            ),
            el::DefKind::BuiltinDec(def) => (
                m::DEF_BUILTINDEC,
                vec![
                    def.id.encode_cached(arena, cache)?,
                    list(arena, cache, "tparam", &def.tparams)?,
                    list(arena, cache, "paramEL", &def.params)?,
                    def.plain_typ.encode_cached(arena, cache)?,
                ],
            ),
            el::DefKind::TableDec(def) => (
                m::DEF_TABLEDEC,
                vec![
                    def.id.encode_cached(arena, cache)?,
                    list(arena, cache, "paramEL", &def.params)?,
                    def.plain_typ.encode_cached(arena, cache)?,
                ],
            ),
            el::DefKind::FuncDec(def) => (
                m::DEF_DEC,
                vec![
                    def.id.encode_cached(arena, cache)?,
                    list(arena, cache, "tparam", &def.tparams)?,
                    list(arena, cache, "paramEL", &def.params)?,
                    def.plain_typ.encode_cached(arena, cache)?,
                ],
            ),
            el::DefKind::TableDef(def) => (
                m::DEF_TABLE,
                vec![
                    def.id.encode_cached(arena, cache)?,
                    list(arena, cache, "tblRowEL", &def.rows)?,
                ],
            ),
            el::DefKind::FuncDef(def) => (
                m::DEF_DEF,
                vec![
                    def.id.encode_cached(arena, cache)?,
                    list(arena, cache, "tparam", &def.tparams)?,
                    list(arena, cache, "argEL", &def.args)?,
                    def.exp.encode_cached(arena, cache)?,
                    list(arena, cache, "premEL", &def.prems)?,
                ],
            ),
        };
        Ok(Some(case(arena, shape, "defnEL", values_meta, self.span)?))
    }
}

// - Specifications

/// Encodes a whole EL specification, omitting only separators.
impl Encode for el::Spec {
    type Output = Value;

    fn encode_cached(&self, arena: &mut Arena, cache: &mut Cache) -> Result<Self::Output, Error> {
        // Preserve definition order while dropping separators
        let mut values_meta = Vec::with_capacity(self.len());
        for def_el in self {
            if let Some(value) = def_el.encode_cached(arena, cache)? {
                values_meta.push(value);
            }
        }
        Ok(make::list(
            arena,
            make_typ::var(phrase!(node: "specEL".into(), span: Span::default()), vec![])
                .node
                .into(),
            values_meta,
            Span::default(),
        )?)
    }
}
