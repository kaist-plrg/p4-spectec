//! Encodes AL syntax and runtime values as meta data
//!
//! Shared syntax is encoded by `meta::shared::encode`.
//! Scripts omit variable declarations and non-semantic metadata.

use super::mixops as m;
use crate::interface::meta::shared::{
    Cache, Error,
    encode::{Encode, case, list, opt},
};
use crate::lang::common::prim::num::Number;
use crate::{
    lang::{
        al::ast as al,
        common::source::Span,
        data::{
            arena::Arena,
            typ::make as make_typ,
            value::flat::{Value, ValueField, ValueKind, make},
        },
        hints::input,
    },
    phrase,
};

// == Encoding

// - Values

/// Encodes a runtime value's payload in the same arena.
impl Encode for Value {
    type Output = Value;

    fn encode_cached(&self, arena: &mut Arena, cache: &mut Cache) -> Result<Self::Output, Error> {
        let value_target = *self;
        // Reuse a successful conversion by payload identity
        let id_value = arena.value.canon_id(value_target.node);
        if let Some(value_meta) = cache.values_encode.get(&id_value) {
            return Ok(*value_meta);
        }
        let span = *arena.value.span(value_target.span);
        let value_meta = match arena.value.kind(value_target.node) {
            ValueKind::Bool(value_bool) => {
                let value_bool = *value_bool;
                let values_meta = vec![make::bool(arena, value_bool, span)?];
                case(arena, m::BOOL, "val", values_meta, span)
            }
            ValueKind::Num(num) => {
                let num_target = num.clone();
                let shape = match &num_target {
                    Number::Nat(_) => m::NAT,
                    Number::Int(_) => m::INT,
                };
                let values_meta = vec![make::num(arena, num_target, span)?];
                case(arena, shape, "val", values_meta, span)
            }
            ValueKind::Text(text) => {
                let text_target = text.clone();
                let values_meta = vec![make::text(arena, text_target, span)?];
                case(arena, m::TEXT, "val", values_meta, span)
            }
            ValueKind::Struct(value_fields) => {
                let value_fields = value_fields.clone();
                let values_meta = vec![list(arena, cache, "valField", &value_fields)?];
                case(arena, m::STR, "val", values_meta, span)
            }
            ValueKind::Case(value_case) => {
                let mixop_target = *value_case.mixop();
                let values_target = value_case.args().to_vec();
                let values_case = vec![
                    mixop_target.encode_cached(arena, cache)?,
                    list(arena, cache, "val", &values_target)?,
                ];
                let values_meta = vec![case(arena, m::VALCASE, "valCase", values_case, span)?];
                case(arena, m::INJ, "val", values_meta, span)
            }
            ValueKind::Tuple(values_target) => {
                let values_target = values_target.clone();
                let values_meta = vec![list(arena, cache, "val", &values_target)?];
                case(arena, m::TUP, "val", values_meta, span)
            }
            ValueKind::Opt(value_inner) => {
                let value_inner = *value_inner;
                let values_meta = vec![opt(arena, cache, "val", value_inner.as_ref())?];
                case(arena, m::OPT, "val", values_meta, span)
            }
            ValueKind::List(values_target) => {
                let values_target = values_target.clone();
                let values_meta = vec![list(arena, cache, "val", &values_target)?];
                case(arena, m::LIST, "val", values_meta, span)
            }
            ValueKind::Func(id_target) => {
                let id_target = id_target.clone();
                let values_meta = vec![id_target.encode_cached(arena, cache)?];
                case(arena, m::FUNC, "val", values_meta, span)
            }
            ValueKind::Extern(json) => {
                let json_target = json.clone();
                let value_json = make::external(
                    arena,
                    make_typ::var(phrase!(node: "json".into(), span: Span::default()), vec![])
                        .node
                        .into(),
                    json_target,
                    span,
                )?;
                case(arena, m::EXT, "val", vec![value_json], span)
            }
        }?;
        // Keep only this direction; decoding still reconstructs omitted notes
        cache.values_encode.insert(id_value, value_meta);
        Ok(value_meta)
    }
}

/// Encodes one runtime struct field.
impl Encode for ValueField {
    type Output = Value;

    fn encode_cached(&self, arena: &mut Arena, cache: &mut Cache) -> Result<Self::Output, Error> {
        let values_meta =
            vec![self.atom.encode_cached(arena, cache)?, self.value.encode_cached(arena, cache)?];
        case(arena, m::VALFIELD, "valField", values_meta, self.atom.span)
    }
}

// - Results

/// Encodes a successful function result without retaining conversions.
pub fn ok_value(arena: &mut Arena, value_target: Value) -> Result<Value, Error> {
    ok_value_cached(arena, &mut Cache::new(false), value_target)
}

/// Encodes successful relation outputs without retaining conversions.
pub fn ok_values(arena: &mut Arena, values_target: &[Value]) -> Result<Value, Error> {
    ok_values_cached(arena, &mut Cache::new(false), values_target)
}

/// Encodes a successful function result as `OK val : funcRes`.
pub fn ok_value_cached(
    arena: &mut Arena,
    cache: &mut Cache,
    value_target: Value,
) -> Result<Value, Error> {
    let span = *arena.value.span(value_target.span);
    let value_meta = value_target.encode_cached(arena, cache)?;
    case(arena, m::OK, "funcRes", vec![value_meta], span)
}

/// Encodes successful relation outputs as `OK val* : relRes`.
pub fn ok_values_cached(
    arena: &mut Arena,
    cache: &mut Cache,
    values_target: &[Value],
) -> Result<Value, Error> {
    let values_meta = list(arena, cache, "val", values_target)?;
    case(arena, m::OK, "relRes", vec![values_meta], Span::default())
}

// - Premises

/// Encodes an AL premise.
impl Encode for al::Prem {
    type Output = Value;

    fn encode_cached(&self, arena: &mut Arena, cache: &mut Cache) -> Result<Self::Output, Error> {
        let span = self.span;
        let (shape, values_meta) = match &self.node {
            al::PremKind::Rule(prem_rule_al) => {
                let (exps_input_al, exps_output_al) =
                    input::split(&prem_rule_al.input_hint, prem_rule_al.not_exp.args().to_vec())?;
                (
                    m::REL_PREM,
                    vec![
                        prem_rule_al.id.encode_cached(arena, cache)?,
                        list(arena, cache, "exp", &exps_input_al)?,
                        list(arena, cache, "exp", &exps_output_al)?,
                    ],
                )
            }
            al::PremKind::If(prem_inner_al) => {
                (m::IF, vec![prem_inner_al.exp.encode_cached(arena, cache)?])
            }
            al::PremKind::IfHold(prem_inner_al) => (
                m::IFHOLD,
                vec![
                    prem_inner_al.id.encode_cached(arena, cache)?,
                    list(arena, cache, "exp", prem_inner_al.not_exp.args())?,
                ],
            ),
            al::PremKind::IfNotHold(prem_inner_al) => (
                m::IFNOTHOLD,
                vec![
                    prem_inner_al.id.encode_cached(arena, cache)?,
                    list(arena, cache, "exp", prem_inner_al.not_exp.args())?,
                ],
            ),
            al::PremKind::Let(prem_inner_al) => (
                m::LET,
                vec![
                    prem_inner_al.exp_l.encode_cached(arena, cache)?,
                    prem_inner_al.exp_r.encode_cached(arena, cache)?,
                ],
            ),
            al::PremKind::Iter(prem_inner_al) => (
                m::ITER_PREM,
                vec![
                    prem_inner_al.prem.encode_cached(arena, cache)?,
                    prem_inner_al.prem_iter.encode_cached(arena, cache)?,
                ],
            ),
            al::PremKind::Debug(prem_inner_al) => {
                (m::DEBUG, vec![prem_inner_al.exp.encode_cached(arena, cache)?])
            }
        };
        case(arena, shape, "premAL", values_meta, span)
    }
}

// - Iteration premises

/// Encodes bound and bound-by-iteration variable lists in order.
impl Encode for al::PremIter {
    type Output = Value;

    fn encode_cached(&self, arena: &mut Arena, cache: &mut Cache) -> Result<Self::Output, Error> {
        let values_meta = vec![
            self.iter.encode_cached(arena, cache)?,
            list(arena, cache, "vari", &self.vars_bound)?,
            list(arena, cache, "vari", &self.vars_bind)?,
        ];
        case(arena, m::ITERPREM, "iterPremAL", values_meta, Span::default())
    }
}

// - Rules

/// Encodes a rule match without its printing signature.
impl Encode for al::RuleMatch {
    type Output = Value;

    fn encode_cached(&self, arena: &mut Arena, cache: &mut Cache) -> Result<Self::Output, Error> {
        let values_meta = vec![
            list(arena, cache, "exp", &self.exps_input)?,
            list(arena, cache, "premAL", &self.prems)?,
        ];
        case(arena, m::RULMATCH, "rulMatch", values_meta, Span::default())
    }
}

/// Encodes a rule path and its outputs.
impl Encode for al::RulePath {
    type Output = Value;

    fn encode_cached(&self, arena: &mut Arena, cache: &mut Cache) -> Result<Self::Output, Error> {
        let values_meta = vec![
            self.id.encode_cached(arena, cache)?,
            list(arena, cache, "exp", &self.exps_output)?,
            list(arena, cache, "premAL", &self.prems)?,
        ];
        case(arena, m::RULPATH, "rulPath", values_meta, self.id.span)
    }
}

/// Encodes one rule as its match followed by its path.
impl Encode for (&al::RuleMatch, &al::RulePath) {
    type Output = Value;

    fn encode_cached(&self, arena: &mut Arena, cache: &mut Cache) -> Result<Self::Output, Error> {
        let (rule_match, rule_path) = self;
        let values_meta =
            vec![rule_match.encode_cached(arena, cache)?, rule_path.encode_cached(arena, cache)?];
        case(arena, m::RUL, "rul", values_meta, rule_path.id.span)
    }
}

/// Encodes a rule group, pairing its shared match with each path in order.
impl Encode for al::RuleGroup {
    type Output = Value;

    fn encode_cached(&self, arena: &mut Arena, cache: &mut Cache) -> Result<Self::Output, Error> {
        let rules: Vec<_> = self
            .node
            .rule_paths
            .iter()
            .map(|rule_path| (&self.node.rule_match, rule_path))
            .collect();
        let values_meta =
            vec![self.node.id.encode_cached(arena, cache)?, list(arena, cache, "rul", &rules)?];
        case(arena, m::RULGROUP, "rulGroupAL", values_meta, self.span)
    }
}

/// Encodes an otherwise group with one rule.
impl Encode for al::ElseGroup {
    type Output = Value;

    fn encode_cached(&self, arena: &mut Arena, cache: &mut Cache) -> Result<Self::Output, Error> {
        let rule = (&self.node.rule_match, &self.node.rule_path);
        let values_meta =
            vec![self.node.id.encode_cached(arena, cache)?, rule.encode_cached(arena, cache)?];
        case(arena, m::ELSGROUP, "elsGroupAL", values_meta, self.span)
    }
}

// - Clauses

/// Encodes a function clause.
impl Encode for al::Clause {
    type Output = Value;

    fn encode_cached(&self, arena: &mut Arena, cache: &mut Cache) -> Result<Self::Output, Error> {
        let values_meta = vec![
            list(arena, cache, "arg", &self.node.args)?,
            self.node.exp.encode_cached(arena, cache)?,
            list(arena, cache, "premAL", &self.node.prems)?,
        ];
        case(arena, m::CLAUSE, "clauseAL", values_meta, self.span)
    }
}

/// Encodes an otherwise clause without its keyword annotation.
impl Encode for al::ElseClause {
    type Output = Value;

    fn encode_cached(&self, arena: &mut Arena, cache: &mut Cache) -> Result<Self::Output, Error> {
        let values_meta = vec![
            list(arena, cache, "arg", &self.node.clause.args)?,
            self.node.clause.exp.encode_cached(arena, cache)?,
            list(arena, cache, "premAL", &self.node.clause.prems)?,
        ];
        case(arena, m::CLAUSE, "clauseAL", values_meta, self.span)
    }
}

// - Table rows

/// Encodes a table row without its printing signature.
impl Encode for al::TableRow {
    type Output = Value;

    fn encode_cached(&self, arena: &mut Arena, cache: &mut Cache) -> Result<Self::Output, Error> {
        let values_meta = vec![
            list(arena, cache, "arg", &self.node.args)?,
            self.node.exp.encode_cached(arena, cache)?,
            list(arena, cache, "premAL", &self.node.prems)?,
        ];
        case(arena, m::CLAUSE, "tblRowAL", values_meta, self.span)
    }
}

// - Definitions

/// Encodes a definition; only a Var definition returns None.
impl Encode for al::Def {
    type Output = Option<Value>;

    fn encode_cached(&self, arena: &mut Arena, cache: &mut Cache) -> Result<Self::Output, Error> {
        let span = self.span;
        let (shape, values_meta) = match &self.node {
            al::DefKind::Var(_) => return Ok(None),
            al::DefKind::Typ(al::TypDef::Extern(typdef_al)) => {
                (m::EXTTYP, vec![typdef_al.id.encode_cached(arena, cache)?])
            }
            al::DefKind::Typ(al::TypDef::Defined(typdef_al)) => (
                m::TYP_DEF,
                vec![
                    typdef_al.id.encode_cached(arena, cache)?,
                    list(arena, cache, "id", &typdef_al.tparams)?,
                    typdef_al.def_typ.encode_cached(arena, cache)?,
                ],
            ),
            al::DefKind::Rel(al::RelDef::Extern(rel_al)) => {
                let (typs_input_al, typs_output_al) =
                    input::split(&rel_al.input_hint, rel_al.not_typ.node.args().to_vec())?;
                (
                    m::EXTREL,
                    vec![
                        rel_al.id.encode_cached(arena, cache)?,
                        list(arena, cache, "typ", &typs_input_al)?,
                        list(arena, cache, "typ", &typs_output_al)?,
                    ],
                )
            }
            al::DefKind::Rel(al::RelDef::Defined(rel_al)) => {
                let (typs_input_al, typs_output_al) =
                    input::split(&rel_al.input_hint, rel_al.not_typ.node.args().to_vec())?;
                (
                    m::REL_DEF,
                    vec![
                        rel_al.id.encode_cached(arena, cache)?,
                        list(arena, cache, "typ", &typs_input_al)?,
                        list(arena, cache, "typ", &typs_output_al)?,
                        list(arena, cache, "rulGroupAL", &rel_al.rule_groups)?,
                        opt(arena, cache, "elsGroupAL", rel_al.else_group.as_ref())?,
                    ],
                )
            }
            al::DefKind::MetaFunc(al::MetaFuncDef::Extern(func_al)) => (
                m::EXTFUNC,
                vec![
                    func_al.id.encode_cached(arena, cache)?,
                    list(arena, cache, "id", &func_al.tparams)?,
                    list(arena, cache, "param", &func_al.params)?,
                    func_al.typ.encode_cached(arena, cache)?,
                ],
            ),
            al::DefKind::MetaFunc(al::MetaFuncDef::Builtin(func_al)) => (
                m::BUILTINFUNC,
                vec![
                    func_al.id.encode_cached(arena, cache)?,
                    list(arena, cache, "id", &func_al.tparams)?,
                    list(arena, cache, "param", &func_al.params)?,
                    func_al.typ.encode_cached(arena, cache)?,
                ],
            ),
            al::DefKind::MetaFunc(al::MetaFuncDef::Table(func_al)) => (
                m::TABLEFUNC,
                vec![
                    func_al.id.encode_cached(arena, cache)?,
                    list(arena, cache, "param", &func_al.params)?,
                    func_al.typ.encode_cached(arena, cache)?,
                    list(arena, cache, "tblRowAL", &func_al.table_rows)?,
                ],
            ),
            al::DefKind::MetaFunc(al::MetaFuncDef::Defined(func_al)) => (
                m::FUNC_DEF,
                vec![
                    func_al.id.encode_cached(arena, cache)?,
                    list(arena, cache, "id", &func_al.tparams)?,
                    list(arena, cache, "param", &func_al.params)?,
                    func_al.typ.encode_cached(arena, cache)?,
                    list(arena, cache, "clauseAL", &func_al.clauses)?,
                    opt(arena, cache, "elsClauseAL", func_al.else_clause.as_ref())?,
                ],
            ),
        };
        Ok(Some(case(arena, shape, "defnAL", values_meta, span)?))
    }
}

// - Specifications

/// Encodes a whole source AL script, omitting only variable definitions.
impl Encode for al::Spec {
    type Output = Value;

    fn encode_cached(&self, arena: &mut Arena, cache: &mut Cache) -> Result<Self::Output, Error> {
        let spec_al = self;
        // Preserve definition order while filtering the explicitly omitted form
        let mut values_meta = Vec::with_capacity(spec_al.len());
        for def_al in spec_al {
            if let Some(value) = def_al.encode_cached(arena, cache)? {
                values_meta.push(value);
            }
        }
        Ok(make::list(
            arena,
            make_typ::var(phrase!(node: "specAL".into(), span: Span::default()), vec![])
                .node
                .into(),
            values_meta,
            Span::default(),
        )?)
    }
}
