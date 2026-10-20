//! AL specifications and P4 programs as KAST JSON version 4, for `spec-meta-k/`
//!
//! A specification is a `Script` term and a P4 program a `someP4(val)` term
//! of the frozen `1-syntax.k`; `ToTerm` gives each node of the AL syntax its
//! term. A `FUNC` type becomes `funcT()` without its signature, the type
//! arguments of `VAR` and `CALL` are `TArgList`s, and `var` definitions,
//! signatures, hints and operand types are omitted.

use std::{
    borrow::Cow,
    fmt::{self, Write},
};

use serde::Serialize;

use crate::{
    lang::{
        al::ast as al,
        common::{
            Id, Iter,
            prim::{
                bool as boolean,
                num::{self, Number},
            },
        },
        data::{
            arena::Arena,
            notation::AtomPhrase,
            typ::{Typ, TypKind},
            value::flat::{Value, ValueKind},
        },
        hints::input::InputHint,
        il::ast as il,
        traits::print::Print,
    },
    util::json::json,
};

use super::{Emit, atom_rows, atom_rows_flat, split_inputs};

/// The KAST JSON format version this module emits.
pub const VERSION: u64 = 4;

// == KAST JSON nodes

/// A KAST term: a constructor application or a token.
#[derive(Serialize)]
#[serde(tag = "node")]
enum Term {
    KApply { label: Name, arity: usize, args: Vec<Term> },
    KToken { sort: Name, token: String },
}

/// A label or a sort.
#[derive(Serialize)]
struct Name {
    node: &'static str,
    name: Cow<'static, str>,
    params: [(); 0],
}

impl Name {
    fn label(name: impl Into<Cow<'static, str>>) -> Self {
        Self { node: "KLabel", name: name.into(), params: [] }
    }

    fn sort(name: &'static str) -> Self {
        Self { node: "KSort", name: name.into(), params: [] }
    }
}

impl Term {
    fn apply(label: &'static str, args: impl IntoIterator<Item = Term>) -> Self {
        let args: Vec<Term> = args.into_iter().collect();
        Self::KApply { label: Name::label(label), arity: args.len(), args }
    }

    fn token(sort: &'static str, token: String) -> Self {
        Self::KToken { sort: Name::sort(sort), token }
    }

    /// A cons list of `symbol`, ending in its terminator `.symbol`.
    fn list(symbol: &'static str, items: impl IntoIterator<Item = Term>) -> Self {
        let terminator =
            Self::KApply { label: Name::label(format!(".{symbol}")), arity: 0, args: vec![] };
        let items: Vec<Term> = items.into_iter().collect();
        items
            .into_iter()
            .rev()
            .fold(terminator, |tail, item| Self::apply(symbol, [item, tail]))
    }

    fn opt(none: &'static str, some: &'static str, item: Option<Term>) -> Self {
        match item {
            None => Self::apply(none, []),
            Some(item) => Self::apply(some, [item]),
        }
    }
}

/// A KAST document: the envelope `krun` reads.
#[derive(Serialize)]
pub struct Document {
    format: &'static str,
    version: u64,
    term: Term,
}

impl Document {
    fn new(term: Term) -> Self {
        Self { format: "KAST", version: VERSION, term }
    }
}

impl fmt::Display for Document {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt.write_str(&serde_json::to_string(self).map_err(|_| fmt::Error)?)
    }
}

// == Tokens

fn bool_token(value: bool) -> Term {
    Term::token("Bool", value.to_string())
}

fn int_token(digits: String) -> Term {
    Term::token("Int", digits)
}

/// Text in K's string literal syntax.
struct KString<'a>(&'a str);

impl fmt::Display for KString<'_> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt.write_char('"')?;
        for ch in self.0.chars() {
            match ch {
                '"' => fmt.write_str("\\\"")?,
                '\\' => fmt.write_str("\\\\")?,
                '\n' => fmt.write_str("\\n")?,
                '\t' => fmt.write_str("\\t")?,
                '\r' => fmt.write_str("\\r")?,
                '\x0c' => fmt.write_str("\\f")?,
                ch if (ch as u32) < 0x20 || ch == '\x7f' => write!(fmt, "\\x{:02x}", ch as u32)?,
                ch if ch.is_ascii() => fmt.write_char(ch)?,
                ch if (ch as u32) <= 0xffff => write!(fmt, "\\u{:04x}", ch as u32)?,
                ch => write!(fmt, "\\U{:08x}", ch as u32)?,
            }
        }
        fmt.write_char('"')
    }
}

fn string_token(text: &str) -> Term {
    Term::token("String", KString(text).to_string())
}

// == Extern JSON as K `JSON` terms

fn json_term(value: &json) -> Term {
    match value {
        json::Null => Term::apply("JSONnull", []),
        json::Bool(value) => bool_token(*value),
        json::Number(number) if number.is_f64() => Term::token("Float", number.to_string()),
        json::Number(number) => int_token(number.to_string()),
        json::String(text) => string_token(text),
        json::Array(values) => {
            Term::apply("JSONList", [Term::list("JSONs", values.iter().map(json_term))])
        }
        json::Object(fields) => {
            let entries = fields.iter().map(|(key, value)| {
                Term::apply("JSONEntry", [string_token(key), json_term(value)])
            });
            Term::apply("JSONObject", [Term::list("JSONs", entries)])
        }
    }
}

// == Terms of the AL syntax

/// The KAST term of a node of the AL syntax.
trait ToTerm {
    fn term(&self) -> Term;
}

/// A `List{_}` of `symbol` over the terms of `items`.
fn list<'a, T: ToTerm + 'a>(symbol: &'static str, items: impl IntoIterator<Item = &'a T>) -> Term {
    Term::list(symbol, items.into_iter().map(ToTerm::term))
}

// - Identifiers, atoms, and notation

impl ToTerm for Id {
    fn term(&self) -> Term {
        string_token(&self.node)
    }
}

impl ToTerm for AtomPhrase {
    fn term(&self) -> Term {
        string_token(&Print::to_string(&self.node))
    }
}

/// The `mixop` of the atom rows between argument positions.
fn mixop(rows: Vec<Vec<&AtomPhrase>>) -> Term {
    Term::list("mixop", rows.into_iter().map(|row| list("atomList", row)))
}

impl ToTerm for Iter {
    fn term(&self) -> Term {
        match self {
            Iter::Opt => Term::apply("quest", []),
            Iter::List => Term::apply("star", []),
        }
    }
}

// - Types

impl ToTerm for Typ {
    fn term(&self) -> Term {
        match &self.node {
            TypKind::Bool => Term::apply("boolT", []),
            TypKind::Num(num::Typ::Nat) => Term::apply("natT", []),
            TypKind::Num(num::Typ::Int) => Term::apply("intT", []),
            TypKind::Text => Term::apply("textT", []),
            TypKind::Var(id, targs) => Term::apply("varT", [id.term(), list("targList", targs)]),
            TypKind::Tuple(typs) => Term::apply("tupT", [list("typList", typs)]),
            TypKind::Iter(typ, iter) => Term::apply("iterT", [typ.term(), iter.term()]),
            // The frozen K syntax has `funcT()` without its signature
            TypKind::Func(_) => Term::apply("funcT", []),
        }
    }
}

impl ToTerm for il::Var {
    fn term(&self) -> Term {
        Term::apply("vari", [self.id.term(), self.typ.term(), list("iterList", &self.iters)])
    }
}

impl ToTerm for il::DefTyp {
    fn term(&self) -> Term {
        match &self.node {
            il::DefTypKind::Plain(typ) => Term::apply("aliasDT", [typ.term()]),
            il::DefTypKind::Struct(fields) => {
                Term::apply("structDT", [list("typFieldList", fields)])
            }
            il::DefTypKind::Variant(cases) => {
                Term::apply("variantDT", [list("typCaseList", cases)])
            }
        }
    }
}

impl ToTerm for il::TypField {
    fn term(&self) -> Term {
        Term::apply("typField", [self.atom.term(), self.typ.term()])
    }
}

impl ToTerm for il::TypCase {
    fn term(&self) -> Term {
        let not_typ = &self.not_typ.node;
        let rows = atom_rows(not_typ.mixop());
        Term::apply("typCase", [mixop(rows), list("typList", not_typ.args())])
    }
}

// - Operators and numbers

impl ToTerm for il::UnOp {
    fn term(&self) -> Term {
        let label = match self {
            il::UnOp::Bool(boolean::UnOp::Not) => "notOp",
            il::UnOp::Num(num::UnOp::Plus) => "plusOp",
            il::UnOp::Num(num::UnOp::Minus) => "minusOp",
        };
        Term::apply(label, [])
    }
}

impl ToTerm for il::BinOp {
    fn term(&self) -> Term {
        let label = match self {
            il::BinOp::Bool(boolean::BinOp::And) => "andOp",
            il::BinOp::Bool(boolean::BinOp::Or) => "orOp",
            il::BinOp::Bool(boolean::BinOp::Impl) => "implOp",
            il::BinOp::Bool(boolean::BinOp::Equiv) => "equivOp",
            il::BinOp::Num(num::BinOp::Add) => "addOp",
            il::BinOp::Num(num::BinOp::Sub) => "subOp",
            il::BinOp::Num(num::BinOp::Mul) => "mulOp",
            il::BinOp::Num(num::BinOp::Div) => "divOp",
            il::BinOp::Num(num::BinOp::Mod) => "modOp",
            il::BinOp::Num(num::BinOp::Pow) => "powOp",
        };
        Term::apply(label, [])
    }
}

impl ToTerm for il::CmpOp {
    fn term(&self) -> Term {
        let label = match self {
            il::CmpOp::Bool(boolean::CmpOp::Eq) => "eqOp",
            il::CmpOp::Bool(boolean::CmpOp::Ne) => "neOp",
            il::CmpOp::Num(num::CmpOp::Lt) => "ltOp",
            il::CmpOp::Num(num::CmpOp::Le) => "leOp",
            il::CmpOp::Num(num::CmpOp::Gt) => "gtOp",
            il::CmpOp::Num(num::CmpOp::Ge) => "geOp",
        };
        Term::apply(label, [])
    }
}

/// A number of an expression or a value.
impl ToTerm for Number {
    fn term(&self) -> Term {
        match self {
            Number::Nat(nat) => Term::apply("natN", [int_token(nat.to_string())]),
            Number::Int(int) => Term::apply("intN", [int_token(int.to_string())]),
        }
    }
}

// - Expressions

impl ToTerm for il::Exp {
    fn term(&self) -> Term {
        match &self.node {
            il::ExpKind::Bool(value) => Term::apply("boolE", [bool_token(*value)]),
            il::ExpKind::Num(num) => num.term(),
            il::ExpKind::Text(text) => Term::apply("textE", [string_token(text)]),
            il::ExpKind::Id(id) => Term::apply("varE", [id.term()]),
            il::ExpKind::Un(op, _, exp) => Term::apply("unE", [op.term(), exp.term()]),
            il::ExpKind::Bin(op, _, exp_l, exp_r) => {
                Term::apply("binE", [op.term(), exp_l.term(), exp_r.term()])
            }
            il::ExpKind::Cmp(op, _, exp_l, exp_r) => {
                Term::apply("cmpE", [op.term(), exp_l.term(), exp_r.term()])
            }
            il::ExpKind::UpCast(typ, exp) => Term::apply("upCastE", [typ.term(), exp.term()]),
            il::ExpKind::DownCast(typ, exp) => Term::apply("downCastE", [typ.term(), exp.term()]),
            il::ExpKind::Sub(exp, typ) => Term::apply("subE", [exp.term(), typ.term()]),
            il::ExpKind::Match(exp, pattern) => Term::apply("matchE", [exp.term(), pattern.term()]),
            il::ExpKind::Tuple(exps) => Term::apply("tupE", [list("expList", exps)]),
            il::ExpKind::Case(not_exp) => {
                let rows = atom_rows(not_exp.mixop());
                let case = Term::apply("expCase", [mixop(rows), list("expList", not_exp.args())]);
                Term::apply("injE", [case])
            }
            il::ExpKind::Str(fields) => Term::apply("strE", [list("expFieldList", fields)]),
            il::ExpKind::Opt(exp) => {
                let exp = exp.as_deref().map(ToTerm::term);
                Term::apply("optE", [Term::opt("noExp", "someExp", exp)])
            }
            il::ExpKind::List(exps) => Term::apply("listE", [list("expList", exps)]),
            il::ExpKind::Cons(exp_h, exp_t) => Term::apply("consE", [exp_h.term(), exp_t.term()]),
            il::ExpKind::Cat(exp_l, exp_r) => Term::apply("catE", [exp_l.term(), exp_r.term()]),
            il::ExpKind::Mem(exp_e, exp_s) => Term::apply("memE", [exp_e.term(), exp_s.term()]),
            il::ExpKind::Len(exp) => Term::apply("lenE", [exp.term()]),
            il::ExpKind::Dot(exp, atom) => Term::apply("dotE", [exp.term(), atom.term()]),
            il::ExpKind::Idx(exp_b, exp_i) => Term::apply("idxE", [exp_b.term(), exp_i.term()]),
            il::ExpKind::Slice(exp_b, exp_i, exp_n) => {
                Term::apply("sliceE", [exp_b.term(), exp_i.term(), exp_n.term()])
            }
            il::ExpKind::Upd(exp_b, path, exp_n) => {
                Term::apply("updE", [exp_b.term(), path.term(), exp_n.term()])
            }
            il::ExpKind::Call(id, targs, args) => {
                Term::apply("callE", [id.term(), list("targList", targs), list("argList", args)])
            }
            il::ExpKind::Iter(exp, exp_iter) => {
                let iter_exp = Term::apply(
                    "iterExp",
                    [exp_iter.iter.term(), list("variList", &exp_iter.vars)],
                );
                Term::apply("iterE", [exp.term(), iter_exp])
            }
        }
    }
}

impl ToTerm for il::ExpField {
    fn term(&self) -> Term {
        Term::apply("expField", [self.atom.term(), self.exp.term()])
    }
}

impl ToTerm for il::Pattern {
    fn term(&self) -> Term {
        match self {
            il::Pattern::Case(mixop_il) => Term::apply("injPat", [mixop(atom_rows(mixop_il))]),
            il::Pattern::List(il::ListPattern::Cons) => Term::apply("consPat", []),
            il::Pattern::List(il::ListPattern::Fixed(len)) => {
                Term::apply("fixedPat", [int_token(len.to_string())])
            }
            il::Pattern::List(il::ListPattern::Nil) => Term::apply("nilPat", []),
            il::Pattern::Opt(il::OptPattern::Some) => Term::apply("somePat", []),
            il::Pattern::Opt(il::OptPattern::None) => Term::apply("nonePat", []),
        }
    }
}

impl ToTerm for il::Path {
    fn term(&self) -> Term {
        match &self.node {
            il::PathKind::Root => Term::apply("rootPath", []),
            il::PathKind::Idx(path, exp_i) => Term::apply("idxPath", [path.term(), exp_i.term()]),
            il::PathKind::Slice(path, exp_i, exp_n) => {
                Term::apply("slicePath", [path.term(), exp_i.term(), exp_n.term()])
            }
            il::PathKind::Dot(path, atom) => Term::apply("dotPath", [path.term(), atom.term()]),
        }
    }
}

impl ToTerm for il::Arg {
    fn term(&self) -> Term {
        match &self.node {
            il::ArgKind::Exp(exp) => Term::apply("expA", [exp.term()]),
            il::ArgKind::Def(id) => Term::apply("funA", [id.term()]),
        }
    }
}

impl ToTerm for il::Param {
    fn term(&self) -> Term {
        match &self.node {
            il::ParamKind::Exp(typ) => Term::apply("expParam", [typ.term()]),
            il::ParamKind::Def(id, tparams, params, typ) => {
                Term::apply("funParam", signature(id, tparams, params, typ))
            }
        }
    }
}

/// The identifier, type parameters, parameters and result type of a function.
fn signature(id: &Id, tparams: &[Id], params: &[il::Param], typ: &Typ) -> [Term; 4] {
    [id.term(), list("tparamList", tparams), list("paramList", params), typ.term()]
}

// - Premises

impl ToTerm for al::Prem {
    fn term(&self) -> Term {
        match &self.node {
            al::PremKind::Rule(prem) => {
                let (exps_in, exps_out) = split_inputs(&prem.input_hint, prem.not_exp.args());
                Term::apply(
                    "relPr",
                    [prem.id.term(), list("expList", exps_in), list("expList", exps_out)],
                )
            }
            al::PremKind::If(prem) => Term::apply("ifPr", [prem.exp.term()]),
            al::PremKind::IfHold(prem) => {
                Term::apply("ifHoldPr", [prem.id.term(), list("expList", prem.not_exp.args())])
            }
            al::PremKind::IfNotHold(prem) => {
                Term::apply("ifNotHoldPr", [prem.id.term(), list("expList", prem.not_exp.args())])
            }
            al::PremKind::Let(prem) => Term::apply("letPr", [prem.exp_l.term(), prem.exp_r.term()]),
            al::PremKind::Iter(prem) => {
                let iter_prem = &prem.prem_iter;
                let iter_prem = Term::apply(
                    "iterPrem",
                    [
                        iter_prem.iter.term(),
                        list("variList", &iter_prem.vars_bound),
                        list("variList", &iter_prem.vars_bind),
                    ],
                );
                Term::apply("iterPr", [prem.prem.term(), iter_prem])
            }
            al::PremKind::Debug(prem) => Term::apply("debugPr", [prem.exp.term()]),
        }
    }
}

// - Rules, clauses, and table rows

impl ToTerm for al::RuleMatch {
    fn term(&self) -> Term {
        Term::apply("rulMatch", [list("expList", &self.exps_input), list("premList", &self.prems)])
    }
}

/// Outputs precede premises, unlike the field order.
impl ToTerm for al::RulePath {
    fn term(&self) -> Term {
        Term::apply(
            "rulPath",
            [self.id.term(), list("expList", &self.exps_output), list("premList", &self.prems)],
        )
    }
}

impl ToTerm for al::RuleGroup {
    fn term(&self) -> Term {
        let group = &self.node;
        Term::apply(
            "rulGroup",
            [group.id.term(), group.rule_match.term(), list("rulPathList", &group.rule_paths)],
        )
    }
}

impl ToTerm for al::ElseGroup {
    fn term(&self) -> Term {
        let group = &self.node;
        Term::apply("elsGroup", [group.id.term(), group.rule_match.term(), group.rule_path.term()])
    }
}

impl ToTerm for al::ClauseKind {
    fn term(&self) -> Term {
        Term::apply(
            "clause",
            [list("argList", &self.args), self.exp.term(), list("premList", &self.prems)],
        )
    }
}

impl ToTerm for al::Clause {
    fn term(&self) -> Term {
        self.node.term()
    }
}

impl ToTerm for al::ElseClause {
    fn term(&self) -> Term {
        self.node.clause.term()
    }
}

impl ToTerm for al::TableRow {
    fn term(&self) -> Term {
        let row = &self.node;
        Term::apply(
            "tblRow",
            [list("argList", &row.args), row.exp.term(), list("premList", &row.prems)],
        )
    }
}

// - Definitions

/// The input and output types of a relation's notation.
fn rel_typs(hint: &InputHint, not_typ: &il::NotTyp) -> [Term; 2] {
    let (typs_in, typs_out) = split_inputs(hint, not_typ.node.args());
    [list("typList", typs_in), list("typList", typs_out)]
}

/// The term of a definition; `var` definitions have none.
fn def(def: &al::Def) -> Option<Term> {
    Some(match &def.node {
        al::DefKind::Var(_) => return None,
        al::DefKind::Typ(al::TypDef::Extern(typdef)) => Term::apply("extTypD", [typdef.id.term()]),
        al::DefKind::Typ(al::TypDef::Defined(typdef)) => Term::apply(
            "typD",
            [typdef.id.term(), list("tparamList", &typdef.tparams), typdef.def_typ.term()],
        ),
        al::DefKind::Rel(al::RelDef::Extern(rel)) => {
            let [typs_in, typs_out] = rel_typs(&rel.input_hint, &rel.not_typ);
            Term::apply("extRelD", [rel.id.term(), typs_in, typs_out])
        }
        al::DefKind::Rel(al::RelDef::Defined(rel)) => {
            let [typs_in, typs_out] = rel_typs(&rel.input_hint, &rel.not_typ);
            let rule_groups = list("rulGroupList", &rel.rule_groups);
            let else_group = rel.else_group.as_ref().map(ToTerm::term);
            let else_group = Term::opt("noElsGroup", "someElsGroup", else_group);
            Term::apply("relD", [rel.id.term(), typs_in, typs_out, rule_groups, else_group])
        }
        al::DefKind::MetaFunc(al::MetaFuncDef::Extern(func)) => {
            Term::apply("extFuncD", signature(&func.id, &func.tparams, &func.params, &func.typ))
        }
        al::DefKind::MetaFunc(al::MetaFuncDef::Builtin(func)) => {
            Term::apply("builtinFuncD", signature(&func.id, &func.tparams, &func.params, &func.typ))
        }
        al::DefKind::MetaFunc(al::MetaFuncDef::Table(func)) => Term::apply(
            "tableFuncD",
            [
                func.id.term(),
                list("paramList", &func.params),
                func.typ.term(),
                list("tblRowList", &func.table_rows),
            ],
        ),
        al::DefKind::MetaFunc(al::MetaFuncDef::Defined(func)) => {
            let clauses = list("clauseList", &func.clauses);
            let else_clause = func.else_clause.as_ref().map(ToTerm::term);
            let else_clause = Term::opt("noElsClause", "someElsClause", else_clause);
            let signature = signature(&func.id, &func.tparams, &func.params, &func.typ);
            Term::apply("funcD", signature.into_iter().chain([clauses, else_clause]))
        }
    })
}

// == Values

fn value(arena: &Arena, value: Value) -> Term {
    match arena.value.kind(value.node) {
        ValueKind::Bool(value) => Term::apply("boolV", [bool_token(*value)]),
        ValueKind::Num(num) => num.term(),
        ValueKind::Text(text) => Term::apply("textV", [string_token(text)]),
        ValueKind::Struct(fields) => {
            let fields = fields.iter().map(|field| {
                Term::apply("valField", [field.atom.term(), self::value(arena, field.value)])
            });
            Term::apply("strV", [Term::list("valFieldList", fields)])
        }
        ValueKind::Case(case) => {
            let rows = atom_rows_flat(arena, *case.mixop());
            let case = Term::apply("valCase", [mixop(rows), values(arena, case.args())]);
            Term::apply("injV", [case])
        }
        ValueKind::Tuple(items) => Term::apply("tupV", [values(arena, items)]),
        ValueKind::Opt(item) => {
            let item = item.map(|item| self::value(arena, item));
            Term::apply("optV", [Term::opt("noVal", "someVal", item)])
        }
        ValueKind::List(items) => Term::apply("listV", [values(arena, items)]),
        ValueKind::Func(id) => Term::apply("funcV", [id.term()]),
        ValueKind::Extern(json) => Term::apply("extV", [json_term(json)]),
    }
}

fn values(arena: &Arena, values: &[Value]) -> Term {
    Term::list("valList", values.iter().map(|item| value(arena, *item)))
}

// == The emitter

/// K: KAST documents of sort `Script` and `P4Opt`.
pub struct K;

impl Emit for K {
    type Script = Document;
    type Program = Document;

    fn script(spec: &al::Spec) -> Document {
        Document::new(Term::list("script", spec.iter().filter_map(def)))
    }

    /// The `val` wrapped as `someP4(val)`.
    fn program(arena: &Arena, value: Value) -> Document {
        Document::new(Term::apply("someP4", [self::value(arena, value)]))
    }
}
