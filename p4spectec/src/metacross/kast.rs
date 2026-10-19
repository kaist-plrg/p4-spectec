//! AL specifications and P4 programs as KAST JSON version 4, for `spec-meta-k/`
//!
//! A specification is a `Script` term and a P4 program a `someP4(val)` term
//! of the frozen `1-syntax.k`. A `FUNC` type becomes `funcT()` without its
//! signature, the type arguments of `VAR` and `CALL` are `TArgList`s, and
//! `var` definitions, signatures, hints and operand types are omitted.

use std::fmt::{self, Write};

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
        il::ast as il,
        traits::print::Print,
    },
    util::json::json,
};

use super::{Emit, Result, atom_rows, atom_rows_flat, split_inputs};

/// The KAST JSON format version this module emits.
pub const VERSION: u64 = 4;

// == KAST JSON nodes

/// A KAST term: a constructor application or a token.
#[derive(Serialize)]
#[serde(tag = "node")]
enum Term {
    KApply { label: Label, arity: usize, args: Vec<Term> },
    KToken { sort: Sort, token: String },
}

#[derive(Serialize)]
struct Label {
    node: &'static str,
    name: &'static str,
    params: [(); 0],
}

#[derive(Serialize)]
struct Sort {
    node: &'static str,
    name: &'static str,
    params: [(); 0],
}

impl Term {
    fn apply(label: &'static str, args: Vec<Term>) -> Self {
        let label = Label { node: "KLabel", name: label, params: [] };
        Self::KApply { label, arity: args.len(), args }
    }

    fn token(sort: &'static str, token: String) -> Self {
        Self::KToken { sort: Sort { node: "KSort", name: sort, params: [] }, token }
    }

    /// A cons list ending in its terminator.
    fn list(symbol: &'static str, terminator: &'static str, items: Vec<Term>) -> Self {
        items
            .into_iter()
            .rev()
            .fold(Self::apply(terminator, vec![]), |tail, item| {
                Self::apply(symbol, vec![item, tail])
            })
    }

    fn opt(none: &'static str, some: &'static str, item: Option<Term>) -> Self {
        match item {
            None => Self::apply(none, vec![]),
            Some(item) => Self::apply(some, vec![item]),
        }
    }
}

/// A `List{_}` of the terms of `items`.
fn list<T>(
    symbol: &'static str,
    terminator: &'static str,
    items: &[T],
    term: impl Fn(&T) -> Term,
) -> Term {
    Term::list(symbol, terminator, items.iter().map(term).collect())
}

/// A `List{_}` of the terms of `items`, where a term can fail.
fn try_list<T>(
    symbol: &'static str,
    terminator: &'static str,
    items: &[T],
    term: impl Fn(&T) -> Result<Term>,
) -> Result<Term> {
    Ok(Term::list(symbol, terminator, items.iter().map(term).collect::<Result<_>>()?))
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
        json::Null => Term::apply("JSONnull", vec![]),
        json::Bool(value) => bool_token(*value),
        json::Number(number) if number.is_f64() => Term::token("Float", number.to_string()),
        json::Number(number) => int_token(number.to_string()),
        json::String(text) => string_token(text),
        json::Array(values) => {
            let items = values.iter().map(json_term).collect();
            Term::apply("JSONList", vec![Term::list("JSONs", ".JSONs", items)])
        }
        json::Object(fields) => {
            let entries = fields
                .iter()
                .map(|(key, value)| {
                    Term::apply("JSONEntry", vec![string_token(key), json_term(value)])
                })
                .collect();
            Term::apply("JSONObject", vec![Term::list("JSONs", ".JSONs", entries)])
        }
    }
}

// == Identifiers, atoms, and notation

fn id(id_al: &Id) -> Term {
    string_token(&id_al.node)
}

fn atom(atom_al: &AtomPhrase) -> Term {
    string_token(&Print::to_string(&atom_al.node))
}

fn mixop(rows: &[Vec<&AtomPhrase>]) -> Term {
    list("mixop", ".mixop", rows, |row| list("atomList", ".atomList", row, |atom_al| atom(atom_al)))
}

fn iter(iter_al: &Iter) -> Term {
    let label = match iter_al {
        Iter::Opt => "quest",
        Iter::List => "star",
    };
    Term::apply(label, vec![])
}

// == Types

fn typ(typ_al: &Typ) -> Term {
    match &typ_al.node {
        TypKind::Bool => Term::apply("boolT", vec![]),
        TypKind::Num(num::Typ::Nat) => Term::apply("natT", vec![]),
        TypKind::Num(num::Typ::Int) => Term::apply("intT", vec![]),
        TypKind::Text => Term::apply("textT", vec![]),
        TypKind::Var(id_al, targs_al) => Term::apply("varT", vec![id(id_al), targs(targs_al)]),
        TypKind::Tuple(typs_al) => Term::apply("tupT", vec![typs(typs_al)]),
        TypKind::Iter(typ_al, iter_al) => Term::apply("iterT", vec![typ(typ_al), iter(iter_al)]),
        // The frozen K syntax has `funcT()` without its signature
        TypKind::Func(_) => Term::apply("funcT", vec![]),
    }
}

fn typs(typs_al: &[Typ]) -> Term {
    list("typList", ".typList", typs_al, typ)
}

fn targs(targs_al: &[Typ]) -> Term {
    list("targList", ".targList", targs_al, typ)
}

fn tparams(tparams_al: &[Id]) -> Term {
    list("tparamList", ".tparamList", tparams_al, id)
}

fn vari(var_al: &il::Var) -> Term {
    let iters = list("iterList", ".iterList", &var_al.iters, iter);
    Term::apply("vari", vec![id(&var_al.id), typ(&var_al.typ), iters])
}

fn varis(vars_al: &[il::Var]) -> Term {
    list("variList", ".variList", vars_al, vari)
}

fn deftyp(deftyp_al: &il::DefTyp) -> Term {
    match &deftyp_al.node {
        il::DefTypKind::Plain(typ_al) => Term::apply("aliasDT", vec![typ(typ_al)]),
        il::DefTypKind::Struct(fields_al) => {
            let fields = list("typFieldList", ".typFieldList", fields_al, |field_al| {
                Term::apply("typField", vec![atom(&field_al.atom), typ(&field_al.typ)])
            });
            Term::apply("structDT", vec![fields])
        }
        il::DefTypKind::Variant(cases_al) => {
            let cases = list("typCaseList", ".typCaseList", cases_al, |case_al| {
                let not_typ = &case_al.not_typ.node;
                let rows = atom_rows(not_typ.mixop());
                Term::apply("typCase", vec![mixop(&rows), typs(not_typ.args())])
            });
            Term::apply("variantDT", vec![cases])
        }
    }
}

// == Operators and numbers

fn unop(op: &il::UnOp) -> Term {
    let label = match op {
        il::UnOp::Bool(boolean::UnOp::Not) => "notOp",
        il::UnOp::Num(num::UnOp::Plus) => "plusOp",
        il::UnOp::Num(num::UnOp::Minus) => "minusOp",
    };
    Term::apply(label, vec![])
}

fn binop(op: &il::BinOp) -> Term {
    let label = match op {
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
    Term::apply(label, vec![])
}

fn cmpop(op: &il::CmpOp) -> Term {
    let label = match op {
        il::CmpOp::Bool(boolean::CmpOp::Eq) => "eqOp",
        il::CmpOp::Bool(boolean::CmpOp::Ne) => "neOp",
        il::CmpOp::Num(num::CmpOp::Lt) => "ltOp",
        il::CmpOp::Num(num::CmpOp::Le) => "leOp",
        il::CmpOp::Num(num::CmpOp::Gt) => "gtOp",
        il::CmpOp::Num(num::CmpOp::Ge) => "geOp",
    };
    Term::apply(label, vec![])
}

/// A number of an expression or a value.
fn number(num: &Number) -> Term {
    match num {
        Number::Nat(nat) => Term::apply("natN", vec![int_token(nat.to_string())]),
        Number::Int(int) => Term::apply("intN", vec![int_token(int.to_string())]),
    }
}

// == Expressions

fn exp(exp_al: &il::Exp) -> Term {
    match &exp_al.node {
        il::ExpKind::Bool(value) => Term::apply("boolE", vec![bool_token(*value)]),
        il::ExpKind::Num(num) => number(num),
        il::ExpKind::Text(text) => Term::apply("textE", vec![string_token(text)]),
        il::ExpKind::Id(id_al) => Term::apply("varE", vec![id(id_al)]),
        il::ExpKind::Un(op, _, exp_al) => Term::apply("unE", vec![unop(op), exp(exp_al)]),
        il::ExpKind::Bin(op, _, exp_l, exp_r) => {
            Term::apply("binE", vec![binop(op), exp(exp_l), exp(exp_r)])
        }
        il::ExpKind::Cmp(op, _, exp_l, exp_r) => {
            Term::apply("cmpE", vec![cmpop(op), exp(exp_l), exp(exp_r)])
        }
        il::ExpKind::UpCast(typ_al, exp_al) => {
            Term::apply("upCastE", vec![typ(typ_al), exp(exp_al)])
        }
        il::ExpKind::DownCast(typ_al, exp_al) => {
            Term::apply("downCastE", vec![typ(typ_al), exp(exp_al)])
        }
        il::ExpKind::Sub(exp_al, typ_al) => Term::apply("subE", vec![exp(exp_al), typ(typ_al)]),
        il::ExpKind::Match(exp_al, pattern_al) => {
            Term::apply("matchE", vec![exp(exp_al), pattern(pattern_al)])
        }
        il::ExpKind::Tuple(exps_al) => Term::apply("tupE", vec![exps(exps_al)]),
        il::ExpKind::Case(not_exp) => {
            let rows = atom_rows(not_exp.mixop());
            let case = Term::apply("expCase", vec![mixop(&rows), exps(not_exp.args())]);
            Term::apply("injE", vec![case])
        }
        il::ExpKind::Str(fields_al) => {
            let fields = list("expFieldList", ".expFieldList", fields_al, |field_al| {
                Term::apply("expField", vec![atom(&field_al.atom), exp(&field_al.exp)])
            });
            Term::apply("strE", vec![fields])
        }
        il::ExpKind::Opt(exp_al) => {
            Term::apply("optE", vec![Term::opt("noExp", "someExp", exp_al.as_deref().map(exp))])
        }
        il::ExpKind::List(exps_al) => Term::apply("listE", vec![exps(exps_al)]),
        il::ExpKind::Cons(exp_h, exp_t) => Term::apply("consE", vec![exp(exp_h), exp(exp_t)]),
        il::ExpKind::Cat(exp_l, exp_r) => Term::apply("catE", vec![exp(exp_l), exp(exp_r)]),
        il::ExpKind::Mem(exp_e, exp_s) => Term::apply("memE", vec![exp(exp_e), exp(exp_s)]),
        il::ExpKind::Len(exp_al) => Term::apply("lenE", vec![exp(exp_al)]),
        il::ExpKind::Dot(exp_al, atom_al) => Term::apply("dotE", vec![exp(exp_al), atom(atom_al)]),
        il::ExpKind::Idx(exp_b, exp_i) => Term::apply("idxE", vec![exp(exp_b), exp(exp_i)]),
        il::ExpKind::Slice(exp_b, exp_i, exp_n) => {
            Term::apply("sliceE", vec![exp(exp_b), exp(exp_i), exp(exp_n)])
        }
        il::ExpKind::Upd(exp_b, path_al, exp_n) => {
            Term::apply("updE", vec![exp(exp_b), path(path_al), exp(exp_n)])
        }
        il::ExpKind::Call(id_al, targs_al, args_al) => {
            Term::apply("callE", vec![id(id_al), targs(targs_al), args(args_al)])
        }
        il::ExpKind::Iter(exp_al, exp_iter) => {
            let iter_exp =
                Term::apply("iterExp", vec![iter(&exp_iter.iter), varis(&exp_iter.vars)]);
            Term::apply("iterE", vec![exp(exp_al), iter_exp])
        }
    }
}

fn exps(exps_al: &[il::Exp]) -> Term {
    list("expList", ".expList", exps_al, exp)
}

fn pattern(pattern_al: &il::Pattern) -> Term {
    match pattern_al {
        il::Pattern::Case(mixop_al) => Term::apply("injPat", vec![mixop(&atom_rows(mixop_al))]),
        il::Pattern::List(il::ListPattern::Cons) => Term::apply("consPat", vec![]),
        il::Pattern::List(il::ListPattern::Fixed(len)) => {
            Term::apply("fixedPat", vec![int_token(len.to_string())])
        }
        il::Pattern::List(il::ListPattern::Nil) => Term::apply("nilPat", vec![]),
        il::Pattern::Opt(il::OptPattern::Some) => Term::apply("somePat", vec![]),
        il::Pattern::Opt(il::OptPattern::None) => Term::apply("nonePat", vec![]),
    }
}

fn path(path_al: &il::Path) -> Term {
    match &path_al.node {
        il::PathKind::Root => Term::apply("rootPath", vec![]),
        il::PathKind::Idx(path_al, exp_i) => {
            Term::apply("idxPath", vec![path(path_al), exp(exp_i)])
        }
        il::PathKind::Slice(path_al, exp_i, exp_n) => {
            Term::apply("slicePath", vec![path(path_al), exp(exp_i), exp(exp_n)])
        }
        il::PathKind::Dot(path_al, atom_al) => {
            Term::apply("dotPath", vec![path(path_al), atom(atom_al)])
        }
    }
}

fn arg(arg_al: &il::Arg) -> Term {
    match &arg_al.node {
        il::ArgKind::Exp(exp_al) => Term::apply("expA", vec![exp(exp_al)]),
        il::ArgKind::Def(id_al) => Term::apply("funA", vec![id(id_al)]),
    }
}

fn args(args_al: &[il::Arg]) -> Term {
    list("argList", ".argList", args_al, arg)
}

fn param(param_al: &il::Param) -> Term {
    match &param_al.node {
        il::ParamKind::Exp(typ_al) => Term::apply("expParam", vec![typ(typ_al)]),
        il::ParamKind::Def(id_al, tparams_al, params_al, typ_al) => Term::apply(
            "funParam",
            vec![id(id_al), tparams(tparams_al), params(params_al), typ(typ_al)],
        ),
    }
}

fn params(params_al: &[il::Param]) -> Term {
    list("paramList", ".paramList", params_al, param)
}

// == Premises

fn prem(prem_al: &al::Prem) -> Result<Term> {
    Ok(match &prem_al.node {
        al::PremKind::Rule(prem_rule) => {
            let (exps_in, exps_out) =
                split_inputs(&prem_rule.input_hint, prem_rule.not_exp.args())?;
            let exps_in = list("expList", ".expList", &exps_in, |exp_al| exp(exp_al));
            let exps_out = list("expList", ".expList", &exps_out, |exp_al| exp(exp_al));
            Term::apply("relPr", vec![id(&prem_rule.id), exps_in, exps_out])
        }
        al::PremKind::If(prem_if) => Term::apply("ifPr", vec![exp(&prem_if.exp)]),
        al::PremKind::IfHold(prem_hold) => {
            Term::apply("ifHoldPr", vec![id(&prem_hold.id), exps(prem_hold.not_exp.args())])
        }
        al::PremKind::IfNotHold(prem_hold) => {
            Term::apply("ifNotHoldPr", vec![id(&prem_hold.id), exps(prem_hold.not_exp.args())])
        }
        al::PremKind::Let(prem_let) => {
            Term::apply("letPr", vec![exp(&prem_let.exp_l), exp(&prem_let.exp_r)])
        }
        al::PremKind::Iter(prem_iter) => {
            let iter_prem = &prem_iter.prem_iter;
            let iter_prem = Term::apply(
                "iterPrem",
                vec![
                    iter(&iter_prem.iter),
                    varis(&iter_prem.vars_bound),
                    varis(&iter_prem.vars_bind),
                ],
            );
            Term::apply("iterPr", vec![prem(&prem_iter.prem)?, iter_prem])
        }
        al::PremKind::Debug(prem_debug) => Term::apply("debugPr", vec![exp(&prem_debug.exp)]),
    })
}

fn prems(prems_al: &[al::Prem]) -> Result<Term> {
    try_list("premList", ".premList", prems_al, prem)
}

// == Rules, clauses, and table rows

fn rulmatch(rule_match: &al::RuleMatch) -> Result<Term> {
    Ok(Term::apply("rulMatch", vec![exps(&rule_match.exps_input), prems(&rule_match.prems)?]))
}

/// Outputs precede premises, unlike the field order.
fn rulpath(rule_path: &al::RulePath) -> Result<Term> {
    Ok(Term::apply(
        "rulPath",
        vec![id(&rule_path.id), exps(&rule_path.exps_output), prems(&rule_path.prems)?],
    ))
}

fn rulgroup(rule_group: &al::RuleGroup) -> Result<Term> {
    let rule_group = &rule_group.node;
    let rule_paths = try_list("rulPathList", ".rulPathList", &rule_group.rule_paths, rulpath)?;
    Ok(Term::apply(
        "rulGroup",
        vec![id(&rule_group.id), rulmatch(&rule_group.rule_match)?, rule_paths],
    ))
}

fn elsgroup(else_group: &al::ElseGroup) -> Result<Term> {
    let else_group = &else_group.node;
    Ok(Term::apply(
        "elsGroup",
        vec![
            id(&else_group.id),
            rulmatch(&else_group.rule_match)?,
            rulpath(&else_group.rule_path)?,
        ],
    ))
}

fn clause(clause_al: &al::Clause) -> Result<Term> {
    let clause_al = &clause_al.node;
    Ok(Term::apply(
        "clause",
        vec![args(&clause_al.args), exp(&clause_al.exp), prems(&clause_al.prems)?],
    ))
}

fn tblrow(row_al: &al::TableRow) -> Result<Term> {
    let row_al = &row_al.node;
    Ok(Term::apply("tblRow", vec![args(&row_al.args), exp(&row_al.exp), prems(&row_al.prems)?]))
}

// == Definitions

/// The term of a definition; `var` definitions have none.
fn def(def_al: &al::Def) -> Result<Option<Term>> {
    let term = match &def_al.node {
        al::DefKind::Var(_) => return Ok(None),
        al::DefKind::Typ(al::TypDef::Extern(typdef_al)) => {
            Term::apply("extTypD", vec![id(&typdef_al.id)])
        }
        al::DefKind::Typ(al::TypDef::Defined(typdef_al)) => Term::apply(
            "typD",
            vec![id(&typdef_al.id), tparams(&typdef_al.tparams), deftyp(&typdef_al.def_typ)],
        ),
        al::DefKind::Rel(al::RelDef::Extern(rel_al)) => {
            let (typs_in, typs_out) = split_inputs(&rel_al.input_hint, rel_al.not_typ.node.args())?;
            let typs_in = list("typList", ".typList", &typs_in, |typ_al| typ(typ_al));
            let typs_out = list("typList", ".typList", &typs_out, |typ_al| typ(typ_al));
            Term::apply("extRelD", vec![id(&rel_al.id), typs_in, typs_out])
        }
        al::DefKind::Rel(al::RelDef::Defined(rel_al)) => {
            let (typs_in, typs_out) = split_inputs(&rel_al.input_hint, rel_al.not_typ.node.args())?;
            let typs_in = list("typList", ".typList", &typs_in, |typ_al| typ(typ_al));
            let typs_out = list("typList", ".typList", &typs_out, |typ_al| typ(typ_al));
            let rule_groups =
                try_list("rulGroupList", ".rulGroupList", &rel_al.rule_groups, rulgroup)?;
            let else_group = rel_al.else_group.as_ref().map(elsgroup).transpose()?;
            let else_group = Term::opt("noElsGroup", "someElsGroup", else_group);
            Term::apply("relD", vec![id(&rel_al.id), typs_in, typs_out, rule_groups, else_group])
        }
        al::DefKind::MetaFunc(al::MetaFuncDef::Extern(func_al)) => Term::apply(
            "extFuncD",
            vec![
                id(&func_al.id),
                tparams(&func_al.tparams),
                params(&func_al.params),
                typ(&func_al.typ),
            ],
        ),
        al::DefKind::MetaFunc(al::MetaFuncDef::Builtin(func_al)) => Term::apply(
            "builtinFuncD",
            vec![
                id(&func_al.id),
                tparams(&func_al.tparams),
                params(&func_al.params),
                typ(&func_al.typ),
            ],
        ),
        al::DefKind::MetaFunc(al::MetaFuncDef::Table(func_al)) => {
            let rows = try_list("tblRowList", ".tblRowList", &func_al.table_rows, tblrow)?;
            Term::apply(
                "tableFuncD",
                vec![id(&func_al.id), params(&func_al.params), typ(&func_al.typ), rows],
            )
        }
        al::DefKind::MetaFunc(al::MetaFuncDef::Defined(func_al)) => {
            let clauses = try_list("clauseList", ".clauseList", &func_al.clauses, clause)?;
            let else_clause = func_al.else_clause.as_ref().map(clause).transpose()?;
            let else_clause = Term::opt("noElsClause", "someElsClause", else_clause);
            Term::apply(
                "funcD",
                vec![
                    id(&func_al.id),
                    tparams(&func_al.tparams),
                    params(&func_al.params),
                    typ(&func_al.typ),
                    clauses,
                    else_clause,
                ],
            )
        }
    };
    Ok(Some(term))
}

// == Values

fn value(arena: &Arena, value: Value) -> Term {
    match arena.value.kind(value.node) {
        ValueKind::Bool(value) => Term::apply("boolV", vec![bool_token(*value)]),
        ValueKind::Num(num) => number(num),
        ValueKind::Text(text) => Term::apply("textV", vec![string_token(text)]),
        ValueKind::Struct(fields) => {
            let fields = list("valFieldList", ".valFieldList", fields, |field| {
                Term::apply("valField", vec![atom(&field.atom), self::value(arena, field.value)])
            });
            Term::apply("strV", vec![fields])
        }
        ValueKind::Case(case) => {
            let rows = atom_rows_flat(arena, *case.mixop());
            let case = Term::apply("valCase", vec![mixop(&rows), values(arena, case.args())]);
            Term::apply("injV", vec![case])
        }
        ValueKind::Tuple(items) => Term::apply("tupV", vec![values(arena, items)]),
        ValueKind::Opt(item) => {
            let item = item.map(|item| self::value(arena, item));
            Term::apply("optV", vec![Term::opt("noVal", "someVal", item)])
        }
        ValueKind::List(items) => Term::apply("listV", vec![values(arena, items)]),
        ValueKind::Func(id_al) => Term::apply("funcV", vec![id(id_al)]),
        ValueKind::Extern(json) => Term::apply("extV", vec![json_term(json)]),
    }
}

fn values(arena: &Arena, values: &[Value]) -> Term {
    list("valList", ".valList", values, |item| value(arena, *item))
}

// == The emitter

/// K: KAST documents of sort `Script` and `P4Opt`.
pub struct K;

impl Emit for K {
    type Script = Document;
    type Program = Document;

    fn script(spec: &al::Spec) -> Result<Document> {
        let defs = spec
            .iter()
            .filter_map(|def_al| def(def_al).transpose())
            .collect::<Result<Vec<_>>>()?;
        Ok(Document::new(Term::list("script", ".script", defs)))
    }

    /// The `val` wrapped as `someP4(val)`.
    fn program(arena: &Arena, value: Value) -> Result<Document> {
        Ok(Document::new(Term::apply("someP4", vec![self::value(arena, value)])))
    }
}
