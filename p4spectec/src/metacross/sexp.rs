//! AL specifications and P4 programs as Racket-readable s-expressions, for `spec-meta-redex/`
//!
//! A keyword case prints as `(KEYWORD arg ...)`, or `KEYWORD` alone without
//! arguments; a record without a keyword (`vari`, `typcase`, `rulpath`, …)
//! prints as the list of its fields. Options are lists of length zero or
//! one, a `FUNC` type prints bare, and `var` definitions, signatures, hints
//! and operand types are omitted, as the frozen Redex syntax expects.

use std::fmt::{self, Write};

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

// == Terms

/// An s-expression in Racket reader syntax.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Sexp {
    Sym(String),
    Str(String),
    Int(String),
    Bool(bool),
    List(Vec<Sexp>),
}

/// Text as a string literal with the escapes Racket's reader accepts.
struct RacketString<'a>(&'a str);

impl fmt::Display for RacketString<'_> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt.write_char('"')?;
        for ch in self.0.chars() {
            match ch {
                '"' => fmt.write_str("\\\"")?,
                '\\' => fmt.write_str("\\\\")?,
                '\n' => fmt.write_str("\\n")?,
                '\t' => fmt.write_str("\\t")?,
                '\r' => fmt.write_str("\\r")?,
                ch if (ch as u32) < 0x20 || ch == '\x7f' => write!(fmt, "\\u{:04x}", ch as u32)?,
                ch => fmt.write_char(ch)?,
            }
        }
        fmt.write_char('"')
    }
}

impl fmt::Display for Sexp {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Sym(sym) => fmt.write_str(sym),
            Self::Str(text) => RacketString(text).fmt(fmt),
            Self::Int(digits) => fmt.write_str(digits),
            Self::Bool(true) => fmt.write_str("#t"),
            Self::Bool(false) => fmt.write_str("#f"),
            Self::List(items) => {
                fmt.write_char('(')?;
                for (index, item) in items.iter().enumerate() {
                    if index > 0 {
                        fmt.write_char(' ')?;
                    }
                    item.fmt(fmt)?;
                }
                fmt.write_char(')')
            }
        }
    }
}

/// A `script`: its definitions, one per line.
pub struct Script(Vec<Sexp>);

impl fmt::Display for Script {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt.write_char('(')?;
        for defn in &self.0 {
            writeln!(fmt)?;
            defn.fmt(fmt)?;
        }
        fmt.write_str("\n)")
    }
}

fn sym(keyword: &str) -> Sexp {
    Sexp::Sym(keyword.into())
}

/// `(KEYWORD arg ...)`.
fn app(keyword: &str, args: Vec<Sexp>) -> Sexp {
    let mut items = Vec::with_capacity(args.len() + 1);
    items.push(sym(keyword));
    items.extend(args);
    Sexp::List(items)
}

fn list<T>(items: &[T], term: impl Fn(&T) -> Sexp) -> Sexp {
    Sexp::List(items.iter().map(term).collect())
}

fn try_list<T>(items: &[T], term: impl Fn(&T) -> Result<Sexp>) -> Result<Sexp> {
    Ok(Sexp::List(items.iter().map(term).collect::<Result<_>>()?))
}

/// `x?` as a list of length zero or one.
fn opt(item: Option<Sexp>) -> Sexp {
    Sexp::List(item.into_iter().collect())
}

// == Identifiers, atoms, and notation

fn id(id_al: &Id) -> Sexp {
    Sexp::Str(id_al.node.to_string())
}

fn atom(atom_al: &AtomPhrase) -> Sexp {
    Sexp::Str(Print::to_string(&atom_al.node))
}

fn mixop(rows: &[Vec<&AtomPhrase>]) -> Sexp {
    list(rows, |row| list(row, |atom_al| atom(atom_al)))
}

fn iter(iter_al: &Iter) -> Sexp {
    match iter_al {
        Iter::Opt => sym("QUEST"),
        Iter::List => sym("STAR"),
    }
}

// == Types

fn typ(typ_al: &Typ) -> Sexp {
    match &typ_al.node {
        TypKind::Bool => sym("BOOL"),
        TypKind::Num(num::Typ::Nat) => sym("NAT"),
        TypKind::Num(num::Typ::Int) => sym("INT"),
        TypKind::Text => sym("TEXT"),
        TypKind::Var(id_al, targs_al) => app("VAR", vec![id(id_al), typs(targs_al)]),
        TypKind::Tuple(typs_al) => app("TUP", vec![typs(typs_al)]),
        TypKind::Iter(typ_al, iter_al) => app("ITER", vec![typ(typ_al), iter(iter_al)]),
        // The frozen Redex syntax has `FUNC` without its signature
        TypKind::Func(_) => sym("FUNC"),
    }
}

fn typs(typs_al: &[Typ]) -> Sexp {
    list(typs_al, typ)
}

fn tparams(tparams_al: &[Id]) -> Sexp {
    list(tparams_al, id)
}

fn vari(var_al: &il::Var) -> Sexp {
    Sexp::List(vec![id(&var_al.id), typ(&var_al.typ), list(&var_al.iters, iter)])
}

fn varis(vars_al: &[il::Var]) -> Sexp {
    list(vars_al, vari)
}

fn deftyp(deftyp_al: &il::DefTyp) -> Sexp {
    match &deftyp_al.node {
        il::DefTypKind::Plain(typ_al) => app("ALIAS", vec![typ(typ_al)]),
        il::DefTypKind::Struct(fields_al) => {
            let fields = list(fields_al, |field_al| {
                Sexp::List(vec![atom(&field_al.atom), typ(&field_al.typ)])
            });
            app("STRUCT", vec![fields])
        }
        il::DefTypKind::Variant(cases_al) => {
            let cases = list(cases_al, |case_al| {
                let not_typ = &case_al.not_typ.node;
                Sexp::List(vec![mixop(&atom_rows(not_typ.mixop())), typs(not_typ.args())])
            });
            app("VARIANT", vec![cases])
        }
    }
}

// == Operators and numbers

fn unop(op: &il::UnOp) -> Sexp {
    sym(match op {
        il::UnOp::Bool(boolean::UnOp::Not) => "NOT",
        il::UnOp::Num(num::UnOp::Plus) => "PLUS",
        il::UnOp::Num(num::UnOp::Minus) => "MINUS",
    })
}

fn binop(op: &il::BinOp) -> Sexp {
    sym(match op {
        il::BinOp::Bool(boolean::BinOp::And) => "AND",
        il::BinOp::Bool(boolean::BinOp::Or) => "OR",
        il::BinOp::Bool(boolean::BinOp::Impl) => "IMPL",
        il::BinOp::Bool(boolean::BinOp::Equiv) => "EQUIV",
        il::BinOp::Num(num::BinOp::Add) => "ADD",
        il::BinOp::Num(num::BinOp::Sub) => "SUB",
        il::BinOp::Num(num::BinOp::Mul) => "MUL",
        il::BinOp::Num(num::BinOp::Div) => "DIV",
        il::BinOp::Num(num::BinOp::Mod) => "MOD",
        il::BinOp::Num(num::BinOp::Pow) => "POW",
    })
}

fn cmpop(op: &il::CmpOp) -> Sexp {
    sym(match op {
        il::CmpOp::Bool(boolean::CmpOp::Eq) => "EQ",
        il::CmpOp::Bool(boolean::CmpOp::Ne) => "NE",
        il::CmpOp::Num(num::CmpOp::Lt) => "LT",
        il::CmpOp::Num(num::CmpOp::Le) => "LE",
        il::CmpOp::Num(num::CmpOp::Gt) => "GT",
        il::CmpOp::Num(num::CmpOp::Ge) => "GE",
    })
}

/// A number of an expression or a value.
fn number(num: &Number) -> Sexp {
    match num {
        Number::Nat(nat) => app("NAT", vec![Sexp::Int(nat.to_string())]),
        Number::Int(int) => app("INT", vec![Sexp::Int(int.to_string())]),
    }
}

// == Expressions

fn exp(exp_al: &il::Exp) -> Sexp {
    match &exp_al.node {
        il::ExpKind::Bool(value) => app("BOOL", vec![Sexp::Bool(*value)]),
        il::ExpKind::Num(num) => number(num),
        il::ExpKind::Text(text) => app("TEXT", vec![Sexp::Str(text.clone())]),
        il::ExpKind::Id(id_al) => app("VAR", vec![id(id_al)]),
        il::ExpKind::Un(op, _, exp_al) => app("UN", vec![unop(op), exp(exp_al)]),
        il::ExpKind::Bin(op, _, exp_l, exp_r) => {
            app("BIN", vec![binop(op), exp(exp_l), exp(exp_r)])
        }
        il::ExpKind::Cmp(op, _, exp_l, exp_r) => {
            app("CMP", vec![cmpop(op), exp(exp_l), exp(exp_r)])
        }
        il::ExpKind::UpCast(typ_al, exp_al) => app("UPCAST", vec![typ(typ_al), exp(exp_al)]),
        il::ExpKind::DownCast(typ_al, exp_al) => app("DOWNCAST", vec![typ(typ_al), exp(exp_al)]),
        il::ExpKind::Sub(exp_al, typ_al) => app("SUB", vec![exp(exp_al), typ(typ_al)]),
        il::ExpKind::Match(exp_al, pattern_al) => {
            app("MATCH", vec![exp(exp_al), pattern(pattern_al)])
        }
        il::ExpKind::Tuple(exps_al) => app("TUP", vec![exps(exps_al)]),
        il::ExpKind::Case(not_exp) => {
            let case = Sexp::List(vec![mixop(&atom_rows(not_exp.mixop())), exps(not_exp.args())]);
            app("INJ", vec![case])
        }
        il::ExpKind::Str(fields_al) => {
            let fields = list(fields_al, |field_al| {
                Sexp::List(vec![atom(&field_al.atom), exp(&field_al.exp)])
            });
            app("STR", vec![fields])
        }
        il::ExpKind::Opt(exp_al) => app("OPT", vec![opt(exp_al.as_deref().map(exp))]),
        il::ExpKind::List(exps_al) => app("LIST", vec![exps(exps_al)]),
        il::ExpKind::Cons(exp_h, exp_t) => app("CONS", vec![exp(exp_h), exp(exp_t)]),
        il::ExpKind::Cat(exp_l, exp_r) => app("CAT", vec![exp(exp_l), exp(exp_r)]),
        il::ExpKind::Mem(exp_e, exp_s) => app("MEM", vec![exp(exp_e), exp(exp_s)]),
        il::ExpKind::Len(exp_al) => app("LEN", vec![exp(exp_al)]),
        il::ExpKind::Dot(exp_al, atom_al) => app("DOT", vec![exp(exp_al), atom(atom_al)]),
        il::ExpKind::Idx(exp_b, exp_i) => app("IDX", vec![exp(exp_b), exp(exp_i)]),
        il::ExpKind::Slice(exp_b, exp_i, exp_n) => {
            app("SLICE", vec![exp(exp_b), exp(exp_i), exp(exp_n)])
        }
        il::ExpKind::Upd(exp_b, path_al, exp_n) => {
            app("UPD", vec![exp(exp_b), path(path_al), exp(exp_n)])
        }
        il::ExpKind::Call(id_al, targs_al, args_al) => {
            app("CALL", vec![id(id_al), typs(targs_al), args(args_al)])
        }
        il::ExpKind::Iter(exp_al, exp_iter) => {
            let iter_exp = Sexp::List(vec![iter(&exp_iter.iter), varis(&exp_iter.vars)]);
            app("ITER", vec![exp(exp_al), iter_exp])
        }
    }
}

fn exps(exps_al: &[il::Exp]) -> Sexp {
    list(exps_al, exp)
}

fn pattern(pattern_al: &il::Pattern) -> Sexp {
    match pattern_al {
        il::Pattern::Case(mixop_al) => app("INJ", vec![mixop(&atom_rows(mixop_al))]),
        il::Pattern::List(il::ListPattern::Cons) => sym("CONS"),
        il::Pattern::List(il::ListPattern::Fixed(len)) => {
            app("FIXED", vec![Sexp::Int(len.to_string())])
        }
        il::Pattern::List(il::ListPattern::Nil) => sym("NIL"),
        il::Pattern::Opt(il::OptPattern::Some) => sym("SOME"),
        il::Pattern::Opt(il::OptPattern::None) => sym("NONE"),
    }
}

fn path(path_al: &il::Path) -> Sexp {
    match &path_al.node {
        il::PathKind::Root => sym("ROOT"),
        il::PathKind::Idx(path_al, exp_i) => app("IDX", vec![path(path_al), exp(exp_i)]),
        il::PathKind::Slice(path_al, exp_i, exp_n) => {
            app("SLICE", vec![path(path_al), exp(exp_i), exp(exp_n)])
        }
        il::PathKind::Dot(path_al, atom_al) => app("DOT", vec![path(path_al), atom(atom_al)]),
    }
}

fn arg(arg_al: &il::Arg) -> Sexp {
    match &arg_al.node {
        il::ArgKind::Exp(exp_al) => app("EXP", vec![exp(exp_al)]),
        il::ArgKind::Def(id_al) => app("FUN", vec![id(id_al)]),
    }
}

fn args(args_al: &[il::Arg]) -> Sexp {
    list(args_al, arg)
}

fn param(param_al: &il::Param) -> Sexp {
    match &param_al.node {
        il::ParamKind::Exp(typ_al) => app("EXP", vec![typ(typ_al)]),
        il::ParamKind::Def(id_al, tparams_al, params_al, typ_al) => {
            app("FUN", vec![id(id_al), tparams(tparams_al), params(params_al), typ(typ_al)])
        }
    }
}

fn params(params_al: &[il::Param]) -> Sexp {
    list(params_al, param)
}

// == Premises

fn prem(prem_al: &al::Prem) -> Result<Sexp> {
    Ok(match &prem_al.node {
        al::PremKind::Rule(prem_rule) => {
            let (exps_in, exps_out) =
                split_inputs(&prem_rule.input_hint, prem_rule.not_exp.args())?;
            let exps_in = list(&exps_in, |exp_al| exp(exp_al));
            let exps_out = list(&exps_out, |exp_al| exp(exp_al));
            app("REL", vec![id(&prem_rule.id), exps_in, exps_out])
        }
        al::PremKind::If(prem_if) => app("IF", vec![exp(&prem_if.exp)]),
        al::PremKind::IfHold(prem_hold) => {
            app("IFHOLD", vec![id(&prem_hold.id), exps(prem_hold.not_exp.args())])
        }
        al::PremKind::IfNotHold(prem_hold) => {
            app("IFNOTHOLD", vec![id(&prem_hold.id), exps(prem_hold.not_exp.args())])
        }
        al::PremKind::Let(prem_let) => app("LET", vec![exp(&prem_let.exp_l), exp(&prem_let.exp_r)]),
        al::PremKind::Iter(prem_iter) => {
            let iter_prem = &prem_iter.prem_iter;
            let iter_prem = Sexp::List(vec![
                iter(&iter_prem.iter),
                varis(&iter_prem.vars_bound),
                varis(&iter_prem.vars_bind),
            ]);
            app("ITER", vec![prem(&prem_iter.prem)?, iter_prem])
        }
        al::PremKind::Debug(prem_debug) => app("DEBUG", vec![exp(&prem_debug.exp)]),
    })
}

fn prems(prems_al: &[al::Prem]) -> Result<Sexp> {
    try_list(prems_al, prem)
}

// == Rules, clauses, and table rows

fn rulmatch(rule_match: &al::RuleMatch) -> Result<Sexp> {
    Ok(Sexp::List(vec![exps(&rule_match.exps_input), prems(&rule_match.prems)?]))
}

/// Outputs precede premises, unlike the field order.
fn rulpath(rule_path: &al::RulePath) -> Result<Sexp> {
    Ok(Sexp::List(vec![id(&rule_path.id), exps(&rule_path.exps_output), prems(&rule_path.prems)?]))
}

fn rulgroup(rule_group: &al::RuleGroup) -> Result<Sexp> {
    let rule_group = &rule_group.node;
    Ok(Sexp::List(vec![
        id(&rule_group.id),
        rulmatch(&rule_group.rule_match)?,
        try_list(&rule_group.rule_paths, rulpath)?,
    ]))
}

fn elsgroup(else_group: &al::ElseGroup) -> Result<Sexp> {
    let else_group = &else_group.node;
    Ok(Sexp::List(vec![
        id(&else_group.id),
        rulmatch(&else_group.rule_match)?,
        rulpath(&else_group.rule_path)?,
    ]))
}

fn clause(clause_al: &al::Clause) -> Result<Sexp> {
    let clause_al = &clause_al.node;
    Ok(Sexp::List(vec![args(&clause_al.args), exp(&clause_al.exp), prems(&clause_al.prems)?]))
}

fn tblrow(row_al: &al::TableRow) -> Result<Sexp> {
    let row_al = &row_al.node;
    Ok(Sexp::List(vec![args(&row_al.args), exp(&row_al.exp), prems(&row_al.prems)?]))
}

// == Definitions

/// The s-expression of a definition; `var` definitions have none.
fn def(def_al: &al::Def) -> Result<Option<Sexp>> {
    let sexp = match &def_al.node {
        al::DefKind::Var(_) => return Ok(None),
        al::DefKind::Typ(al::TypDef::Extern(typdef_al)) => app("EXTTYP", vec![id(&typdef_al.id)]),
        al::DefKind::Typ(al::TypDef::Defined(typdef_al)) => app(
            "TYP",
            vec![id(&typdef_al.id), tparams(&typdef_al.tparams), deftyp(&typdef_al.def_typ)],
        ),
        al::DefKind::Rel(al::RelDef::Extern(rel_al)) => {
            let (typs_in, typs_out) = split_inputs(&rel_al.input_hint, rel_al.not_typ.node.args())?;
            let typs_in = list(&typs_in, |typ_al| typ(typ_al));
            let typs_out = list(&typs_out, |typ_al| typ(typ_al));
            app("EXTREL", vec![id(&rel_al.id), typs_in, typs_out])
        }
        al::DefKind::Rel(al::RelDef::Defined(rel_al)) => {
            let (typs_in, typs_out) = split_inputs(&rel_al.input_hint, rel_al.not_typ.node.args())?;
            let typs_in = list(&typs_in, |typ_al| typ(typ_al));
            let typs_out = list(&typs_out, |typ_al| typ(typ_al));
            let rule_groups = try_list(&rel_al.rule_groups, rulgroup)?;
            let else_group = opt(rel_al.else_group.as_ref().map(elsgroup).transpose()?);
            app("REL", vec![id(&rel_al.id), typs_in, typs_out, rule_groups, else_group])
        }
        al::DefKind::MetaFunc(al::MetaFuncDef::Extern(func_al)) => app(
            "EXTFUNC",
            vec![
                id(&func_al.id),
                tparams(&func_al.tparams),
                params(&func_al.params),
                typ(&func_al.typ),
            ],
        ),
        al::DefKind::MetaFunc(al::MetaFuncDef::Builtin(func_al)) => app(
            "BUILTINFUNC",
            vec![
                id(&func_al.id),
                tparams(&func_al.tparams),
                params(&func_al.params),
                typ(&func_al.typ),
            ],
        ),
        al::DefKind::MetaFunc(al::MetaFuncDef::Table(func_al)) => app(
            "TABLEFUNC",
            vec![
                id(&func_al.id),
                params(&func_al.params),
                typ(&func_al.typ),
                try_list(&func_al.table_rows, tblrow)?,
            ],
        ),
        al::DefKind::MetaFunc(al::MetaFuncDef::Defined(func_al)) => app(
            "FUNC",
            vec![
                id(&func_al.id),
                tparams(&func_al.tparams),
                params(&func_al.params),
                typ(&func_al.typ),
                try_list(&func_al.clauses, clause)?,
                opt(func_al.else_clause.as_ref().map(clause).transpose()?),
            ],
        ),
    };
    Ok(Some(sexp))
}

// == Values

fn value(arena: &Arena, value: Value) -> Sexp {
    match arena.value.kind(value.node) {
        ValueKind::Bool(value) => app("BOOL", vec![Sexp::Bool(*value)]),
        ValueKind::Num(num) => number(num),
        ValueKind::Text(text) => app("TEXT", vec![Sexp::Str(text.clone())]),
        ValueKind::Struct(fields) => {
            let fields = list(fields, |field| {
                Sexp::List(vec![atom(&field.atom), self::value(arena, field.value)])
            });
            app("STR", vec![fields])
        }
        ValueKind::Case(case) => {
            let rows = atom_rows_flat(arena, *case.mixop());
            app("INJ", vec![Sexp::List(vec![mixop(&rows), values(arena, case.args())])])
        }
        ValueKind::Tuple(items) => app("TUP", vec![values(arena, items)]),
        ValueKind::Opt(item) => app("OPT", vec![opt(item.map(|item| self::value(arena, item)))]),
        ValueKind::List(items) => app("LIST", vec![values(arena, items)]),
        ValueKind::Func(id_al) => app("FUNC", vec![id(id_al)]),
        // Redex decodes the JSON text back with `string->jsexpr`
        ValueKind::Extern(json) => app("EXT", vec![Sexp::Str(json_text(json))]),
    }
}

fn values(arena: &Arena, values: &[Value]) -> Sexp {
    list(values, |item| value(arena, *item))
}

fn json_text(json: &json) -> String {
    serde_json::to_string(json).expect("JSON values serialize")
}

// == The emitter

/// PLT Redex: `script` and `val` s-expressions.
pub struct Redex;

impl Emit for Redex {
    type Script = Script;
    type Program = Sexp;

    fn script(spec: &al::Spec) -> Result<Script> {
        let defs = spec
            .iter()
            .filter_map(|def_al| def(def_al).transpose())
            .collect::<Result<Vec<_>>>()?;
        Ok(Script(defs))
    }

    fn program(arena: &Arena, value: Value) -> Result<Sexp> {
        Ok(self::value(arena, value))
    }
}
