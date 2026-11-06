//! The s-expressions that spec-meta-redex reads, as Racket's `read` reads them
//!
//! Every call and response to spec-meta-redex (`serve`) is one line holding one s-expression,
//! which `read` in `spec-meta-redex/3-execution/3.04-extern-host.rkt` takes as a Redex term.
//!
//! `ToSexp` gives each node of the AL syntax its s-expression, as a `spec` term (`script`).
//! A keyword case prints as `(KEYWORD arg ...)`, or `KEYWORD` alone without arguments;
//! a record without a keyword (`vari`, `typcase`, `rulpath`, …) prints as the list of its fields.
//! Options are lists of length zero or one,
//! and `var` definitions, signatures, hints and operand types are omitted,
//! as the Redex syntax expects.
//!
//! `write_value` and `write_typ` print values and types as the `val` and `typ` terms
//! of spec-meta-redex (`2-runtime/2.0-value.rkt`, `1-syntax/1.0-syntax.rkt`):
//!
//! ```text
//! val   ::= (BOOL #t) | (BOOL #f) | (NAT n) | (INT i) | (TEXT "text")
//!         | (STR (("atom" val) ...)) | (INJ (mixop (val ...)))
//!         | (TUP (val ...)) | (OPT ()) | (OPT (val)) | (LIST (val ...))
//!         | (FUNC "id") | (EXT "json")
//! mixop ::= (("atom" ...) ...)        atoms between argument positions
//! typ   ::= BOOL | NAT | INT | TEXT | (VAR "id" (typ ...)) | (TUP (typ ...))
//!         | (ITER typ QUEST) | (ITER typ STAR) | (FUNC ("id" ...) (typ ...) typ)
//! ```
//!
//! Atoms travel in their printed spelling.
//! An extern value's payload is its JSON text in a string,
//! which spec-meta-redex holds without reading it.

use super::cache::Cache;
use crate::lang::{
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
        notation::{AtomPhrase, flat::Mixop, items::Item, tree},
        typ::{Typ, TypKind},
        value::flat::{Value, ValueKind},
    },
    hints::input::{self, InputHint},
    il::ast as il,
    traits::print::Print,
};
use std::fmt::{self, Write};

/// The input and output positions of a relation's notation.
///
/// Elaboration validates every input hint against its relation's arity.
fn split_inputs<'a, T>(hint: &InputHint, items: &'a [T]) -> (Vec<&'a T>, Vec<&'a T>) {
    input::split(hint, items.iter().collect()).expect("input hint must fit relation")
}

/// The `script` of `spec`: the list of its definitions.
pub(super) fn script(spec: &al::Spec) -> String {
    Sexp::List(spec.iter().filter_map(def).collect()).to_string()
}

// == Terms

/// An s-expression in Racket reader syntax.
enum Sexp {
    Sym(&'static str),
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

/// `(KEYWORD arg ...)`.
fn app(keyword: &'static str, args: impl IntoIterator<Item = Sexp>) -> Sexp {
    Sexp::List(std::iter::once(Sexp::Sym(keyword)).chain(args).collect())
}

/// `x?` as a list of length zero or one.
fn opt(item: Option<Sexp>) -> Sexp {
    Sexp::List(item.into_iter().collect())
}

// == S-expressions of the AL syntax

/// The s-expression of a node of the AL syntax.
trait ToSexp {
    fn sexp(&self) -> Sexp;
}

impl<T: ToSexp + ?Sized> ToSexp for &T {
    fn sexp(&self) -> Sexp {
        (**self).sexp()
    }
}

impl<T: ToSexp> ToSexp for [T] {
    fn sexp(&self) -> Sexp {
        Sexp::List(self.iter().map(ToSexp::sexp).collect())
    }
}

impl<T: ToSexp> ToSexp for Vec<T> {
    fn sexp(&self) -> Sexp {
        self[..].sexp()
    }
}

// - Identifiers, atoms, and notation

impl ToSexp for Id {
    fn sexp(&self) -> Sexp {
        Sexp::Str(self.node.to_string())
    }
}

impl ToSexp for AtomPhrase {
    fn sexp(&self) -> Sexp {
        Sexp::Str(Print::to_string(&self.node))
    }
}

impl ToSexp for Iter {
    fn sexp(&self) -> Sexp {
        match self {
            Iter::Opt => Sexp::Sym("QUEST"),
            Iter::List => Sexp::Sym("STAR"),
        }
    }
}

// - Types

impl ToSexp for Typ {
    fn sexp(&self) -> Sexp {
        match &self.node {
            TypKind::Bool => Sexp::Sym("BOOL"),
            TypKind::Num(num::Typ::Nat) => Sexp::Sym("NAT"),
            TypKind::Num(num::Typ::Int) => Sexp::Sym("INT"),
            TypKind::Text => Sexp::Sym("TEXT"),
            TypKind::Var(id, targs) => app("VAR", [id.sexp(), targs.sexp()]),
            TypKind::Tuple(typs) => app("TUP", [typs.sexp()]),
            TypKind::Iter(typ, iter) => app("ITER", [typ.sexp(), iter.sexp()]),
            TypKind::Func(func) => {
                app("FUNC", [func.tparams.sexp(), func.typs_params.sexp(), func.typ_ret.sexp()])
            }
        }
    }
}

impl ToSexp for il::Var {
    fn sexp(&self) -> Sexp {
        Sexp::List(vec![self.id.sexp(), self.typ.sexp(), self.iters.sexp()])
    }
}

impl ToSexp for il::DefTyp {
    fn sexp(&self) -> Sexp {
        match &self.node {
            il::DefTypKind::Plain(typ) => app("ALIAS", [typ.sexp()]),
            il::DefTypKind::Struct(fields) => app("STRUCT", [fields.sexp()]),
            il::DefTypKind::Variant(cases) => app("VARIANT", [cases.sexp()]),
        }
    }
}

impl ToSexp for il::TypField {
    fn sexp(&self) -> Sexp {
        Sexp::List(vec![self.atom.sexp(), self.typ.sexp()])
    }
}

impl ToSexp for il::TypCase {
    fn sexp(&self) -> Sexp {
        let not_typ = &self.not_typ.node;
        Sexp::List(vec![rows_tree(not_typ.mixop()).sexp(), not_typ.args().sexp()])
    }
}

// - Operators and numbers

impl ToSexp for il::UnOp {
    fn sexp(&self) -> Sexp {
        Sexp::Sym(match self {
            il::UnOp::Bool(boolean::UnOp::Not) => "NOT",
            il::UnOp::Num(num::UnOp::Plus) => "PLUS",
            il::UnOp::Num(num::UnOp::Minus) => "MINUS",
        })
    }
}

impl ToSexp for il::BinOp {
    fn sexp(&self) -> Sexp {
        Sexp::Sym(match self {
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
}

impl ToSexp for il::CmpOp {
    fn sexp(&self) -> Sexp {
        Sexp::Sym(match self {
            il::CmpOp::Bool(boolean::CmpOp::Eq) => "EQ",
            il::CmpOp::Bool(boolean::CmpOp::Ne) => "NE",
            il::CmpOp::Num(num::CmpOp::Lt) => "LT",
            il::CmpOp::Num(num::CmpOp::Le) => "LE",
            il::CmpOp::Num(num::CmpOp::Gt) => "GT",
            il::CmpOp::Num(num::CmpOp::Ge) => "GE",
        })
    }
}

/// A number of an expression or a value.
impl ToSexp for Number {
    fn sexp(&self) -> Sexp {
        match self {
            Number::Nat(nat) => app("NAT", [Sexp::Int(nat.to_string())]),
            Number::Int(int) => app("INT", [Sexp::Int(int.to_string())]),
        }
    }
}

// - Expressions

impl ToSexp for il::Exp {
    fn sexp(&self) -> Sexp {
        match &self.node {
            il::ExpKind::Bool(value) => app("BOOL", [Sexp::Bool(*value)]),
            il::ExpKind::Num(num) => num.sexp(),
            il::ExpKind::Text(text) => app("TEXT", [Sexp::Str(text.clone())]),
            il::ExpKind::Id(id) => app("VAR", [id.sexp()]),
            il::ExpKind::Un(op, _, exp) => app("UN", [op.sexp(), exp.sexp()]),
            il::ExpKind::Bin(op, _, exp_l, exp_r) => {
                app("BIN", [op.sexp(), exp_l.sexp(), exp_r.sexp()])
            }
            il::ExpKind::Cmp(op, _, exp_l, exp_r) => {
                app("CMP", [op.sexp(), exp_l.sexp(), exp_r.sexp()])
            }
            il::ExpKind::UpCast(typ, exp) => app("UPCAST", [typ.sexp(), exp.sexp()]),
            il::ExpKind::DownCast(typ, exp) => app("DOWNCAST", [typ.sexp(), exp.sexp()]),
            il::ExpKind::Sub(exp, typ) => app("SUB", [exp.sexp(), typ.sexp()]),
            il::ExpKind::Match(exp, pattern) => app("MATCH", [exp.sexp(), pattern.sexp()]),
            il::ExpKind::Tuple(exps) => app("TUP", [exps.sexp()]),
            il::ExpKind::Case(not_exp) => {
                let case =
                    Sexp::List(vec![rows_tree(not_exp.mixop()).sexp(), not_exp.args().sexp()]);
                app("INJ", [case])
            }
            il::ExpKind::Str(fields) => app("STR", [fields.sexp()]),
            il::ExpKind::Opt(exp) => app("OPT", [opt(exp.as_deref().map(ToSexp::sexp))]),
            il::ExpKind::List(exps) => app("LIST", [exps.sexp()]),
            il::ExpKind::Cons(exp_h, exp_t) => app("CONS", [exp_h.sexp(), exp_t.sexp()]),
            il::ExpKind::Cat(exp_l, exp_r) => app("CAT", [exp_l.sexp(), exp_r.sexp()]),
            il::ExpKind::Mem(exp_e, exp_s) => app("MEM", [exp_e.sexp(), exp_s.sexp()]),
            il::ExpKind::Len(exp) => app("LEN", [exp.sexp()]),
            il::ExpKind::Dot(exp, atom) => app("DOT", [exp.sexp(), atom.sexp()]),
            il::ExpKind::Idx(exp_b, exp_i) => app("IDX", [exp_b.sexp(), exp_i.sexp()]),
            il::ExpKind::Slice(exp_b, exp_i, exp_n) => {
                app("SLICE", [exp_b.sexp(), exp_i.sexp(), exp_n.sexp()])
            }
            il::ExpKind::Upd(exp_b, path, exp_n) => {
                app("UPD", [exp_b.sexp(), path.sexp(), exp_n.sexp()])
            }
            il::ExpKind::Call(id, targs, args) => {
                app("CALL", [id.sexp(), targs.sexp(), args.sexp()])
            }
            il::ExpKind::Iter(exp, exp_iter) => {
                let iter_exp = Sexp::List(vec![exp_iter.iter.sexp(), exp_iter.vars.sexp()]);
                app("ITER", [exp.sexp(), iter_exp])
            }
        }
    }
}

impl ToSexp for il::ExpField {
    fn sexp(&self) -> Sexp {
        Sexp::List(vec![self.atom.sexp(), self.exp.sexp()])
    }
}

impl ToSexp for il::Pattern {
    fn sexp(&self) -> Sexp {
        match self {
            il::Pattern::Case(mixop) => app("INJ", [rows_tree(mixop).sexp()]),
            il::Pattern::List(il::ListPattern::Cons) => Sexp::Sym("CONS"),
            il::Pattern::List(il::ListPattern::Fixed(len)) => {
                app("FIXED", [Sexp::Int(len.to_string())])
            }
            il::Pattern::List(il::ListPattern::Nil) => Sexp::Sym("NIL"),
            il::Pattern::Opt(il::OptPattern::Some) => Sexp::Sym("SOME"),
            il::Pattern::Opt(il::OptPattern::None) => Sexp::Sym("NONE"),
        }
    }
}

impl ToSexp for il::Path {
    fn sexp(&self) -> Sexp {
        match &self.node {
            il::PathKind::Root => Sexp::Sym("ROOT"),
            il::PathKind::Idx(path, exp_i) => app("IDX", [path.sexp(), exp_i.sexp()]),
            il::PathKind::Slice(path, exp_i, exp_n) => {
                app("SLICE", [path.sexp(), exp_i.sexp(), exp_n.sexp()])
            }
            il::PathKind::Dot(path, atom) => app("DOT", [path.sexp(), atom.sexp()]),
        }
    }
}

impl ToSexp for il::Arg {
    fn sexp(&self) -> Sexp {
        match &self.node {
            il::ArgKind::Exp(exp) => app("EXP", [exp.sexp()]),
            il::ArgKind::Def(id) => app("FUN", [id.sexp()]),
        }
    }
}

impl ToSexp for il::Param {
    fn sexp(&self) -> Sexp {
        match &self.node {
            il::ParamKind::Exp(typ) => app("EXP", [typ.sexp()]),
            il::ParamKind::Def(id, tparams, params, typ) => {
                app("FUN", signature(id, tparams, params, typ))
            }
        }
    }
}

/// The identifier, type parameters, parameters and result type of a function.
fn signature(id: &Id, tparams: &[Id], params: &[il::Param], typ: &Typ) -> [Sexp; 4] {
    [id.sexp(), tparams.sexp(), params.sexp(), typ.sexp()]
}

// - Premises

impl ToSexp for al::Prem {
    fn sexp(&self) -> Sexp {
        match &self.node {
            al::PremKind::Rule(prem) => {
                let (exps_in, exps_out) = split_inputs(&prem.input_hint, prem.not_exp.args());
                app("REL", [prem.id.sexp(), exps_in.sexp(), exps_out.sexp()])
            }
            al::PremKind::If(prem) => app("IF", [prem.exp.sexp()]),
            al::PremKind::IfHold(prem) => {
                app("IFHOLD", [prem.id.sexp(), prem.not_exp.args().sexp()])
            }
            al::PremKind::IfNotHold(prem) => {
                app("IFNOTHOLD", [prem.id.sexp(), prem.not_exp.args().sexp()])
            }
            al::PremKind::Let(prem) => app("LET", [prem.exp_l.sexp(), prem.exp_r.sexp()]),
            al::PremKind::Iter(prem) => {
                let iter_prem = &prem.prem_iter;
                let iter_prem = Sexp::List(vec![
                    iter_prem.iter.sexp(),
                    iter_prem.vars_bound.sexp(),
                    iter_prem.vars_bind.sexp(),
                ]);
                app("ITER", [prem.prem.sexp(), iter_prem])
            }
            al::PremKind::Debug(prem) => app("DEBUG", [prem.exp.sexp()]),
        }
    }
}

// - Rules, clauses, and table rows

impl ToSexp for al::RuleMatch {
    fn sexp(&self) -> Sexp {
        Sexp::List(vec![self.exps_input.sexp(), self.prems.sexp()])
    }
}

/// Outputs precede premises, unlike the field order.
impl ToSexp for al::RulePath {
    fn sexp(&self) -> Sexp {
        Sexp::List(vec![self.id.sexp(), self.exps_output.sexp(), self.prems.sexp()])
    }
}

impl ToSexp for al::RuleGroup {
    fn sexp(&self) -> Sexp {
        let group = &self.node;
        Sexp::List(vec![group.id.sexp(), group.rule_match.sexp(), group.rule_paths.sexp()])
    }
}

impl ToSexp for al::ElseGroup {
    fn sexp(&self) -> Sexp {
        let group = &self.node;
        Sexp::List(vec![group.id.sexp(), group.rule_match.sexp(), group.rule_path.sexp()])
    }
}

impl ToSexp for al::ClauseKind {
    fn sexp(&self) -> Sexp {
        Sexp::List(vec![self.args.sexp(), self.exp.sexp(), self.prems.sexp()])
    }
}

impl ToSexp for al::Clause {
    fn sexp(&self) -> Sexp {
        self.node.sexp()
    }
}

impl ToSexp for al::ElseClause {
    fn sexp(&self) -> Sexp {
        self.node.clause.sexp()
    }
}

impl ToSexp for al::TableRow {
    fn sexp(&self) -> Sexp {
        let row = &self.node;
        Sexp::List(vec![row.args.sexp(), row.exp.sexp(), row.prems.sexp()])
    }
}

// - Definitions

/// The input and output types of a relation's notation.
fn rel_typs(hint: &InputHint, not_typ: &il::NotTyp) -> [Sexp; 2] {
    let (typs_in, typs_out) = split_inputs(hint, not_typ.node.args());
    [typs_in.sexp(), typs_out.sexp()]
}

/// The s-expression of a definition; `var` definitions have none.
fn def(def: &al::Def) -> Option<Sexp> {
    Some(match &def.node {
        al::DefKind::Var(_) => return None,
        al::DefKind::Typ(al::TypDef::Extern(typdef)) => app("EXTTYP", [typdef.id.sexp()]),
        al::DefKind::Typ(al::TypDef::Defined(typdef)) => {
            app("TYP", [typdef.id.sexp(), typdef.tparams.sexp(), typdef.def_typ.sexp()])
        }
        al::DefKind::Rel(al::RelDef::Extern(rel)) => {
            let [typs_in, typs_out] = rel_typs(&rel.input_hint, &rel.not_typ);
            app("EXTREL", [rel.id.sexp(), typs_in, typs_out])
        }
        al::DefKind::Rel(al::RelDef::Defined(rel)) => {
            let [typs_in, typs_out] = rel_typs(&rel.input_hint, &rel.not_typ);
            let else_group = opt(rel.else_group.as_ref().map(ToSexp::sexp));
            app("REL", [rel.id.sexp(), typs_in, typs_out, rel.rule_groups.sexp(), else_group])
        }
        al::DefKind::MetaFunc(al::MetaFuncDef::Extern(func)) => {
            app("EXTFUNC", signature(&func.id, &func.tparams, &func.params, &func.typ))
        }
        al::DefKind::MetaFunc(al::MetaFuncDef::Builtin(func)) => {
            app("BUILTINFUNC", signature(&func.id, &func.tparams, &func.params, &func.typ))
        }
        al::DefKind::MetaFunc(al::MetaFuncDef::Table(func)) => app(
            "TABLEFUNC",
            [func.id.sexp(), func.params.sexp(), func.typ.sexp(), func.table_rows.sexp()],
        ),
        al::DefKind::MetaFunc(al::MetaFuncDef::Defined(func)) => {
            let else_clause = opt(func.else_clause.as_ref().map(ToSexp::sexp));
            let signature = signature(&func.id, &func.tparams, &func.params, &func.typ);
            app(
                "FUNC",
                signature
                    .into_iter()
                    .chain([func.clauses.sexp(), else_clause]),
            )
        }
    })
}

// == Notation

/// The atom rows between argument positions, from a visit of the notation.
///
/// A mixop with n holes gives n + 1 rows, including empty boundary rows.
fn rows<'a>(visit: impl FnOnce(&mut dyn FnMut(Item<'a, usize>))) -> Vec<Vec<&'a AtomPhrase>> {
    let mut rows = vec![vec![]];
    visit(&mut |item| match item {
        Item::Arg(_) => rows.push(vec![]),
        Item::Atom(atom) => rows.last_mut().expect("rows start nonempty").push(atom),
    });
    rows
}

/// The atom rows of tree notation, as the specification writes it.
fn rows_tree(mixop: &tree::Mixop) -> Vec<Vec<&AtomPhrase>> {
    rows(|visit_item| mixop.visit(visit_item))
}

// == Values and types

/// Appends `text` as a string literal that `read` reads back as `text`.
pub(super) fn write_string(out: &mut String, text: &str) {
    write!(out, "{}", RacketString(text)).expect("writing to a string succeeds");
}

/// Appends a list of `items`, each printed by `write_item`.
fn write_list<T>(
    out: &mut String,
    items: impl IntoIterator<Item = T>,
    mut write_item: impl FnMut(&mut String, T),
) {
    out.push('(');
    for (idx, item) in items.into_iter().enumerate() {
        if idx > 0 {
            out.push(' ');
        }
        write_item(out, item);
    }
    out.push(')');
}

fn write_typ(out: &mut String, typ: &Typ) {
    write!(out, "{}", typ.sexp()).expect("writing to a string succeeds");
}

/// Appends `(typ ...)`.
pub(super) fn write_typs(out: &mut String, typs: &[Typ]) {
    write_list(out, typs, write_typ);
}

/// Appends the atom rows of a value's notation, printed once per notation.
fn write_mixop(out: &mut String, arena: &Arena, cache: &mut Cache, mixop: Mixop) {
    if let Some(text) = cache.mixops_out.get(&mixop) {
        out.push_str(text);
        return;
    }
    let text = rows(|visit_item| mixop.visit(&arena.mixop, visit_item))
        .sexp()
        .to_string();
    out.push_str(&text);
    if cache.enabled {
        cache.mixops_out.insert(mixop, text);
    }
}

/// Appends `value` as a `val` term.
pub(super) fn write_value(out: &mut String, arena: &Arena, cache: &mut Cache, value: Value) {
    match arena.value.kind(value.node) {
        ValueKind::Bool(value) => out.push_str(if *value { "(BOOL #t)" } else { "(BOOL #f)" }),
        ValueKind::Num(Number::Nat(nat)) => write!(out, "(NAT {nat})").expect("writing succeeds"),
        ValueKind::Num(Number::Int(int)) => write!(out, "(INT {int})").expect("writing succeeds"),
        ValueKind::Text(text) => {
            out.push_str("(TEXT ");
            write_string(out, text);
            out.push(')');
        }
        ValueKind::Struct(fields) => {
            out.push_str("(STR ");
            write_list(out, fields, |out, field| {
                out.push('(');
                write_string(out, &Print::to_string(&field.atom.node));
                out.push(' ');
                write_value(out, arena, cache, field.value);
                out.push(')');
            });
            out.push(')');
        }
        ValueKind::Case(case) => {
            out.push_str("(INJ (");
            write_mixop(out, arena, cache, *case.mixop());
            out.push(' ');
            write_values(out, arena, cache, case.args());
            out.push_str("))");
        }
        ValueKind::Tuple(values) => {
            out.push_str("(TUP ");
            write_values(out, arena, cache, values);
            out.push(')');
        }
        ValueKind::Opt(value_opt) => {
            out.push_str("(OPT ");
            write_list(out, value_opt, |out, value| write_value(out, arena, cache, *value));
            out.push(')');
        }
        ValueKind::List(values) => {
            out.push_str("(LIST ");
            write_values(out, arena, cache, values);
            out.push(')');
        }
        ValueKind::Func(id) => {
            out.push_str("(FUNC ");
            write_string(out, &id.node);
            out.push(')');
        }
        ValueKind::Extern(json) => {
            out.push_str("(EXT ");
            write_string(out, &json.to_string());
            out.push(')');
        }
    }
}

/// Appends `(val ...)`.
pub(super) fn write_values(out: &mut String, arena: &Arena, cache: &mut Cache, values: &[Value]) {
    write_list(out, values, |out, value| write_value(out, arena, cache, *value));
}
