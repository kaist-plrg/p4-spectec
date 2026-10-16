//! Meta values as Racket-readable s-expressions, for `spec-meta-redex/`
//!
//! A case prints as `(KEYWORD arg ...)`, or `KEYWORD` alone without arguments;
//! a case whose notation has no leading keyword prints as the list of its
//! arguments. Punctuation atoms are dropped. Options are lists of length zero
//! or one, and a `FUNC` type prints bare, as the frozen Redex syntax expects.

use std::fmt::{self, Write};

use crate::{
    lang::{
        common::prim::num,
        data::{arena::Arena, value::flat::Value, value::flat::ValueKind},
    },
    util::json::json,
};

use super::{Emit, Error, Result, leading_keyword, typ_name};

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

// == Walking meta values

/// The s-expression of a meta value.
fn term(arena: &Arena, value: Value) -> Result<Sexp> {
    let typ = arena.value.typ(value.note);
    match arena.value.kind(value.node) {
        ValueKind::Bool(value) => Ok(Sexp::Bool(*value)),
        ValueKind::Num(num) => Ok(Sexp::Int(num::to_int(num).to_string())),
        ValueKind::Text(text) => Ok(Sexp::Str(text.clone())),
        ValueKind::List(values) => Ok(Sexp::List(terms(arena, values)?)),
        ValueKind::Opt(value) => Ok(Sexp::List(terms(arena, value.as_slice())?)),
        ValueKind::Tuple(values) => Ok(Sexp::List(terms(arena, values)?)),
        // Redex decodes the JSON text back with `string->jsexpr`
        ValueKind::Extern(json) => Ok(Sexp::Str(json_text(json))),
        ValueKind::Case(case) => {
            let args = terms(arena, case.args())?;
            match leading_keyword(arena, *case.mixop()) {
                // The frozen Redex syntax has `FUNC` without its signature
                Some(keyword) if keyword == "FUNC" && typ_name(typ) == Some("typ") => {
                    Ok(Sexp::Sym(keyword.into()))
                }
                Some(keyword) if args.is_empty() => Ok(Sexp::Sym(keyword.into())),
                Some(keyword) => {
                    let mut items = Vec::with_capacity(args.len() + 1);
                    items.push(Sexp::Sym(keyword.into()));
                    items.extend(args);
                    Ok(Sexp::List(items))
                }
                None => Ok(Sexp::List(args)),
            }
        }
        ValueKind::Struct(_) | ValueKind::Func(_) => {
            Err(Error::emit_invalid("struct and function values have no s-expression"))
        }
    }
}

fn terms(arena: &Arena, values: &[Value]) -> Result<Vec<Sexp>> {
    values.iter().map(|value| term(arena, *value)).collect()
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

    fn script(arena: &Arena, value: Value) -> Result<Script> {
        let ValueKind::List(defns) = arena.value.kind(value.node) else {
            return Err(Error::emit_invalid("a script is a list of definitions"));
        };
        Ok(Script(terms(arena, defns)?))
    }

    fn program(arena: &Arena, value: Value) -> Result<Sexp> {
        term(arena, value)
    }
}
