//! Meta values as Racket-readable s-expressions, for `spec-meta-redex/`
//!
//! A case prints as `(KEYWORD arg ...)`, or `KEYWORD` alone without arguments;
//! a case whose notation has no leading keyword prints as the list of its
//! arguments. Punctuation atoms are dropped. Options are lists of length zero
//! or one, and a `FUNC` type prints bare, as the frozen Redex syntax expects.

use std::fmt::Write;

use crate::{
    lang::{
        common::prim::num,
        data::{arena::Arena, value::flat::Value, value::flat::ValueKind},
    },
    util::json::json,
};

use super::{Result, error::emit_invalid, leading_keyword, typ_name};

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

// == Printing

/// Appends a string literal with the escapes Racket's reader accepts.
fn write_string(out: &mut String, text: &str) {
    out.push('"');
    for ch in text.chars() {
        match ch {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\t' => out.push_str("\\t"),
            '\r' => out.push_str("\\r"),
            ch if (ch as u32) < 0x20 || ch == '\x7f' => {
                write!(out, "\\u{:04x}", ch as u32).expect("writing to a String cannot fail");
            }
            ch => out.push(ch),
        }
    }
    out.push('"');
}

impl Sexp {
    /// Appends this term to `out`.
    pub fn write(&self, out: &mut String) {
        match self {
            Self::Sym(sym) => out.push_str(sym),
            Self::Str(text) => write_string(out, text),
            Self::Int(digits) => out.push_str(digits),
            Self::Bool(true) => out.push_str("#t"),
            Self::Bool(false) => out.push_str("#f"),
            Self::List(items) => {
                out.push('(');
                for (index, item) in items.iter().enumerate() {
                    if index > 0 {
                        out.push(' ');
                    }
                    item.write(out);
                }
                out.push(')');
            }
        }
    }
}

// == Walking meta values

/// The s-expression of a meta value.
pub fn term(arena: &Arena, value: Value) -> Result<Sexp> {
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
                    Ok(Sexp::Sym(keyword))
                }
                Some(keyword) if args.is_empty() => Ok(Sexp::Sym(keyword)),
                Some(keyword) => {
                    let mut items = Vec::with_capacity(args.len() + 1);
                    items.push(Sexp::Sym(keyword));
                    items.extend(args);
                    Ok(Sexp::List(items))
                }
                None => Ok(Sexp::List(args)),
            }
        }
        ValueKind::Struct(_) | ValueKind::Func(_) => {
            Err(emit_invalid("struct and function values have no s-expression"))
        }
    }
}

fn terms(arena: &Arena, values: &[Value]) -> Result<Vec<Sexp>> {
    values.iter().map(|value| term(arena, *value)).collect()
}

fn json_text(json: &json) -> String {
    serde_json::to_string(json).expect("JSON values serialize")
}

// == Entry points

/// Prints a `script` meta value, one definition per line.
pub fn script(arena: &Arena, value: Value) -> Result<String> {
    let ValueKind::List(defns) = arena.value.kind(value.node) else {
        return Err(emit_invalid("a script is a list of definitions"));
    };
    let mut out = String::with_capacity(1 << 20);
    out.push('(');
    for defn in defns {
        out.push('\n');
        term(arena, *defn)?.write(&mut out);
    }
    out.push_str("\n)");
    Ok(out)
}

/// Prints a `val` meta value.
pub fn val(arena: &Arena, value: Value) -> Result<String> {
    let mut out = String::with_capacity(1 << 20);
    term(arena, value)?.write(&mut out);
    Ok(out)
}
