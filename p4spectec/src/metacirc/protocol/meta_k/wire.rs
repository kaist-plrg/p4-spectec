//! The JSON of spec-meta-k's boundary
//!
//! Shared with `spec-meta-k/3-execution/3.04.1-extern-json.k`:
//!
//! ```text
//! val      ::= ["boolV", bool] | ["natN", "<decimal>"] | ["intN", "<decimal>"]
//!            | ["textV", text] | ["strV", [[atom, val]...]]
//!            | ["injV", mixop, [val...]] | ["tupV", [val...]]
//!            | ["optV", null | val] | ["listV", [val...]]
//!            | ["funcV", id] | ["extV", json]
//! mixop    ::= [[atom...]...]                  atoms between argument positions
//! typ      ::= ["boolT"] | ["natT"] | ["intT"] | ["textT"]
//!            | ["varT", id, [typ...]] | ["tupT", [typ...]] | ["iterT", typ, "?" | "*"]
//!            | ["funcT", [id...], [typ...], typ]
//! ```
//!
//! `Writer` prints JSON directly; decoding reads `serde_json` values into the arena.
//! Atoms travel in their printed spelling. A decoded value has the placeholder
//! type `_` where the wire has none; tuples, options, and lists take theirs
//! from their elements.

use super::Cache;
use crate::{
    frontend::parse::parse_typ,
    lang::{
        common::{
            Id, Iter,
            notation::atom::Atom,
            prim::num::{self, Natural, Number},
            source::Span,
        },
        data::{
            arena::Arena,
            notation::{
                AtomPhrase,
                flat::{Mixfix, Mixop, parse::ToMixop},
                items::Item,
                tree,
            },
            typ::{self, Typ, TypKind},
            value::flat::{Value, ValueField, ValueKind, make},
        },
        el::ast as el,
        traits::print::Print,
    },
    phrase,
    runner::ExternError,
    util::json::json,
};
use num_bigint::BigInt;
use serde::Deserialize;
use std::{fmt::Write as _, rc::Rc};

// == Printing

/// JSON text, printed in order; commas go between the items of an array or object.
pub(super) struct Writer {
    text: String,
    first: bool,
}

impl Writer {
    pub(super) fn new() -> Self {
        Self { text: String::new(), first: true }
    }

    pub(super) fn finish(self) -> String {
        self.text
    }

    /// Starts an item, after a comma unless it is the first.
    fn item(&mut self) {
        if !self.first {
            self.text.push(',');
        }
        self.first = false;
    }

    pub(super) fn null(&mut self) {
        self.item();
        self.text.push_str("null");
    }

    pub(super) fn bool(&mut self, value: bool) {
        self.item();
        self.text.push_str(if value { "true" } else { "false" });
    }

    pub(super) fn number(&mut self, number: impl std::fmt::Display) {
        self.item();
        write!(self.text, "{number}").expect("writing to a string succeeds");
    }

    pub(super) fn text(&mut self, text: &str) {
        self.item();
        escape(&mut self.text, text);
    }

    /// An item that is already JSON text.
    pub(super) fn json(&mut self, text: &str) {
        self.item();
        self.text.push_str(text);
    }

    pub(super) fn array(&mut self, body: impl FnOnce(&mut Self)) {
        self.item();
        self.text.push('[');
        self.first = true;
        body(self);
        self.text.push(']');
        self.first = false;
    }

    /// `[tag, ...]`, the rest printed by `body`.
    pub(super) fn tag(&mut self, tag: &str, body: impl FnOnce(&mut Self)) {
        self.array(|writer| {
            writer.text(tag);
            body(writer);
        });
    }

    /// An array of `items`, each printed by `each`.
    pub(super) fn list<T>(
        &mut self,
        items: impl IntoIterator<Item = T>,
        mut each: impl FnMut(&mut Self, T),
    ) {
        self.array(|writer| items.into_iter().for_each(|item| each(writer, item)));
    }

    /// An object; `body` prints each field with `key` before its value.
    pub(super) fn object(&mut self, body: impl FnOnce(&mut Self)) {
        self.item();
        self.text.push('{');
        self.first = true;
        body(self);
        self.text.push('}');
        self.first = false;
    }

    pub(super) fn key(&mut self, key: &str) {
        self.item();
        escape(&mut self.text, key);
        self.text.push(':');
        self.first = true;
    }
}

/// Appends `text` as a JSON string.
fn escape(out: &mut String, text: &str) {
    out.push('"');
    for ch in text.chars() {
        match ch {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            ch if (ch as u32) < 0x20 => {
                write!(out, "\\u{:04x}", ch as u32).expect("writing to a string succeeds");
            }
            ch => out.push(ch),
        }
    }
    out.push('"');
}

// - Notation

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
pub(super) fn rows_tree(mixop: &tree::Mixop) -> Vec<Vec<&AtomPhrase>> {
    rows(|visit_item| mixop.visit(visit_item))
}

pub(super) fn atom(writer: &mut Writer, atom: &AtomPhrase) {
    writer.text(&Print::to_string(&atom.node));
}

pub(super) fn mixop_rows(writer: &mut Writer, rows: Vec<Vec<&AtomPhrase>>) {
    writer.list(rows, |writer, row| writer.list(row, atom));
}

/// The atom rows of a value's notation, printed once per notation.
fn mixop(writer: &mut Writer, arena: &Arena, cache: &mut Cache, mixop: Mixop) {
    if let Some(text) = cache.mixops_out.get(&mixop) {
        writer.json(text);
        return;
    }
    let mut writer_mixop = Writer::new();
    mixop_rows(&mut writer_mixop, rows(|visit_item| mixop.visit(&arena.mixop, visit_item)));
    let text = writer_mixop.finish();
    writer.json(&text);
    if cache.enabled {
        cache.mixops_out.insert(mixop, text);
    }
}

// - Types and values

pub(super) fn id(writer: &mut Writer, id: &Id) {
    writer.text(&id.node);
}

pub(super) fn iter(writer: &mut Writer, iter: &Iter) {
    writer.text(match iter {
        Iter::Opt => "?",
        Iter::List => "*",
    });
}

pub(super) fn typ(writer: &mut Writer, typ: &Typ) {
    match &typ.node {
        TypKind::Bool => writer.tag("boolT", |_| {}),
        TypKind::Num(num::Typ::Nat) => writer.tag("natT", |_| {}),
        TypKind::Num(num::Typ::Int) => writer.tag("intT", |_| {}),
        TypKind::Text => writer.tag("textT", |_| {}),
        TypKind::Var(id_typ, targs) => writer.tag("varT", |writer| {
            id(writer, id_typ);
            writer.list(targs, self::typ);
        }),
        TypKind::Tuple(typs) => writer.tag("tupT", |writer| writer.list(typs, self::typ)),
        TypKind::Iter(typ_inner, iter_typ) => writer.tag("iterT", |writer| {
            self::typ(writer, typ_inner);
            iter(writer, iter_typ);
        }),
        TypKind::Func(func) => writer.tag("funcT", |writer| {
            writer.list(&func.tparams, id);
            writer.list(&func.typs_params, self::typ);
            self::typ(writer, &func.typ_ret);
        }),
    }
}

pub(super) fn number(writer: &mut Writer, number: &Number) {
    match number {
        Number::Nat(nat) => writer.tag("natN", |writer| writer.text(&nat.to_string())),
        Number::Int(int) => writer.tag("intN", |writer| writer.text(&int.to_string())),
    }
}

pub(super) fn value(writer: &mut Writer, arena: &Arena, cache: &mut Cache, value: Value) {
    match arena.value.kind(value.node) {
        ValueKind::Bool(value) => writer.tag("boolV", |writer| writer.bool(*value)),
        ValueKind::Num(num) => number(writer, num),
        ValueKind::Text(text) => writer.tag("textV", |writer| writer.text(text)),
        ValueKind::Struct(fields) => writer.tag("strV", |writer| {
            writer.list(fields, |writer, field| {
                writer.array(|writer| {
                    atom(writer, &field.atom);
                    self::value(writer, arena, cache, field.value);
                });
            });
        }),
        ValueKind::Case(case) => writer.tag("injV", |writer| {
            mixop(writer, arena, cache, *case.mixop());
            values(writer, arena, cache, case.args());
        }),
        ValueKind::Tuple(items) => writer.tag("tupV", |writer| values(writer, arena, cache, items)),
        ValueKind::Opt(item) => writer.tag("optV", |writer| match item {
            None => writer.null(),
            Some(item) => self::value(writer, arena, cache, *item),
        }),
        ValueKind::List(items) => writer.tag("listV", |writer| values(writer, arena, cache, items)),
        ValueKind::Func(id_func) => writer.tag("funcV", |writer| id(writer, id_func)),
        ValueKind::Extern(json) => writer.tag("extV", |writer| writer.json(&json.to_string())),
    }
}

pub(super) fn values(writer: &mut Writer, arena: &Arena, cache: &mut Cache, values: &[Value]) {
    writer.list(values, |writer, item| value(writer, arena, cache, *item));
}

// == Reading

/// Parses JSON text of any depth.
pub(super) fn parse(text: &str) -> Result<json, ExternError> {
    let mut deserializer = serde_json::Deserializer::from_str(text);
    deserializer.disable_recursion_limit();
    let json = json::deserialize(&mut deserializer)?;
    deserializer.end()?;
    Ok(json)
}

/// Reports JSON of another shape than the wire grammar gives.
pub(super) fn unexpected(expected: &str, found: &json) -> ExternError {
    let mut found = found.to_string();
    if found.len() > 200 {
        let end = (0..=200)
            .rev()
            .find(|end| found.is_char_boundary(*end))
            .unwrap_or(0);
        found.truncate(end);
        found.push_str("...");
    }
    ExternError::diagnostic_message(format!(
        "spec-meta-k's JSON: expected {expected}, but got {found}"
    ))
}

/// The tag and the rest of `[tag, rest...]`.
fn untag<'a>(expected: &str, json: &'a json) -> Result<(&'a str, &'a [json]), ExternError> {
    if let json::Array(items) = json
        && let [json::String(tag), rest @ ..] = &items[..]
    {
        return Ok((tag, rest));
    }
    Err(unexpected(expected, json))
}

/// The items of an array.
pub(super) fn items<'a>(expected: &str, json: &'a json) -> Result<&'a [json], ExternError> {
    match json {
        json::Array(items) => Ok(items),
        _ => Err(unexpected(expected, json)),
    }
}

fn id_of(name: &str) -> Id {
    phrase!(node: name.into(), span: Span::default())
}

pub(super) fn read_id(json: &json) -> Result<Id, ExternError> {
    match json {
        json::String(name) => Ok(id_of(name)),
        _ => Err(unexpected("an identifier", json)),
    }
}

// - Notation

/// One notation from its atom rows, interned once per spelling.
fn read_mixop(arena: &mut Arena, cache: &mut Cache, json: &json) -> Result<Mixop, ExternError> {
    let rows = items("a mixop", json)?
        .iter()
        .map(|row| {
            items("a row of atoms", row)?
                .iter()
                .map(|atom| match atom {
                    json::String(text) => Ok(text.as_str()),
                    _ => Err(unexpected("an atom", atom)),
                })
                .collect::<Result<Vec<_>, _>>()
        })
        .collect::<Result<Vec<_>, _>>()?;
    // The rows printed as `[Vec<AtomPhrase>]::to_mixop` prints them, a placeholder at each hole
    let text = rows
        .iter()
        .map(|row| row.join(" "))
        .collect::<Vec<_>>()
        .join(" nat ");
    if let Some(mixop) = cache.mixops_in.get(&text) {
        return Ok(*mixop);
    }
    let failed = |error: String| {
        ExternError::diagnostic_message(format!(
            "spec-meta-k's JSON: invalid mixop {text}: {error}"
        ))
    };
    // [[]] is the empty notation, which has no text to parse
    let mixop = if let [row] = &rows[..]
        && row.is_empty()
    {
        [Vec::<AtomPhrase>::new()][..]
            .to_mixop(&mut arena.mixop)
            .map_err(|error| failed(error.to_string()))?
    } else {
        let mixop = text
            .as_str()
            .to_mixop(&mut arena.mixop)
            .map_err(|error| failed(error.to_string()))?;
        if arena.mixop.arity(mixop) + 1 != rows.len() {
            return Err(failed("the rows do not match its holes".into()));
        }
        mixop
    };
    if cache.enabled {
        cache.mixops_in.insert(text, mixop);
    }
    Ok(mixop)
}

// - Values

/// The type of values the wire does not type.
fn placeholder_typ() -> Typ {
    typ::make::var(id_of("_"), vec![])
}

fn placeholder() -> Rc<TypKind> {
    Rc::new(placeholder_typ().node)
}

fn typ_of(arena: &Arena, value: Value) -> Typ {
    phrase!(node: (**arena.value.typ(value.note)).clone(), span: Span::default())
}

fn decimal(json: &json) -> Result<BigInt, ExternError> {
    match json {
        json::String(text) => text.parse().map_err(|_| unexpected("a decimal", json)),
        _ => Err(unexpected("a decimal", json)),
    }
}

pub(super) fn read_value(
    arena: &mut Arena,
    cache: &mut Cache,
    json: &json,
) -> Result<Value, ExternError> {
    let span = Span::default();
    let value = match untag("a value", json)? {
        ("boolV", [json::Bool(value)]) => make::bool(arena, *value, span)?,
        ("natN", [digits]) => {
            let nat = Natural::try_from(decimal(digits)?)
                .map_err(|_| unexpected("a natural number", digits))?;
            make::nat(arena, nat, span)?
        }
        ("intN", [digits]) => make::int(arena, decimal(digits)?, span)?,
        ("textV", [json::String(text)]) => make::text(arena, text.clone(), span)?,
        ("strV", [fields]) => {
            let fields = items("struct fields", fields)?
                .iter()
                .map(|field| match field {
                    json::Array(parts) => match &parts[..] {
                        [json::String(text), value] => Ok(ValueField {
                            atom: read_atom(cache, text)?,
                            value: read_value(arena, cache, value)?,
                        }),
                        _ => Err(unexpected("a struct field", field)),
                    },
                    _ => Err(unexpected("a struct field", field)),
                })
                .collect::<Result<Vec<_>, _>>()?;
            make::structure(arena, placeholder(), fields, span)?
        }
        ("injV", [mixop, args]) => {
            let mixop = read_mixop(arena, cache, mixop)?;
            let args = read_values(arena, cache, args)?;
            let mixfix = Mixfix::new(&arena.mixop, mixop, args)
                .map_err(|error| ExternError::diagnostic_message(error.to_string()))?;
            make::case_flat(arena, placeholder(), mixfix, span)?
        }
        ("tupV", [items]) => {
            let values = read_values(arena, cache, items)?;
            let typ = typ::make::tuple(values.iter().map(|value| typ_of(arena, *value)).collect());
            make::tuple(arena, Rc::new(typ.node), values, span)?
        }
        ("optV", [json::Null]) => {
            let typ = typ::make::opt(placeholder_typ());
            make::opt(arena, Rc::new(typ.node), None, span)?
        }
        ("optV", [item]) => {
            let value = read_value(arena, cache, item)?;
            let typ = typ::make::opt(typ_of(arena, value));
            make::opt(arena, Rc::new(typ.node), Some(value), span)?
        }
        ("listV", [items]) => {
            let values = read_values(arena, cache, items)?;
            let typ_elem = values
                .first()
                .map_or_else(placeholder_typ, |value| typ_of(arena, *value));
            let typ = typ::make::list(typ_elem);
            make::list(arena, Rc::new(typ.node), values, span)?
        }
        ("funcV", [id]) => Value::new(arena, ValueKind::Func(read_id(id)?), placeholder(), span)?,
        ("extV", [json]) => make::external(arena, placeholder(), Rc::new(json.clone()), span)?,
        _ => return Err(unexpected("a value", json)),
    };
    Ok(value)
}

pub(super) fn read_values(
    arena: &mut Arena,
    cache: &mut Cache,
    json: &json,
) -> Result<Vec<Value>, ExternError> {
    items("a list of values", json)?
        .iter()
        .map(|item| read_value(arena, cache, item))
        .collect()
}

/// One atom from its printed spelling, as a struct field names it, parsed once per spelling.
fn read_atom(cache: &mut Cache, text: &str) -> Result<AtomPhrase, ExternError> {
    if let Some(atom) = cache.atoms_in.get(text) {
        return Ok(atom.clone());
    }
    // Delimiter atoms do not parse outside their matching pair
    let atom = match text {
        "`<" => Atom::LAngle,
        "`>" => Atom::RAngle,
        "`(" => Atom::LParen,
        "`)" => Atom::RParen,
        "`[" => Atom::LBrack,
        "`]" => Atom::RBrack,
        "`{" => Atom::LBrace,
        "`}" => Atom::RBrace,
        _ => match parse_typ(text)? {
            el::Typ::Notation(notation) => match notation.node {
                el::NotTypKind::Atom(atom) => atom.node,
                _ => return Err(unexpected("an atom", &json::String(text.into()))),
            },
            el::Typ::Plain(_) => return Err(unexpected("an atom", &json::String(text.into()))),
        },
    };
    let atom = phrase!(node: atom, span: Span::default());
    if cache.enabled {
        cache.atoms_in.insert(text.into(), atom.clone());
    }
    Ok(atom)
}

// - Types

pub(super) fn read_typ(json: &json) -> Result<Typ, ExternError> {
    Ok(match untag("a type", json)? {
        ("boolT", []) => typ::make::bool(),
        ("natT", []) => typ::make::nat(),
        ("intT", []) => typ::make::int(),
        ("textT", []) => typ::make::text(),
        ("varT", [id, targs]) => typ::make::var(read_id(id)?, read_typs(targs)?),
        ("tupT", [typs]) => typ::make::tuple(read_typs(typs)?),
        ("iterT", [typ, json::String(iter)]) => {
            let iter = match iter.as_str() {
                "?" => Iter::Opt,
                "*" => Iter::List,
                _ => return Err(unexpected("a type", json)),
            };
            typ::make::iter(read_typ(typ)?, iter)
        }
        ("funcT", [tparams, typs, typ]) => {
            let tparams = items("type parameters", tparams)?
                .iter()
                .map(read_id)
                .collect::<Result<_, _>>()?;
            typ::make::func(tparams, read_typs(typs)?, read_typ(typ)?)
        }
        _ => return Err(unexpected("a type", json)),
    })
}

pub(super) fn read_typs(json: &json) -> Result<Vec<Typ>, ExternError> {
    items("a list of types", json)?
        .iter()
        .map(read_typ)
        .collect()
}
