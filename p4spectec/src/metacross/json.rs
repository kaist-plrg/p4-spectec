//! The JSON of the host boundary
//!
//! Shared with `spec-meta-k/al/4.1-extern-json.k` and
//! `spec-meta-redex/common/0.1-extern-json.rkt`:
//!
//! ```text
//! val      ::= ["boolV", bool] | ["natN", "<decimal>"] | ["intN", "<decimal>"]
//!            | ["textV", text] | ["strV", [[atom, val]...]]
//!            | ["injV", mixop, [val...]] | ["tupV", [val...]]
//!            | ["optV", null | val] | ["listV", [val...]]
//!            | ["funcV", id] | ["extV", json]
//! mixop    ::= [[atom...]...]                  atoms between argument positions
//! typ      ::= ["boolT"] | ["natT"] | ["intT"] | ["textT"]
//!            | ["varT", id, [typ...]] | ["tupT", [typ...]]
//!            | ["iterT", typ, "?" | "*"] | ["funcT"]
//! request  ::= {"builtin": id, "targs": [typ...], "args": [val...]}
//!            | {"extern-func": id, "targs": [typ...], "args": [val...]}
//!            | {"extern-rel": id, "args": [val...]}
//! response ::= {"ok": val} | {"ok": [val...]} | {"fail": null} | {"error": text}
//! ```
//!
//! `ToJson` reads the arena and `FromJson` allocates in it. Atoms travel in
//! their printed spelling. Decoded values carry the placeholder type `_`
//! where the wire has no type; tuples, options and lists take their type
//! from their elements.

use std::rc::Rc;

use num_bigint::BigInt;
use serde_json::Map;

use crate::{
    frontend::parse::parse_atom,
    lang::{
        common::{
            Id, Iter,
            prim::num::{self, Natural, Number},
            source::Span,
        },
        data::{
            arena::Arena,
            notation::{
                AtomPhrase,
                flat::{Mixfix, Mixop, parse::ToMixop},
                items::Item,
            },
            typ::{self, Typ, TypKind},
            value::flat::{Value, ValueField, ValueKind, make},
        },
        traits::print::Print,
    },
    phrase,
    util::json::json,
};

use super::{Error, Result};

// == The traits

/// The JSON of host data, read from the arena; encoding never allocates in it.
pub trait ToJson {
    fn to_json(&self, arena: &Arena) -> json;
}

/// Host data decoded from JSON into the arena.
pub trait FromJson: Sized {
    fn from_json(arena: &mut Arena, json: &json) -> Result<Self>;
}

// - Lists

impl<T: ToJson> ToJson for [T] {
    fn to_json(&self, arena: &Arena) -> json {
        json::Array(self.iter().map(|item| item.to_json(arena)).collect())
    }
}

impl<T: ToJson> ToJson for Vec<T> {
    fn to_json(&self, arena: &Arena) -> json {
        self[..].to_json(arena)
    }
}

impl<T: FromJson> FromJson for Vec<T> {
    fn from_json(arena: &mut Arena, json: &json) -> Result<Self> {
        let json::Array(items) = json else {
            return Err(Error::unexpected("a list", json));
        };
        items.iter().map(|item| T::from_json(arena, item)).collect()
    }
}

// == Tagged arrays

/// `[tag, rest...]`.
fn tag(tag: &str, rest: impl IntoIterator<Item = json>) -> json {
    json::Array(
        std::iter::once(json::String(tag.into()))
            .chain(rest)
            .collect(),
    )
}

/// The tag and the rest of `[tag, rest...]`.
fn untag<'a>(expected: &'static str, json: &'a json) -> Result<(&'a str, &'a [json])> {
    if let json::Array(items) = json
        && let [json::String(tag), rest @ ..] = &items[..]
    {
        return Ok((tag, rest));
    }
    Err(Error::unexpected(expected, json))
}

// == Atoms and mixops

impl ToJson for AtomPhrase {
    fn to_json(&self, _arena: &Arena) -> json {
        json::String(Print::to_string(&self.node))
    }
}

impl FromJson for AtomPhrase {
    fn from_json(_arena: &mut Arena, json: &json) -> Result<Self> {
        let json::String(text) = json else {
            return Err(Error::unexpected("an atom", json));
        };
        let atom = parse_atom(text)
            .map_err(Error::Syntax)?
            .ok_or_else(|| Error::Atom(text.clone()))?;
        Ok(phrase!(node: atom, span: Span::default()))
    }
}

/// Atom rows between argument positions.
impl ToJson for Mixop {
    fn to_json(&self, arena: &Arena) -> json {
        let mut rows = vec![vec![]];
        self.visit(&arena.mixop, |item| match item {
            Item::Arg(_) => rows.push(vec![]),
            Item::Atom(atom) => rows
                .last_mut()
                .expect("rows start nonempty")
                .push(atom.to_json(arena)),
        });
        json::Array(rows.into_iter().map(json::Array).collect())
    }
}

impl FromJson for Mixop {
    fn from_json(arena: &mut Arena, json: &json) -> Result<Self> {
        let matrix = Vec::<Vec<AtomPhrase>>::from_json(arena, json)?;
        matrix[..]
            .to_mixop(&mut arena.mixop)
            .map_err(|error| Error::Mixop(error.to_string()))
    }
}

// == Values

impl ToJson for Value {
    fn to_json(&self, arena: &Arena) -> json {
        match arena.value.kind(self.node) {
            ValueKind::Bool(value) => tag("boolV", [json::Bool(*value)]),
            ValueKind::Num(Number::Nat(nat)) => tag("natN", [nat.to_string().into()]),
            ValueKind::Num(Number::Int(int)) => tag("intN", [int.to_string().into()]),
            ValueKind::Text(text) => tag("textV", [text.clone().into()]),
            ValueKind::Struct(fields) => tag("strV", [fields.to_json(arena)]),
            ValueKind::Case(case) => {
                tag("injV", [case.mixop().to_json(arena), case.args().to_json(arena)])
            }
            ValueKind::Tuple(values) => tag("tupV", [values.to_json(arena)]),
            ValueKind::Opt(value) => {
                tag("optV", [value.map_or(json::Null, |value| value.to_json(arena))])
            }
            ValueKind::List(values) => tag("listV", [values.to_json(arena)]),
            ValueKind::Func(id) => tag("funcV", [id.node.to_string().into()]),
            ValueKind::Extern(json) => tag("extV", [(**json).clone()]),
        }
    }
}

impl ToJson for ValueField {
    fn to_json(&self, arena: &Arena) -> json {
        json::Array(vec![self.atom.to_json(arena), self.value.to_json(arena)])
    }
}

// - The decoder's typing

fn id_of(name: &str) -> Id {
    phrase!(node: name.into(), span: Span::default())
}

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

fn natural(text: &str) -> Result<Natural> {
    let unexpected = || Error::Unexpected { expected: "a natural number", found: text.into() };
    let int = text.parse::<BigInt>().map_err(|_| unexpected())?;
    Natural::try_from(int).map_err(|_| unexpected())
}

fn integer(text: &str) -> Result<BigInt> {
    text.parse()
        .map_err(|_| Error::Unexpected { expected: "an integer", found: text.into() })
}

impl FromJson for Value {
    fn from_json(arena: &mut Arena, json: &json) -> Result<Self> {
        let span = Span::default();
        let value = match untag("a value", json)? {
            ("boolV", [json::Bool(value)]) => make::bool(arena, *value, span)?,
            ("natN", [json::String(text)]) => make::nat(arena, natural(text)?, span)?,
            ("intN", [json::String(text)]) => make::int(arena, integer(text)?, span)?,
            ("textV", [json::String(text)]) => make::text(arena, text.clone(), span)?,
            ("strV", [fields]) => {
                let fields = Vec::<ValueField>::from_json(arena, fields)?;
                make::structure(arena, placeholder(), fields, span)?
            }
            ("injV", [mixop, args]) => {
                let mixop = Mixop::from_json(arena, mixop)?;
                let args = Vec::<Value>::from_json(arena, args)?;
                let mixfix = Mixfix::new(&arena.mixop, mixop, args)?;
                make::case_flat(arena, placeholder(), mixfix, span)?
            }
            ("tupV", [items]) => {
                let values = Vec::<Value>::from_json(arena, items)?;
                let typ =
                    typ::make::tuple(values.iter().map(|value| typ_of(arena, *value)).collect());
                make::tuple(arena, Rc::new(typ.node), values, span)?
            }
            ("optV", [json::Null]) => {
                let typ = typ::make::opt(placeholder_typ());
                make::opt(arena, Rc::new(typ.node), None, span)?
            }
            ("optV", [inner]) => {
                let value = Value::from_json(arena, inner)?;
                let typ = typ::make::opt(typ_of(arena, value));
                make::opt(arena, Rc::new(typ.node), Some(value), span)?
            }
            ("listV", [items]) => {
                let values = Vec::<Value>::from_json(arena, items)?;
                let typ_elem = values
                    .first()
                    .map_or_else(placeholder_typ, |value| typ_of(arena, *value));
                let typ = typ::make::list(typ_elem);
                make::list(arena, Rc::new(typ.node), values, span)?
            }
            ("funcV", [json::String(id)]) => {
                Value::new(arena, ValueKind::Func(id_of(id)), placeholder(), span)?
            }
            ("extV", [json]) => make::external(arena, placeholder(), Rc::new(json.clone()), span)?,
            _ => return Err(Error::unexpected("a value", json)),
        };
        Ok(value)
    }
}

impl FromJson for ValueField {
    fn from_json(arena: &mut Arena, json: &json) -> Result<Self> {
        if let json::Array(items) = json
            && let [atom, value] = &items[..]
        {
            return Ok(Self {
                atom: AtomPhrase::from_json(arena, atom)?,
                value: Value::from_json(arena, value)?,
            });
        }
        Err(Error::unexpected("a struct field", json))
    }
}

// == Types

impl ToJson for Typ {
    fn to_json(&self, arena: &Arena) -> json {
        match &self.node {
            TypKind::Bool => tag("boolT", []),
            TypKind::Num(num::Typ::Nat) => tag("natT", []),
            TypKind::Num(num::Typ::Int) => tag("intT", []),
            TypKind::Text => tag("textT", []),
            TypKind::Var(id, targs) => {
                tag("varT", [id.node.to_string().into(), targs.to_json(arena)])
            }
            TypKind::Tuple(typs) => tag("tupT", [typs.to_json(arena)]),
            TypKind::Iter(typ, iter) => {
                let iter = match iter {
                    Iter::Opt => "?",
                    Iter::List => "*",
                };
                tag("iterT", [typ.to_json(arena), iter.into()])
            }
            TypKind::Func(_) => tag("funcT", []),
        }
    }
}

/// Types need no arena; the parameter only serves `Vec<Typ>`.
impl FromJson for Typ {
    fn from_json(arena: &mut Arena, json: &json) -> Result<Self> {
        Ok(match untag("a type", json)? {
            ("boolT", []) => typ::make::bool(),
            ("natT", []) => typ::make::nat(),
            ("intT", []) => typ::make::int(),
            ("textT", []) => typ::make::text(),
            ("varT", [json::String(id), targs]) => {
                typ::make::var(id_of(id), Vec::<Typ>::from_json(arena, targs)?)
            }
            ("tupT", [typs]) => typ::make::tuple(Vec::<Typ>::from_json(arena, typs)?),
            ("iterT", [typ, json::String(iter)]) => {
                let iter = match iter.as_str() {
                    "?" => Iter::Opt,
                    "*" => Iter::List,
                    _ => return Err(Error::unexpected("a type", json)),
                };
                typ::make::iter(Typ::from_json(arena, typ)?, iter)
            }
            ("funcT", []) => {
                return Err(Error::Request(
                    "a function type cannot cross the host boundary".into(),
                ));
            }
            _ => return Err(Error::unexpected("a type", json)),
        })
    }
}

// == Requests and replies

/// A decoded host request.
pub enum Request {
    /// A builtin of the interface, resolved whether or not the specification declares it.
    Builtin { id: Id, targs: Vec<Typ>, args: Vec<Value> },
    /// A function of the loaded specification.
    ExternFunc { id: Id, targs: Vec<Typ>, args: Vec<Value> },
    /// A relation of the loaded specification.
    ExternRel { id: Id, args: Vec<Value> },
}

fn field<'a>(fields: &'a Map<String, json>, name: &str) -> Result<&'a json> {
    fields
        .get(name)
        .ok_or_else(|| Error::Request(format!("the request is missing the field {name}")))
}

impl FromJson for Request {
    fn from_json(arena: &mut Arena, json: &json) -> Result<Self> {
        let json::Object(fields) = json else {
            return Err(Error::unexpected("a request object", json));
        };
        let (kind, id) =
            match (fields.get("builtin"), fields.get("extern-func"), fields.get("extern-rel")) {
                (Some(id), None, None) => ("builtin", id),
                (None, Some(id), None) => ("extern-func", id),
                (None, None, Some(id)) => ("extern-rel", id),
                _ => {
                    return Err(Error::Request(
                    "a request has exactly one of the fields builtin, extern-func and extern-rel"
                        .into(),
                ));
                }
            };
        let json::String(id) = id else {
            return Err(Error::Request(format!("expected a name for {kind}")));
        };
        let id = id_of(id);
        let args = Vec::<Value>::from_json(arena, field(fields, "args")?)?;
        if kind == "extern-rel" {
            return Ok(Self::ExternRel { id, args });
        }
        let targs = Vec::<Typ>::from_json(arena, field(fields, "targs")?)?;
        Ok(if kind == "builtin" {
            Self::Builtin { id, targs, args }
        } else {
            Self::ExternFunc { id, targs, args }
        })
    }
}

/// A reply the host can give; an error is `Err` until `host::eval` renders it.
pub enum Reply {
    /// `{"ok": val}`, for a builtin or extern function.
    Ok(Value),
    /// `{"ok": [val...]}`, for an extern relation.
    OkAll(Vec<Value>),
    /// `{"fail": null}`: the specification's extern found no derivation.
    Fail,
}

fn object(key: &str, value: json) -> json {
    let mut fields = Map::with_capacity(1);
    fields.insert(key.into(), value);
    json::Object(fields)
}

impl ToJson for Reply {
    fn to_json(&self, arena: &Arena) -> json {
        match self {
            Self::Ok(value) => object("ok", value.to_json(arena)),
            Self::OkAll(values) => object("ok", values.to_json(arena)),
            Self::Fail => object("fail", json::Null),
        }
    }
}

/// `{"error": text}`: the host could not answer. It needs no arena.
pub fn error(message: &str) -> json {
    object("error", message.into())
}
