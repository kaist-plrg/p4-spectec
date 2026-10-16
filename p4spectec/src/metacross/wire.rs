//! The JSON wire of the host boundary
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
//! Atoms travel in their printed spelling. Decoded values carry the
//! placeholder type `_` where the wire has no type; tuples, options and lists
//! take their type from their elements.

use std::{rc::Rc, str::FromStr};

use num_bigint::BigInt;
use serde_json::{Map, Value as Json};

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
            value::{
                ValueError,
                flat::{Value, ValueField, ValueKind, make},
            },
        },
        traits::print::Print,
    },
    phrase,
};

use super::{Result, error::wire_invalid};

// == Requests and responses

/// A decoded host request.
pub enum Request {
    /// A builtin of the interface, resolved whether or not the specification declares it.
    Builtin { id: Id, targs: Vec<Typ>, args: Vec<Value> },
    /// A function of the loaded specification.
    ExternFunc { id: Id, targs: Vec<Typ>, args: Vec<Value> },
    /// A relation of the loaded specification.
    ExternRel { id: Id, args: Vec<Value> },
}

/// Decodes a request, allocating its arguments in the arena.
pub fn request_of_json(arena: &mut Arena, text: &str) -> Result<Request> {
    let json: Json = serde_json::from_str(text)
        .map_err(|error| wire_invalid(format!("malformed request JSON: {error}")))?;
    let Json::Object(fields) = &json else {
        return Err(unexpected("a request object", &json));
    };
    let kinds: Vec<&str> = ["builtin", "extern-func", "extern-rel"]
        .into_iter()
        .filter(|kind| fields.contains_key(*kind))
        .collect();
    let [kind] = kinds[..] else {
        return Err(wire_invalid(
            "a request has exactly one of the fields builtin, extern-func and extern-rel",
        ));
    };
    let Some(Json::String(id)) = fields.get(kind) else {
        return Err(wire_invalid(format!("expected a name for {kind}")));
    };
    let id = id_of(id);
    let args = vals_of_json(arena, field(fields, "args")?)?;
    Ok(match kind {
        "extern-rel" => Request::ExternRel { id, args },
        _ => {
            let targs = typs_of_json(field(fields, "targs")?)?;
            if kind == "builtin" {
                Request::Builtin { id, targs, args }
            } else {
                Request::ExternFunc { id, targs, args }
            }
        }
    })
}

fn field<'a>(fields: &'a Map<String, Json>, name: &str) -> Result<&'a Json> {
    fields
        .get(name)
        .ok_or_else(|| wire_invalid(format!("the request is missing the field {name}")))
}

fn response(key: &str, value: Json) -> Json {
    let mut fields = Map::with_capacity(1);
    fields.insert(key.into(), value);
    Json::Object(fields)
}

/// `{"ok": val}`, for a builtin or extern function.
pub fn response_ok(arena: &Arena, value: Value) -> Json {
    response("ok", json_of_val(arena, value))
}

/// `{"ok": [val...]}`, for an extern relation.
pub fn response_ok_values(arena: &Arena, values: &[Value]) -> Json {
    response("ok", json_of_vals(arena, values))
}

/// `{"fail": null}`: the specification's extern found no derivation.
pub fn response_fail() -> Json {
    response("fail", Json::Null)
}

/// `{"error": text}`: the host could not answer.
pub fn response_error(message: &str) -> Json {
    response("error", Json::String(message.into()))
}

// == Encoding

fn tagged(tag: &str, rest: Vec<Json>) -> Json {
    let mut items = Vec::with_capacity(rest.len() + 1);
    items.push(Json::String(tag.into()));
    items.extend(rest);
    Json::Array(items)
}

fn json_of_atom(atom: &AtomPhrase) -> Json {
    Json::String(Print::to_string(&atom.node))
}

/// Atom rows between argument positions.
fn json_of_mixop(arena: &Arena, mixop: Mixop) -> Json {
    let mut rows = vec![vec![]];
    mixop.visit(&arena.mixop, |item| match item {
        Item::Arg(_) => rows.push(vec![]),
        Item::Atom(atom) => rows
            .last_mut()
            .expect("rows start nonempty")
            .push(json_of_atom(atom)),
    });
    Json::Array(rows.into_iter().map(Json::Array).collect())
}

/// The JSON of a runtime value.
pub fn json_of_val(arena: &Arena, value: Value) -> Json {
    match arena.value.kind(value.node) {
        ValueKind::Bool(value) => tagged("boolV", vec![Json::Bool(*value)]),
        ValueKind::Num(Number::Nat(nat)) => tagged("natN", vec![Json::String(nat.to_string())]),
        ValueKind::Num(Number::Int(int)) => tagged("intN", vec![Json::String(int.to_string())]),
        ValueKind::Text(text) => tagged("textV", vec![Json::String(text.clone())]),
        ValueKind::Struct(fields) => {
            let fields = fields
                .iter()
                .map(|field| {
                    Json::Array(vec![json_of_atom(&field.atom), json_of_val(arena, field.value)])
                })
                .collect();
            tagged("strV", vec![Json::Array(fields)])
        }
        ValueKind::Case(case) => tagged(
            "injV",
            vec![json_of_mixop(arena, *case.mixop()), json_of_vals(arena, case.args())],
        ),
        ValueKind::Tuple(values) => tagged("tupV", vec![json_of_vals(arena, values)]),
        ValueKind::Opt(None) => tagged("optV", vec![Json::Null]),
        ValueKind::Opt(Some(value)) => tagged("optV", vec![json_of_val(arena, *value)]),
        ValueKind::List(values) => tagged("listV", vec![json_of_vals(arena, values)]),
        ValueKind::Func(id) => tagged("funcV", vec![Json::String(id.node.to_string())]),
        ValueKind::Extern(json) => tagged("extV", vec![(**json).clone()]),
    }
}

fn json_of_vals(arena: &Arena, values: &[Value]) -> Json {
    Json::Array(
        values
            .iter()
            .map(|value| json_of_val(arena, *value))
            .collect(),
    )
}

/// The JSON of a type.
pub fn json_of_typ(typ: &Typ) -> Json {
    match &typ.node {
        TypKind::Bool => tagged("boolT", vec![]),
        TypKind::Num(num::Typ::Nat) => tagged("natT", vec![]),
        TypKind::Num(num::Typ::Int) => tagged("intT", vec![]),
        TypKind::Text => tagged("textT", vec![]),
        TypKind::Var(id, targs) => {
            tagged("varT", vec![Json::String(id.node.to_string()), json_of_typs(targs)])
        }
        TypKind::Tuple(typs) => tagged("tupT", vec![json_of_typs(typs)]),
        TypKind::Iter(typ, iter) => {
            let iter = match iter {
                Iter::Opt => "?",
                Iter::List => "*",
            };
            tagged("iterT", vec![json_of_typ(typ), Json::String(iter.into())])
        }
        TypKind::Func(_) => tagged("funcT", vec![]),
    }
}

fn json_of_typs(typs: &[Typ]) -> Json {
    Json::Array(typs.iter().map(json_of_typ).collect())
}

// == Decoding

fn unexpected(expected: &str, json: &Json) -> Box<crate::diagnostic::Report> {
    wire_invalid(format!("expected {expected}, but got {json}"))
}

fn allocated<T>(result: std::result::Result<T, ValueError>) -> Result<T> {
    result.map_err(|error| wire_invalid(format!("cannot allocate the value: {error}")))
}

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

fn atom_of_json(json: &Json) -> Result<AtomPhrase> {
    let Json::String(text) = json else {
        return Err(unexpected("an atom", json));
    };
    let atom = parse_atom(text)?
        .ok_or_else(|| wire_invalid(format!("expected one notation atom, but got {text:?}")))?;
    Ok(phrase!(node: atom, span: Span::default()))
}

fn mixop_of_json(arena: &mut Arena, json: &Json) -> Result<Mixop> {
    let Json::Array(rows) = json else {
        return Err(unexpected("an atoms matrix", json));
    };
    let matrix = rows
        .iter()
        .map(|row| {
            let Json::Array(atoms) = row else {
                return Err(unexpected("a row of atoms", row));
            };
            atoms.iter().map(atom_of_json).collect::<Result<Vec<_>>>()
        })
        .collect::<Result<Vec<_>>>()?;
    matrix[..]
        .to_mixop(&mut arena.mixop)
        .map_err(|error| wire_invalid(format!("cannot rebuild the mixop: {error}")))
}

fn natural_of_text(text: &str) -> Result<Natural> {
    let int = BigInt::from_str(text).map_err(|error| {
        wire_invalid(format!("expected a natural number, but got {text:?}: {error}"))
    })?;
    Natural::try_from(int).map_err(|error| {
        wire_invalid(format!("expected a natural number, but got {text:?}: {error}"))
    })
}

fn int_of_text(text: &str) -> Result<BigInt> {
    BigInt::from_str(text)
        .map_err(|error| wire_invalid(format!("expected an integer, but got {text:?}: {error}")))
}

/// Decodes a runtime value into the arena.
pub fn val_of_json(arena: &mut Arena, json: &Json) -> Result<Value> {
    let span = Span::default();
    let Json::Array(items) = json else {
        return Err(unexpected("a value", json));
    };
    let [Json::String(tag), rest @ ..] = &items[..] else {
        return Err(unexpected("a value", json));
    };
    let value = match (tag.as_str(), rest) {
        ("boolV", [Json::Bool(value)]) => allocated(make::bool(arena, *value, span))?,
        ("natN", [Json::String(text)]) => {
            allocated(make::nat(arena, natural_of_text(text)?, span))?
        }
        ("intN", [Json::String(text)]) => allocated(make::int(arena, int_of_text(text)?, span))?,
        ("textV", [Json::String(text)]) => allocated(make::text(arena, text.clone(), span))?,
        ("strV", [Json::Array(fields)]) => {
            let fields = fields
                .iter()
                .map(|field| field_of_json(arena, field))
                .collect::<Result<Vec<_>>>()?;
            allocated(make::structure(arena, placeholder(), fields, span))?
        }
        ("injV", [mixop, Json::Array(args)]) => {
            let mixop = mixop_of_json(arena, mixop)?;
            let args = vals_of_jsons(arena, args)?;
            let mixfix = Mixfix::new(&arena.mixop, mixop, args).map_err(|error| {
                wire_invalid(format!("case arguments do not fit the mixop: {error}"))
            })?;
            allocated(make::case_flat(arena, placeholder(), mixfix, span))?
        }
        ("tupV", [Json::Array(items)]) => {
            let values = vals_of_jsons(arena, items)?;
            let typ = typ::make::tuple(values.iter().map(|value| typ_of(arena, *value)).collect());
            allocated(make::tuple(arena, Rc::new(typ.node), values, span))?
        }
        ("optV", [Json::Null]) => {
            let typ = typ::make::opt(placeholder_typ());
            allocated(make::opt(arena, Rc::new(typ.node), None, span))?
        }
        ("optV", [inner]) => {
            let value = val_of_json(arena, inner)?;
            let typ = typ::make::opt(typ_of(arena, value));
            allocated(make::opt(arena, Rc::new(typ.node), Some(value), span))?
        }
        ("listV", [Json::Array(items)]) => {
            let values = vals_of_jsons(arena, items)?;
            let typ_elem = values
                .first()
                .map_or_else(placeholder_typ, |value| typ_of(arena, *value));
            let typ = typ::make::list(typ_elem);
            allocated(make::list(arena, Rc::new(typ.node), values, span))?
        }
        ("funcV", [Json::String(id)]) => {
            allocated(Value::new(arena, ValueKind::Func(id_of(id)), placeholder(), span))?
        }
        ("extV", [json]) => {
            allocated(make::external(arena, placeholder(), Rc::new(json.clone()), span))?
        }
        _ => return Err(unexpected("a value", json)),
    };
    Ok(value)
}

fn field_of_json(arena: &mut Arena, json: &Json) -> Result<ValueField> {
    let Json::Array(items) = json else {
        return Err(unexpected("a struct field", json));
    };
    let [atom, value] = &items[..] else {
        return Err(unexpected("a struct field", json));
    };
    Ok(ValueField { atom: atom_of_json(atom)?, value: val_of_json(arena, value)? })
}

fn vals_of_jsons(arena: &mut Arena, jsons: &[Json]) -> Result<Vec<Value>> {
    jsons.iter().map(|json| val_of_json(arena, json)).collect()
}

fn vals_of_json(arena: &mut Arena, json: &Json) -> Result<Vec<Value>> {
    let Json::Array(jsons) = json else {
        return Err(unexpected("a list of values", json));
    };
    vals_of_jsons(arena, jsons)
}

/// Decodes a type; function types do not cross the boundary.
pub fn typ_of_json(json: &Json) -> Result<Typ> {
    let Json::Array(items) = json else {
        return Err(unexpected("a type", json));
    };
    let [Json::String(tag), rest @ ..] = &items[..] else {
        return Err(unexpected("a type", json));
    };
    Ok(match (tag.as_str(), rest) {
        ("boolT", []) => typ::make::bool(),
        ("natT", []) => typ::make::nat(),
        ("intT", []) => typ::make::int(),
        ("textT", []) => typ::make::text(),
        ("varT", [Json::String(id), Json::Array(targs)]) => {
            typ::make::var(id_of(id), typs_of_jsons(targs)?)
        }
        ("tupT", [Json::Array(typs)]) => typ::make::tuple(typs_of_jsons(typs)?),
        ("iterT", [typ, Json::String(iter)]) => {
            let iter = match iter.as_str() {
                "?" => Iter::Opt,
                "*" => Iter::List,
                _ => return Err(unexpected("a type", json)),
            };
            typ::make::iter(typ_of_json(typ)?, iter)
        }
        ("funcT", []) => {
            return Err(wire_invalid("a function type cannot cross the host boundary"));
        }
        _ => return Err(unexpected("a type", json)),
    })
}

fn typs_of_jsons(jsons: &[Json]) -> Result<Vec<Typ>> {
    jsons.iter().map(typ_of_json).collect()
}

fn typs_of_json(json: &Json) -> Result<Vec<Typ>> {
    let Json::Array(jsons) = json else {
        return Err(unexpected("a list of types", json));
    };
    typs_of_jsons(jsons)
}
