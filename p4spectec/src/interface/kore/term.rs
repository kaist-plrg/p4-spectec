//! Terms of the K specification as KORE, and domain values back
//!
//! `to_pattern` writes a term of the specification (`term` in
//! `spec-k/1-syntax.watsup`) as a KORE pattern: the arguments of a hook the
//! K runtime computes, and the final configuration of a run.
//! Domain values are written with the sort hooked to their kind:
//! `INT 5` is `\dv{SortInt{}}("5")`; collections with the constructors of
//! their sort, from its `element`, `concat`, and `unit` attributes.
//! What the runtime answers is read by the specification (`load`).

use num_bigint::BigInt;

use crate::lang::{
    common::prim::num,
    data::{
        arena::Arena,
        notation::flat::{
            Mixop,
            parse::{ParseMixopError, ToMixop},
        },
        value::flat::{self as value, Value},
    },
};

use super::{
    ast::{Pattern, Sort},
    info::Info,
};

type Result<T> = std::result::Result<T, String>;

fn value_error(error: impl std::fmt::Display) -> String {
    error.to_string()
}

// == Shapes of the specification's values

/// The notation of the cases read and written here, interned once.
pub struct Shapes {
    app: Mixop,
    dv: Mixop,
    int: Mixop,
    bool: Mixop,
    string: Mixop,
    float: Mixop,
    bytes: Mixop,
    map: Mixop,
    set: Mixop,
    list: Mixop,
    sort: Mixop,
    sortvar: Mixop,
    pair: Mixop,
    braces: Mixop,
}

fn mixop(arena: &mut Arena, shape: &str) -> Result<Mixop> {
    shape
        .to_mixop(&mut arena.mixop)
        .map_err(|error| match error {
            ParseMixopError::Syntax(error) => panic!("a valid mixop: {error}"),
            ParseMixopError::Intern(error) => error.to_string(),
        })
}

impl Shapes {
    pub fn new(arena: &mut Arena) -> Result<Self> {
        Ok(Self {
            app: mixop(arena, "APP symid sort* term*")?,
            dv: mixop(arena, "DV sort text")?,
            int: mixop(arena, "INT int")?,
            bool: mixop(arena, "BOOL bool")?,
            string: mixop(arena, "STRING text")?,
            float: mixop(arena, "FLOAT text")?,
            bytes: mixop(arena, "BYTES nat*")?,
            map: mixop(arena, "MAP sort m")?,
            set: mixop(arena, "SET sort s")?,
            list: mixop(arena, "LIST sort term*")?,
            sort: mixop(arena, "SORT sortid sort*")?,
            sortvar: mixop(arena, "SORTVAR sortid")?,
            pair: mixop(arena, "k ':' v")?,
            braces: mixop(arena, "`{ k `}")?,
        })
    }
}

// == Reading values

/// The case of a value: its notation's identity and its arguments.
fn case<'a>(arena: &'a Arena, value: &Value) -> Result<(Mixop, &'a [Value])> {
    let case = value::get::case(arena, value).map_err(value_error)?;
    Ok((*case.mixop(), case.args()))
}

fn is(arena: &Arena, mixop: Mixop, shape: Mixop) -> bool {
    arena.mixop.canon_id(mixop) == arena.mixop.canon_id(shape)
}

fn text(arena: &Arena, value: &Value) -> Result<String> {
    Ok(value::get::text(arena, value)
        .map_err(value_error)?
        .to_owned())
}

fn int(arena: &Arena, value: &Value) -> Result<BigInt> {
    let number = value::get::num(arena, value).map_err(value_error)?;
    Ok(num::to_int(number).clone())
}

fn list<'a>(arena: &'a Arena, value: &Value) -> Result<&'a [Value]> {
    value::get::list(arena, value).map_err(value_error)
}

/// The elements of a `{ k* }` case: a set, or the pairs of a map.
fn braces<'a>(arena: &'a Arena, shapes: &Shapes, value: &Value) -> Result<&'a [Value]> {
    match case(arena, value)? {
        (mixop, [elems]) if is(arena, mixop, shapes.braces) => list(arena, elems),
        _ => Err("not a set or map".to_owned()),
    }
}

pub fn sort_of_value(arena: &Arena, shapes: &Shapes, value: &Value) -> Result<Sort> {
    match case(arena, value)? {
        (mixop, [name, sorts]) if is(arena, mixop, shapes.sort) => {
            let sorts = list(arena, sorts)?
                .iter()
                .map(|sort| sort_of_value(arena, shapes, sort))
                .collect::<Result<_>>()?;
            Ok(Sort::Sort(text(arena, name)?, sorts))
        }
        (mixop, [name]) if is(arena, mixop, shapes.sortvar) => Ok(Sort::Var(text(arena, name)?)),
        _ => Err("not a sort".to_owned()),
    }
}

fn sort_named(name: &str) -> Sort {
    Sort::Sort(name.to_owned(), vec![])
}

fn dv(sort: Sort, text: String) -> Pattern {
    Pattern::App("\\dv".to_owned(), vec![sort], vec![Pattern::Str(text)])
}

fn app(symbol: &str, args: Vec<Pattern>) -> Pattern {
    Pattern::App(symbol.to_owned(), vec![], args)
}

/// Elements joined by a collection's concatenation; its unit if none.
fn collection(info: &Info, sort: &Sort, elems: Vec<Pattern>) -> Result<Pattern> {
    let Sort::Sort(name, _) = sort else {
        return Err(format!("collection of sort {sort}"));
    };
    let constructors = info
        .collections
        .get(name)
        .ok_or_else(|| format!("{name} is not a collection sort"))?;
    let mut elems = elems.into_iter();
    let Some(first) = elems.next() else {
        return Ok(app(&constructors.unit, vec![]));
    };
    Ok(elems.fold(first, |acc, elem| app(&constructors.concat, vec![acc, elem])))
}

fn element_symbol<'a>(info: &'a Info, sort: &Sort) -> Result<&'a str> {
    let Sort::Sort(name, _) = sort else {
        return Err(format!("collection of sort {sort}"));
    };
    info.collections
        .get(name)
        .map(|constructors| constructors.element.as_str())
        .ok_or_else(|| format!("{name} is not a collection sort"))
}

/// A term of the specification as a KORE pattern.
pub fn to_pattern(arena: &Arena, info: &Info, shapes: &Shapes, value: &Value) -> Result<Pattern> {
    stacker::maybe_grow(64 * 1024, 16 * 1024 * 1024, || {
        // By the case of the term
        let (mixop, args) = case(arena, value)?;
        let to = |value: &Value| to_pattern(arena, info, shapes, value);
        let hooked = |hook: &str| info.sort_of_hook(hook).map(sort_named);
        match args {
            [symbol, sorts, terms] if is(arena, mixop, shapes.app) => {
                let symbol = text(arena, symbol)?;
                let sorts: Vec<Sort> = list(arena, sorts)?
                    .iter()
                    .map(|sort| sort_of_value(arena, shapes, sort))
                    .collect::<Result<_>>()?;
                let terms = list(arena, terms)?;
                let terms = terms.iter().map(to).collect::<Result<_>>()?;
                Ok(Pattern::App(symbol, sorts, terms))
            }
            [sort, t] if is(arena, mixop, shapes.dv) => {
                let sort = sort_of_value(arena, shapes, sort)?;
                let t = text(arena, t)?;
                Ok(dv(sort, t))
            }
            [i] if is(arena, mixop, shapes.int) => {
                Ok(dv(hooked("INT.Int")?, int(arena, i)?.to_string()))
            }
            [b] if is(arena, mixop, shapes.bool) => {
                let b = value::get::bool(arena, b).map_err(value_error)?;
                Ok(dv(hooked("BOOL.Bool")?, b.to_string()))
            }
            [t] if is(arena, mixop, shapes.string) => {
                Ok(dv(hooked("STRING.String")?, text(arena, t)?))
            }
            [t] if is(arena, mixop, shapes.float) => {
                Ok(dv(hooked("FLOAT.Float")?, text(arena, t)?))
            }
            [bytes] if is(arena, mixop, shapes.bytes) => {
                let text = list(arena, bytes)?
                    .iter()
                    .map(|byte| {
                        let byte = u8::try_from(int(arena, byte)?).map_err(value_error)?;
                        Ok(char::from(byte))
                    })
                    .collect::<Result<String>>()?;
                Ok(dv(hooked("BYTES.Bytes")?, text))
            }
            [sort, map] if is(arena, mixop, shapes.map) => {
                let sort = sort_of_value(arena, shapes, sort)?;
                let element = element_symbol(info, &sort)?;
                let elems = braces(arena, shapes, map)?
                    .iter()
                    .map(|pair| match case(arena, pair)? {
                        (mixop, [k, v]) if is(arena, mixop, shapes.pair) => {
                            Ok(app(element, vec![to(k)?, to(v)?]))
                        }
                        _ => Err("not a map entry".to_owned()),
                    })
                    .collect::<Result<_>>()?;
                collection(info, &sort, elems)
            }
            [sort, set] if is(arena, mixop, shapes.set) => {
                let sort = sort_of_value(arena, shapes, sort)?;
                let element = element_symbol(info, &sort)?;
                let elems = braces(arena, shapes, set)?
                    .iter()
                    .map(|elem| Ok(app(element, vec![to(elem)?])))
                    .collect::<Result<_>>()?;
                collection(info, &sort, elems)
            }
            [sort, terms] if is(arena, mixop, shapes.list) => {
                let sort = sort_of_value(arena, shapes, sort)?;
                let element = element_symbol(info, &sort)?;
                let elems = list(arena, terms)?
                    .iter()
                    .map(|elem| Ok(app(element, vec![to(elem)?])))
                    .collect::<Result<_>>()?;
                collection(info, &sort, elems)
            }
            _ => Err("not a term".to_owned()),
        }
    })
}
