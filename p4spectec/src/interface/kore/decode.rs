//! Values of the K specification as KORE
//!
//! As the AL decoder of P4-SpecTec² (`interface/al/decode.rs`), `term` takes
//! a term of the specification (`term` in `spec-k/1-syntax.watsup`) to the
//! host's data, a KORE pattern: the arguments of a hook the K runtime
//! computes (`offload`), and the configuration of a run.
//! Domain values are written with the sort hooked to their kind:
//! `INT 5` is `\dv{SortInt{}}("5")`; collections with the constructors of
//! their sort, from its `element`, `concat`, and `unit` attributes.

use num_bigint::BigInt;

use crate::lang::{
    common::prim::num,
    data::{
        arena::Arena,
        notation::flat::Mixop,
        value::flat::{self as value, Value},
    },
};

use super::{
    ast::{Pattern, Sort},
    info::Info,
    mixops::Mixops,
};

type Result<T> = std::result::Result<T, String>;

fn value_error(error: impl std::fmt::Display) -> String {
    error.to_string()
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
fn braces<'a>(arena: &'a Arena, mixops: &Mixops, value: &Value) -> Result<&'a [Value]> {
    match case(arena, value)? {
        (mixop, [elems]) if is(arena, mixop, mixops.braces) => list(arena, elems),
        _ => Err("not a set or map".to_owned()),
    }
}

// == Decoding

/// A sort of the specification as a KORE sort.
pub fn sort(arena: &Arena, mixops: &Mixops, value: &Value) -> Result<Sort> {
    match case(arena, value)? {
        (mixop, [name, sorts]) if is(arena, mixop, mixops.sort) => {
            let sorts = list(arena, sorts)?
                .iter()
                .map(|sort| self::sort(arena, mixops, sort))
                .collect::<Result<_>>()?;
            Ok(Sort::Sort(text(arena, name)?, sorts))
        }
        (mixop, [name]) if is(arena, mixop, mixops.sortvar) => Ok(Sort::Var(text(arena, name)?)),
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
pub fn term(arena: &Arena, info: &Info, mixops: &Mixops, value: &Value) -> Result<Pattern> {
    stacker::maybe_grow(64 * 1024, 16 * 1024 * 1024, || {
        // By the case of the term
        let (mixop, args) = case(arena, value)?;
        let to = |value: &Value| self::term(arena, info, mixops, value);
        let hooked = |hook: &str| info.sort_of_hook(hook).map(sort_named);
        match args {
            [symbol, sorts, terms] if is(arena, mixop, mixops.app) => {
                let symbol = text(arena, symbol)?;
                let sorts: Vec<Sort> = list(arena, sorts)?
                    .iter()
                    .map(|sort| self::sort(arena, mixops, sort))
                    .collect::<Result<_>>()?;
                let terms = list(arena, terms)?;
                let terms = terms.iter().map(to).collect::<Result<_>>()?;
                Ok(Pattern::App(symbol, sorts, terms))
            }
            [sort, t] if is(arena, mixop, mixops.dv) => {
                let sort = self::sort(arena, mixops, sort)?;
                let t = text(arena, t)?;
                Ok(dv(sort, t))
            }
            [i] if is(arena, mixop, mixops.int) => {
                Ok(dv(hooked("INT.Int")?, int(arena, i)?.to_string()))
            }
            [b] if is(arena, mixop, mixops.bool) => {
                let b = value::get::bool(arena, b).map_err(value_error)?;
                Ok(dv(hooked("BOOL.Bool")?, b.to_string()))
            }
            [t] if is(arena, mixop, mixops.string) => {
                Ok(dv(hooked("STRING.String")?, text(arena, t)?))
            }
            [t] if is(arena, mixop, mixops.float) => {
                Ok(dv(hooked("FLOAT.Float")?, text(arena, t)?))
            }
            [bytes] if is(arena, mixop, mixops.bytes) => {
                let text = list(arena, bytes)?
                    .iter()
                    .map(|byte| {
                        let byte = u8::try_from(int(arena, byte)?).map_err(value_error)?;
                        Ok(char::from(byte))
                    })
                    .collect::<Result<String>>()?;
                Ok(dv(hooked("BYTES.Bytes")?, text))
            }
            [sort, map] if is(arena, mixop, mixops.map) => {
                let sort = self::sort(arena, mixops, sort)?;
                let element = element_symbol(info, &sort)?;
                let elems = braces(arena, mixops, map)?
                    .iter()
                    .map(|pair| match case(arena, pair)? {
                        (mixop, [k, v]) if is(arena, mixop, mixops.pair) => {
                            Ok(app(element, vec![to(k)?, to(v)?]))
                        }
                        _ => Err("not a map entry".to_owned()),
                    })
                    .collect::<Result<_>>()?;
                collection(info, &sort, elems)
            }
            [sort, set] if is(arena, mixop, mixops.set) => {
                let sort = self::sort(arena, mixops, sort)?;
                let element = element_symbol(info, &sort)?;
                let elems = braces(arena, mixops, set)?
                    .iter()
                    .map(|elem| Ok(app(element, vec![to(elem)?])))
                    .collect::<Result<_>>()?;
                collection(info, &sort, elems)
            }
            [sort, terms] if is(arena, mixop, mixops.list) => {
                let sort = self::sort(arena, mixops, sort)?;
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
