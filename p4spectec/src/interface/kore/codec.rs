//! Terms of the K specification and KORE, at the boundary of the K runtime
//!
//! As the AL codec of P4-SpecTec² (`metacirc/codec.rs`), `decode` takes
//! values of the specification to the host's data, here KORE patterns, and
//! `encode` makes values of the specification from them.
//! `decode::term` writes a term (`term` in `spec-k/1-syntax.watsup`) as a
//! KORE pattern: the arguments of a hook the K runtime computes
//! (`decode::hook_args`), and the final configuration of a run.
//! `encode::hook_result` makes the runtime's answer a term again: a domain
//! value, or a constructor applied to such answers. `encode::domain_value`
//! reads a domain value by the hook of its sort, for the answers and for
//! the definition and the initial term (`load`).
//! Domain values are written with the sort hooked to their kind:
//! `INT 5` is `\dv{SortInt{}}("5")`; collections with the constructors of
//! their sort, from its `element`, `concat`, and `unit` attributes.

use std::rc::Rc;

use num_bigint::BigInt;

use crate::lang::{
    common::{
        Id,
        prim::num::{self, Natural},
        source::Span,
    },
    data::{
        arena::Arena,
        notation::flat::{
            Mixfix, Mixop,
            parse::{ParseMixopError, ToMixop},
        },
        typ::{self, Typ},
        value::flat::{self as value, Value},
    },
};

use crate::phrase;

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
    ok: Mixop,
    error: Mixop,
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
            ok: mixop(arena, "OK x")?,
            error: mixop(arena, "ERROR")?,
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

// == Decoding: values of the specification as KORE

pub mod decode {
    use super::*;

    /// A sort of the specification as a KORE sort.
    pub fn sort(arena: &Arena, shapes: &Shapes, value: &Value) -> Result<Sort> {
        match case(arena, value)? {
            (mixop, [name, sorts]) if is(arena, mixop, shapes.sort) => {
                let sorts = list(arena, sorts)?
                    .iter()
                    .map(|sort| self::sort(arena, shapes, sort))
                    .collect::<Result<_>>()?;
                Ok(Sort::Sort(text(arena, name)?, sorts))
            }
            (mixop, [name]) if is(arena, mixop, shapes.sortvar) => {
                Ok(Sort::Var(text(arena, name)?))
            }
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
    pub fn term(arena: &Arena, info: &Info, shapes: &Shapes, value: &Value) -> Result<Pattern> {
        stacker::maybe_grow(64 * 1024, 16 * 1024 * 1024, || {
            // By the case of the term
            let (mixop, args) = case(arena, value)?;
            let to = |value: &Value| self::term(arena, info, shapes, value);
            let hooked = |hook: &str| info.sort_of_hook(hook).map(sort_named);
            match args {
                [symbol, sorts, terms] if is(arena, mixop, shapes.app) => {
                    let symbol = text(arena, symbol)?;
                    let sorts: Vec<Sort> = list(arena, sorts)?
                        .iter()
                        .map(|sort| self::sort(arena, shapes, sort))
                        .collect::<Result<_>>()?;
                    let terms = list(arena, terms)?;
                    let terms = terms.iter().map(to).collect::<Result<_>>()?;
                    Ok(Pattern::App(symbol, sorts, terms))
                }
                [sort, t] if is(arena, mixop, shapes.dv) => {
                    let sort = self::sort(arena, shapes, sort)?;
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
                    let sort = self::sort(arena, shapes, sort)?;
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
                    let sort = self::sort(arena, shapes, sort)?;
                    let element = element_symbol(info, &sort)?;
                    let elems = braces(arena, shapes, set)?
                        .iter()
                        .map(|elem| Ok(app(element, vec![to(elem)?])))
                        .collect::<Result<_>>()?;
                    collection(info, &sort, elems)
                }
                [sort, terms] if is(arena, mixop, shapes.list) => {
                    let sort = self::sort(arena, shapes, sort)?;
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

    /// The arguments of `Call_hook` (`spec-k/2-relation.watsup`), the hooked
    /// symbol, its sort parameters, its result sort, and the terms it applies
    /// to, as the application to evaluate and its sort.
    pub fn hook_args(
        arena: &Arena,
        info: &Info,
        shapes: &Shapes,
        values: &[Value],
    ) -> Result<(Pattern, Sort)> {
        let [symbol, sorts, sort, terms] = values else {
            return Err(format!("Call_hook takes 4 inputs, not {}", values.len()));
        };
        let symbol = text(arena, symbol)?;
        let sorts = list(arena, sorts)?
            .iter()
            .map(|sort| self::sort(arena, shapes, sort))
            .collect::<Result<_>>()?;
        let sort = self::sort(arena, shapes, sort)?;
        let terms = list(arena, terms)?
            .iter()
            .map(|t| self::term(arena, info, shapes, t))
            .collect::<Result<_>>()?;
        Ok((Pattern::App(symbol, sorts, terms), sort))
    }
}

// == Encoding: KORE as values of the specification

pub mod encode {
    use super::*;

    fn id(name: &str) -> Id {
        phrase!(node: name.into(), span: Span::default())
    }

    fn typ_named(name: &str) -> Typ {
        typ::make::var(id(name), vec![])
    }

    fn case(arena: &mut Arena, mixop: Mixop, args: Vec<Value>, typ: Typ) -> Result<Value> {
        let mixfix = Mixfix::new(&arena.mixop, mixop, args).expect("mixop arity matches");
        let typ = Rc::new(typ.node);
        value::make::case_flat(arena, typ, mixfix, Span::default()).map_err(value_error)
    }

    fn list(arena: &mut Arena, typ_elem: Typ, values: Vec<Value>) -> Result<Value> {
        let typ = Rc::new(typ::make::list(typ_elem).node);
        value::make::list(arena, typ, values, Span::default()).map_err(value_error)
    }

    fn text(arena: &mut Arena, text: &str) -> Result<Value> {
        value::make::text(arena, text.to_owned(), Span::default()).map_err(value_error)
    }

    fn sort(arena: &mut Arena, shapes: &Shapes, sort: &Sort) -> Result<Value> {
        match sort {
            Sort::Sort(name, sorts) => {
                let name = text(arena, name)?;
                let sorts = sorts
                    .iter()
                    .map(|sort| self::sort(arena, shapes, sort))
                    .collect::<Result<_>>()?;
                let sorts = list(arena, typ_named("sort"), sorts)?;
                case(arena, shapes.sort, vec![name, sorts], typ_named("sort"))
            }
            Sort::Var(name) => {
                let name = text(arena, name)?;
                case(arena, shapes.sortvar, vec![name], typ_named("sort"))
            }
        }
    }

    /// A domain value `\dv{S}("text")` by the hook of `S`, as a case of
    /// `typ`, `term` or `pattern`, which share these cases. `float` writes a
    /// Float in the runtime's output format; a KVar, and a value of a sort
    /// without a hook, stay text.
    pub fn domain_value(
        arena: &mut Arena,
        info: &Info,
        shapes: &Shapes,
        typ: &str,
        sort: &Sort,
        t: &str,
        float: impl FnOnce(&str) -> Result<String>,
    ) -> Result<Value> {
        let bad = || format!("bad domain value {t:?} of {sort}");
        let hook = match sort {
            Sort::Sort(name, _) => info.sort_hook(name),
            Sort::Var(_) => return Err(bad()),
        };
        // The hook of the sort says how its text reads
        let (mixop, arg) = match hook {
            Some("INT.Int") => {
                let i = super::super::builtins::parse_base(t, 10).ok_or_else(bad)?;
                let i = value::make::int(arena, i, Span::default()).map_err(value_error)?;
                (shapes.int, i)
            }
            Some("BOOL.Bool") => {
                let b = match t {
                    "true" => true,
                    "false" => false,
                    _ => return Err(bad()),
                };
                let b = value::make::bool(arena, b, Span::default()).map_err(value_error)?;
                (shapes.bool, b)
            }
            Some("STRING.String") => (shapes.string, text(arena, t)?),
            Some("FLOAT.Float") => (shapes.float, text(arena, &float(t)?)?),
            Some("BYTES.Bytes") => {
                let bytes = t
                    .chars()
                    .map(|c| {
                        let n =
                            Natural::try_from(BigInt::from(u32::from(c))).map_err(value_error)?;
                        value::make::nat(arena, n, Span::default()).map_err(value_error)
                    })
                    .collect::<Result<_>>()?;
                (shapes.bytes, list(arena, typ::make::nat(), bytes)?)
            }
            Some("KVAR.KVar") | None => {
                let sort = self::sort(arena, shapes, sort)?;
                let t = text(arena, t)?;
                return case(arena, shapes.dv, vec![sort, t], typ_named(typ));
            }
            Some(hook) => return Err(format!("domain value of {sort}, hooked {hook}")),
        };
        case(arena, mixop, vec![arg], typ_named(typ))
    }

    /// A domain value, or a constructor applied to such terms, as
    /// `G1Point(x, y)` of KRYPTO. The runtime writes a Float in its output
    /// format already.
    fn term(arena: &mut Arena, info: &Info, shapes: &Shapes, pattern: &Pattern) -> Result<Value> {
        match pattern {
            Pattern::App(symbol, sorts, args) if symbol == "\\dv" => {
                match (sorts.as_slice(), args.as_slice()) {
                    ([sort], [Pattern::Str(t)]) => {
                        domain_value(arena, info, shapes, "term", sort, t, |t| Ok(t.to_owned()))
                    }
                    _ => Err(format!("bad domain value {pattern}")),
                }
            }
            Pattern::App(symbol, sorts, args) => {
                let symbol = text(arena, symbol)?;
                let sorts = sorts
                    .iter()
                    .map(|sort| self::sort(arena, shapes, sort))
                    .collect::<Result<_>>()?;
                let sorts = list(arena, typ_named("sort"), sorts)?;
                let args = args
                    .iter()
                    .map(|arg| term(arena, info, shapes, arg))
                    .collect::<Result<_>>()?;
                let args = list(arena, typ_named("term"), args)?;
                case(arena, shapes.app, vec![symbol, sorts, args], typ_named("term"))
            }
            _ => Err(format!("the runtime gives {pattern}, not a term")),
        }
    }

    /// The result of `Call_hook`, of `res<term>`: `OK` the runtime's answer as a
    /// term, or `ERROR` if it failed.
    pub fn hook_result(
        arena: &mut Arena,
        info: &Info,
        shapes: &Shapes,
        answer: Option<&Pattern>,
    ) -> Result<Value> {
        let typ = typ::make::var(id("res"), vec![typ_named("term")]);
        match answer {
            Some(answer) => {
                let term = term(arena, info, shapes, answer)?;
                case(arena, shapes.ok, vec![term], typ)
            }
            None => case(arena, shapes.error, vec![], typ),
        }
    }
}
