//! Terms of the K specification as KORE, and domain values back
//!
//! `to_pattern` writes a term of the specification (`term` in
//! `spec-k/1-syntax.watsup`) as a KORE pattern: the arguments of a hook the
//! K runtime computes, and the final configuration of a run.
//! Domain values are written with the sort hooked to their kind:
//! `INT 5` is `\dv{SortInt{}}("5")`; collections with the constructors of
//! their sort, from its `element`, `concat`, and `unit` attributes.
//! `Maker::of_pattern` reads the result of such a hook back into a term:
//! a domain value, or a constructor applied to such results.

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
        typ::{self, Typ, TypKind},
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
    fail: Mixop,
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
            fail: mixop(arena, "ERROR")?,
        })
    }
}

// == Types

fn id(name: &str) -> Id {
    phrase!(node: name.into(), span: Span::default())
}

fn typ_named(name: &str) -> Typ {
    typ::make::var(id(name), vec![])
}

fn rc(typ: Typ) -> Rc<TypKind> {
    Rc::new(typ.node)
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

// == Making values

/// A builder of terms in an arena.
pub struct Maker<'a> {
    pub arena: &'a mut Arena,
    pub info: &'a Info,
    pub shapes: &'a Shapes,
}

impl Maker<'_> {
    fn case(&mut self, mixop: Mixop, args: Vec<Value>, typ: Typ) -> Result<Value> {
        let mixfix = Mixfix::new(&self.arena.mixop, mixop, args).expect("mixop arity matches");
        value::make::case_flat(self.arena, rc(typ), mixfix, Span::default()).map_err(value_error)
    }

    fn term(&mut self, mixop: Mixop, args: Vec<Value>) -> Result<Value> {
        self.case(mixop, args, typ_named("term"))
    }

    fn list(&mut self, typ_elem: Typ, values: Vec<Value>) -> Result<Value> {
        let typ = rc(typ::make::list(typ_elem));
        value::make::list(self.arena, typ, values, Span::default()).map_err(value_error)
    }

    fn text(&mut self, text: &str) -> Result<Value> {
        value::make::text(self.arena, text.to_owned(), Span::default()).map_err(value_error)
    }

    fn int(&mut self, i: BigInt) -> Result<Value> {
        value::make::int(self.arena, i, Span::default()).map_err(value_error)
    }

    fn nat(&mut self, n: BigInt) -> Result<Value> {
        let n = Natural::try_from(n).map_err(value_error)?;
        value::make::nat(self.arena, n, Span::default()).map_err(value_error)
    }

    pub fn sort(&mut self, sort: &Sort) -> Result<Value> {
        match sort {
            Sort::Sort(name, sorts) => {
                let name = self.text(name)?;
                let sorts = sorts
                    .iter()
                    .map(|sort| self.sort(sort))
                    .collect::<Result<_>>()?;
                let sorts = self.list(typ_named("sort"), sorts)?;
                self.case(self.shapes.sort, vec![name, sorts], typ_named("sort"))
            }
            Sort::Var(name) => {
                let name = self.text(name)?;
                self.case(self.shapes.sortvar, vec![name], typ_named("sort"))
            }
        }
    }

    /// A domain value, by the hook of its sort.
    fn domain_value(&mut self, sort: &Sort, text: &str) -> Result<Value> {
        let Sort::Sort(name, _) = sort else {
            return Err(format!("domain value of sort {sort}"));
        };
        let bad = || format!("bad domain value {text:?} of {sort}");
        // The hook of the sort says how its text reads
        match self.info.sort_hook(name) {
            Some("INT.Int") => {
                let i = text.parse::<BigInt>().map_err(|_| bad())?;
                let i = self.int(i)?;
                self.term(self.shapes.int, vec![i])
            }
            Some("BOOL.Bool") => {
                let b = match text {
                    "true" => true,
                    "false" => false,
                    _ => return Err(bad()),
                };
                let b = value::make::bool(self.arena, b, Span::default()).map_err(value_error)?;
                self.term(self.shapes.bool, vec![b])
            }
            Some("STRING.String") => {
                let t = self.text(text)?;
                self.term(self.shapes.string, vec![t])
            }
            Some("FLOAT.Float") => {
                let t = self.text(text)?;
                self.term(self.shapes.float, vec![t])
            }
            Some("BYTES.Bytes") => {
                let bytes = text
                    .chars()
                    .map(|c| self.nat(BigInt::from(u32::from(c))))
                    .collect::<Result<_>>()?;
                let bytes = self.list(typ::make::nat(), bytes)?;
                self.term(self.shapes.bytes, vec![bytes])
            }
            _ => {
                let sort = self.sort(sort)?;
                let t = self.text(text)?;
                self.term(self.shapes.dv, vec![sort, t])
            }
        }
    }

    /// The result of a hook computed by the runtime as a term: a domain value,
    /// or a constructor applied to such results, as `G1Point(x, y)` of KRYPTO.
    pub fn of_pattern(&mut self, pattern: &Pattern) -> Result<Value> {
        match pattern {
            Pattern::App(symbol, sorts, args) if symbol == "\\dv" => {
                match (sorts.as_slice(), args.as_slice()) {
                    ([sort], [Pattern::Str(text)]) => self.domain_value(sort, text),
                    _ => Err(format!("bad domain value {pattern}")),
                }
            }
            Pattern::App(symbol, sorts, args) => {
                let symbol = self.text(symbol)?;
                let sorts = sorts
                    .iter()
                    .map(|sort| self.sort(sort))
                    .collect::<Result<_>>()?;
                let sorts = self.list(typ_named("sort"), sorts)?;
                let args = args
                    .iter()
                    .map(|arg| self.of_pattern(arg))
                    .collect::<Result<_>>()?;
                let args = self.list(typ_named("term"), args)?;
                self.term(self.shapes.app, vec![symbol, sorts, args])
            }
            _ => Err(format!("the runtime gives {pattern}, not a term")),
        }
    }

    /// `OK term`, of `res<term>`.
    pub fn ok(&mut self, term: Value) -> Result<Value> {
        let typ = typ::make::var(id("res"), vec![typ_named("term")]);
        self.case(self.shapes.ok, vec![term], typ)
    }

    /// `FAIL`, of `res<term>`.
    pub fn fail(&mut self) -> Result<Value> {
        let typ = typ::make::var(id("res"), vec![typ_named("term")]);
        self.case(self.shapes.fail, vec![], typ)
    }
}
