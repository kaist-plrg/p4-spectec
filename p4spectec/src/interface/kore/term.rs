//! Terms of the K specification as KORE, and back, at the runtime boundary
//!
//! A hook delegated to the K runtime gets its arguments, terms of the
//! specification (`term` in `spec-k/1-syntax.watsup`), as KORE patterns,
//! and gives back a KORE pattern that becomes a term again.
//! Domain values are written with the sort hooked to their kind:
//! `INT 5` is `\dv{SortInt{}}("5")`. Collections are written with the
//! constructors of their sort, and read back into the specification's
//! collection values: Map entries and Set elements in the order of
//! `$union_set`, as the specification keeps them (`$sorted_map`).

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

use crate::interface::builtin::sets;

use crate::phrase;

use super::{
    ast::{Pattern, Sort},
    info::{Collection, Info, Role},
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
    mint: Mixop,
    map: Mixop,
    set: Mixop,
    list: Mixop,
    rangemap: Mixop,
    range: Mixop,
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
            mint: mixop(arena, "MINT nat nat")?,
            map: mixop(arena, "MAP sort m")?,
            set: mixop(arena, "SET sort s")?,
            list: mixop(arena, "LIST sort term*")?,
            rangemap: mixop(arena, "RANGEMAP sort rangeitem*")?,
            range: mixop(arena, "RANGE int int term")?,
            sort: mixop(arena, "SORT sortid sort*")?,
            sortvar: mixop(arena, "SORTVAR sortid")?,
            pair: mixop(arena, "k ':' v")?,
            braces: mixop(arena, "`{ k `}")?,
            ok: mixop(arena, "OK x")?,
            fail: mixop(arena, "FAIL")?,
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
                // substOne makes inj{S, S}(V), which the runtime cannot read
                // back: no such symbol is declared. It is written as V.
                if let ([sort_a, sort_b], [term]) = (sorts.as_slice(), terms)
                    && symbol == "inj"
                    && sort_a == sort_b
                {
                    return to(term);
                }
                let terms = terms.iter().map(to).collect::<Result<_>>()?;
                Ok(Pattern::App(symbol, sorts, terms))
            }
            [sort, t] if is(arena, mixop, shapes.dv) => {
                Ok(dv(sort_of_value(arena, shapes, sort)?, text(arena, t)?))
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
            [width, n] if is(arena, mixop, shapes.mint) => {
                let width = int(arena, width)?;
                let sort_width = info
                    .nat_sorts
                    .get(&width.to_string())
                    .ok_or_else(|| format!("no sort stands for the width {width}"))?;
                let mint = info.sort_of_hook("MINT.MInt")?;
                let sort = Sort::Sort(mint.to_owned(), vec![sort_named(sort_width)]);
                Ok(dv(sort, format!("{}p{width}", int(arena, n)?)))
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
            // Each range [a, b) |-> v as RangeMap:Range(a, b) r|-> v
            [sort, items] if is(arena, mixop, shapes.rangemap) => {
                let sort = sort_of_value(arena, shapes, sort)?;
                let element = element_symbol(info, &sort)?;
                let sort_int = hooked("INT.Int")?;
                let key = |i: BigInt| {
                    Pattern::App(
                        "inj".to_owned(),
                        vec![sort_int.clone(), sort_named("SortKItem")],
                        vec![dv(sort_int.clone(), i.to_string())],
                    )
                };
                let elems = list(arena, items)?
                    .iter()
                    .map(|item| match case(arena, item)? {
                        (mixop, [a, b, v]) if is(arena, mixop, shapes.range) => {
                            let range = app(
                                "LblRangeMap'Coln'Range",
                                vec![key(int(arena, a)?), key(int(arena, b)?)],
                            );
                            Ok(app(element, vec![range, to(v)?]))
                        }
                        _ => Err("not a range".to_owned()),
                    })
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

    /// The elements of a set in the order of `$union_set`.
    fn set_value(&mut self, typ_elem: Typ, elems: Vec<Value>) -> Result<Value> {
        let elems = self.list(typ_elem.clone(), elems)?;
        let typ_set = typ::make::var(id("set"), vec![typ_elem.clone()]);
        let set = self.case(self.shapes.braces, vec![elems], typ_set)?;
        sets::union_set(self.arena, &[typ_elem], &[set, set]).map_err(|error| {
            let report = error.into_report();
            report.to_string()
        })
    }

    /// A domain value, by the hook of its sort.
    fn domain_value(&mut self, sort: &Sort, text: &str) -> Result<Value> {
        let Sort::Sort(name, _) = sort else {
            return Err(format!("domain value of sort {sort}"));
        };
        let bad = || format!("bad domain value {text:?} of {sort}");
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
            Some("MINT.MInt") => {
                let (n, width) = text.split_once('p').ok_or_else(bad)?;
                let n = n.parse::<BigInt>().map_err(|_| bad())?;
                let width = width.parse::<BigInt>().map_err(|_| bad())?;
                let width = self.nat(width)?;
                let n = self.nat(n)?;
                self.term(self.shapes.mint, vec![width, n])
            }
            _ => {
                let sort = self.sort(sort)?;
                let t = self.text(text)?;
                self.term(self.shapes.dv, vec![sort, t])
            }
        }
    }

    /// The elements of nested concatenations of a collection sort.
    fn elements<'p>(&self, sort: &str, pattern: &'p Pattern, elems: &mut Vec<&'p [Pattern]>) {
        if let Pattern::App(symbol, _, args) = pattern
            && let Some((sort_c, role)) = self.info.constructors.get(symbol)
            && sort_c == sort
        {
            match role {
                Role::Unit => {}
                Role::Element => elems.push(args),
                Role::Concat => {
                    for arg in args {
                        self.elements(sort, arg, elems);
                    }
                }
            }
        }
    }

    fn range_bound(&self, pattern: &Pattern) -> Result<BigInt> {
        match pattern {
            Pattern::App(inj, _, args) if inj == "inj" => match args.as_slice() {
                [arg] => self.range_bound(arg),
                _ => Err("bad range bound".to_owned()),
            },
            Pattern::App(dv, _, args) if dv == "\\dv" => match args.as_slice() {
                [Pattern::Str(text)] => text
                    .parse::<BigInt>()
                    .map_err(|_| "bad range bound".to_owned()),
                _ => Err("bad range bound".to_owned()),
            },
            _ => Err("bad range bound".to_owned()),
        }
    }

    /// A collection of the given sort, from its constructors.
    fn collection(&mut self, sort: &str, pattern: &Pattern) -> Result<Value> {
        let constructors = &self.info.collections[sort];
        let kind = constructors.collection;
        let mut elems = Vec::new();
        self.elements(sort, pattern, &mut elems);
        let value_sort = self.sort(&sort_named(sort))?;
        let wrong = || format!("bad element of {sort}");
        match kind {
            Collection::Map => {
                let typ_pair =
                    typ::make::var(id("pair"), vec![typ_named("term"), typ_named("term")]);
                let pairs = elems
                    .into_iter()
                    .map(|args| match args {
                        [k, v] => {
                            let k = self.of_pattern(k)?;
                            let v = self.of_pattern(v)?;
                            self.case(self.shapes.pair, vec![k, v], typ_pair.clone())
                        }
                        _ => Err(wrong()),
                    })
                    .collect::<Result<_>>()?;
                let map = self.set_value(typ_pair, pairs)?;
                self.term(self.shapes.map, vec![value_sort, map])
            }
            Collection::Set => {
                let elems = elems
                    .into_iter()
                    .map(|args| match args {
                        [elem] => self.of_pattern(elem),
                        _ => Err(wrong()),
                    })
                    .collect::<Result<_>>()?;
                let set = self.set_value(typ_named("term"), elems)?;
                self.term(self.shapes.set, vec![value_sort, set])
            }
            Collection::List => {
                let elems = elems
                    .into_iter()
                    .map(|args| match args {
                        [elem] => self.of_pattern(elem),
                        _ => Err(wrong()),
                    })
                    .collect::<Result<_>>()?;
                let elems = self.list(typ_named("term"), elems)?;
                self.term(self.shapes.list, vec![value_sort, elems])
            }
            Collection::RangeMap => {
                let mut ranges = elems
                    .into_iter()
                    .map(|args| match args {
                        [Pattern::App(_, _, bounds), v] => match bounds.as_slice() {
                            [a, b] => Ok((self.range_bound(a)?, self.range_bound(b)?, v)),
                            _ => Err(wrong()),
                        },
                        _ => Err(wrong()),
                    })
                    .collect::<Result<Vec<_>>>()?;
                ranges.sort_by(|(a, _, _), (b, _, _)| a.cmp(b));
                let items = ranges
                    .into_iter()
                    .map(|(a, b, v)| {
                        let a = self.int(a)?;
                        let b = self.int(b)?;
                        let v = self.of_pattern(v)?;
                        self.case(self.shapes.range, vec![a, b, v], typ_named("rangeitem"))
                    })
                    .collect::<Result<_>>()?;
                let items = self.list(typ_named("rangeitem"), items)?;
                self.term(self.shapes.rangemap, vec![value_sort, items])
            }
        }
    }

    /// A KORE pattern, without variables, as a term of the specification.
    pub fn of_pattern(&mut self, pattern: &Pattern) -> Result<Value> {
        stacker::maybe_grow(64 * 1024, 16 * 1024 * 1024, || match pattern {
            Pattern::App(symbol, sorts, args) if symbol == "\\dv" => {
                match (sorts.as_slice(), args.as_slice()) {
                    ([sort], [Pattern::Str(text)]) => self.domain_value(sort, text),
                    _ => Err("bad domain value".to_owned()),
                }
            }
            Pattern::App(symbol, sorts, args) => {
                if let Some((sort, _)) = self.info.constructors.get(symbol) {
                    let sort = sort.clone();
                    return self.collection(&sort, pattern);
                }
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
            Pattern::Var(..) | Pattern::SetVar(..) | Pattern::Str(_) => {
                Err(format!("not a term: {pattern}"))
            }
        })
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
