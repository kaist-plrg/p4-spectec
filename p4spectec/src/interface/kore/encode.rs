//! KORE as values of the K specification
//!
//! As the AL encoder of P4-SpecTec² (`interface/al/encode.rs`) encodes a
//! specification, `definition` and `kore` encode the parsed KORE as it is (`kore` and `kdefinition`
//! in `spec-k/1-syntax.watsup`): every axiom with its attributes,
//! and the declarations of sorts and symbols. Alias declarations are not
//! read, and an axiom that applies one is an error: kompile 7.1.337 applies
//! none by default. Connectives are named without their backslash.
//! Domain values are read by the hook of their sort, as a parser reads
//! literals (`domain_value`): `\dv{SortInt{}}("1")` becomes `KDV (INT 1)`.
//! `term` encodes the K runtime's answer, a domain value or a constructor
//! applied to such answers, as a term (`offload`).
//! Which axioms are executed and how they are read is for the
//! specification to decide (`spec-k/3-load.watsup`).

use std::{collections::HashSet, rc::Rc};

use num_bigint::BigInt;

use crate::lang::{
    common::{Id, notation::atom::Atom, prim::num::Natural, source::Span},
    data::{
        arena::Arena,
        notation::flat::{
            Mixfix, Mixop,
            parse::{ParseMixopError, ToMixop},
        },
        typ::{self, Typ, TypKind},
        value::{
            ValueError,
            flat::{self as value, Value, ValueField},
        },
    },
};

use crate::phrase;

use super::{
    Loaded,
    ast::{Attr, Definition, Pattern, Sentence, Sort},
    info::Info,
    mixops::Mixops,
    offload,
};

// == Errors

/// A failure while turning KORE into values.
#[derive(Debug)]
pub enum EncodeError {
    /// A symbol that is not declared.
    SymbolUndeclared(String),
    /// An alias applied in an axiom, as an older kompile wrote rules.
    AliasApplied(String),
    /// A declaration or attribute of an unexpected form.
    Malformed(String),
    /// A domain value that does not read by the hook of its sort.
    Literal(String),
    Value(ValueError),
}

impl From<ValueError> for EncodeError {
    fn from(error: ValueError) -> Self {
        Self::Value(error)
    }
}

impl std::fmt::Display for EncodeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::SymbolUndeclared(name) => write!(f, "undeclared symbol {name}"),
            Self::AliasApplied(name) => write!(
                f,
                "alias {name} is applied: aliases are not read (kompile writes rules so only with --enable-kore-antileft)"
            ),
            Self::Malformed(what) => write!(f, "malformed {what}"),
            Self::Literal(message) => write!(f, "{message}"),
            Self::Value(error) => error.fmt(f),
        }
    }
}

/// A loading result.
type Result<T> = std::result::Result<T, EncodeError>;

/// A result of encoding domain values and the runtime's answers.
type Answer<T> = std::result::Result<T, String>;

fn value_error(error: impl std::fmt::Display) -> String {
    error.to_string()
}

// == Types

/// An identifier without a location.
fn id(name: &str) -> Id {
    phrase!(node: name.into(), span: Span::default())
}

/// A named type without arguments, e.g. `kore`.
fn typ(name: &str) -> Typ {
    typ::make::var(id(name), vec![])
}

/// `map<K, V>`, a case over `pair<K, V>*` (`spec-meta/0-aux/0.0-stdlib.watsup`).
fn typ_map(typ_key: Typ, typ_value: Typ) -> Typ {
    typ::make::var(id("map"), vec![typ_key, typ_value])
}

/// `pair<K, V>`, an entry of a map.
fn typ_pair(typ_key: Typ, typ_value: Typ) -> Typ {
    typ::make::var(id("pair"), vec![typ_key, typ_value])
}

/// A type as values hold it.
fn rc(typ: Typ) -> Rc<TypKind> {
    Rc::new(typ.node)
}

// == Value construction

/// A builder of the values of one definition, in one arena.
pub struct Builder<'a> {
    arena: &'a mut Arena,
    /// The definition: applications must be of its declared symbols, and
    /// domain values read by the hooks of their sorts.
    loaded: &'a Loaded,
    /// Declared aliases, which may not be applied.
    aliases: &'a HashSet<String>,
}

/// A case of the given notation and type, from its arguments.
macro_rules! case {
    ($arena:expr, $shape:expr, [$($arg:expr),* $(,)?], $typ:expr) => {
        value::make::case_string! {
            arena: $arena,
            shape: $shape,
            args: vec![$($arg),*],
            typ: $typ,
            span: Span::default(),
        }
    };
}

impl<'a> Builder<'a> {
    /// A builder into an arena, for the definition `loaded`.
    pub fn new(arena: &'a mut Arena, loaded: &'a Loaded, aliases: &'a HashSet<String>) -> Self {
        Self { arena, loaded, aliases }
    }

    /// A text value.
    fn text(&mut self, text: &str) -> Result<Value> {
        Ok(value::make::text(self.arena, text.to_owned(), Span::default())?)
    }

    /// A list value of the given element type.
    fn list(&mut self, typ_elem: Typ, values: Vec<Value>) -> Result<Value> {
        let typ = rc(typ::make::list(typ_elem));
        Ok(value::make::list(self.arena, typ, values, Span::default())?)
    }

    /// A case of the given shape and type.
    fn case_typed(&mut self, shape: &str, args: Vec<Value>, typ: Typ) -> Result<Value> {
        let mixop = shape
            .to_mixop(&mut self.arena.mixop)
            .map_err(|error| match error {
                ParseMixopError::Syntax(error) => panic!("a valid mixop: {error}"),
                ParseMixopError::Intern(error) => ValueError::from(error),
            })?;
        let mixfix = Mixfix::new(&self.arena.mixop, mixop, args).expect("mixop arity matches");
        Ok(value::make::case_flat(self.arena, rc(typ), mixfix, Span::default())?)
    }

    /// A map from its entries, in order.
    fn map(&mut self, typ_key: Typ, typ_value: Typ, entries: Vec<(Value, Value)>) -> Result<Value> {
        let typ_pair = typ_pair(typ_key.clone(), typ_value.clone());
        let pairs = entries
            .into_iter()
            .map(|(key, value)| self.case_typed("k ':' v", vec![key, value], typ_pair.clone()))
            .collect::<Result<Vec<_>>>()?;
        let list = self.list(typ_pair, pairs)?;
        self.case_typed("`{ k `}", vec![list], typ_map(typ_key, typ_value))
    }

    // - Sorts and patterns

    /// A sort as a `sort` value.
    pub fn sort(&mut self, sort: &Sort) -> Result<Value> {
        match sort {
            Sort::Sort(name, sorts) => {
                let value_name = self.text(name)?;
                let value_sorts = self.sorts(sorts)?;
                Ok(case!(self.arena, "SORT sortid sort*", [value_name, value_sorts], "sort")?)
            }
            Sort::Var(name) => {
                let value_name = self.text(name)?;
                Ok(case!(self.arena, "SORTVAR sortid", [value_name], "sort")?)
            }
        }
    }

    /// Sorts as a `sort*` value.
    fn sorts(&mut self, sorts: &[Sort]) -> Result<Value> {
        let values = sorts
            .iter()
            .map(|sort| self.sort(sort))
            .collect::<Result<Vec<_>>>()?;
        self.list(typ("sort"), values)
    }

    /// A pattern; with `check`, an application must be of a known symbol.
    fn kore_checked(&mut self, pattern: &Pattern, check: bool) -> Result<Value> {
        // Terms nest deeply, e.g. long K sequences and maps
        stacker::maybe_grow(64 * 1024, 16 * 1024 * 1024, || match pattern {
            Pattern::Var(name, sort) => {
                let value_name = self.text(name)?;
                let value_sort = self.sort(sort)?;
                Ok(case!(self.arena, "KVAR varid sort", [value_name, value_sort], "kore")?)
            }
            Pattern::SetVar(name, sort) => {
                let value_name = self.text(name)?;
                let value_sort = self.sort(sort)?;
                Ok(case!(self.arena, "KSETVAR varid sort", [value_name, value_sort], "kore")?)
            }
            Pattern::Str(text) => {
                let value_text = self.text(text)?;
                Ok(case!(self.arena, "KSTR text", [value_text], "kore")?)
            }
            Pattern::App(name, sorts, args) if name == "\\dv" => {
                let ([sort], [Pattern::Str(text)]) = (sorts.as_slice(), args.as_slice()) else {
                    return Err(EncodeError::Malformed(format!("domain value {pattern}")));
                };
                let Loaded { info, mixops, runtime, .. } = self.loaded;
                let float = |text: &str| offload::float_literal(*runtime, info, text);
                let value = domain_value(self.arena, info, mixops, "pattern", sort, text, float)
                    .map_err(EncodeError::Literal)?;
                Ok(case!(self.arena, "KDV pattern", [value], "kore")?)
            }
            Pattern::App(name, sorts, args) => {
                let connective = name.strip_prefix('\\');
                if connective.is_none() && check && !self.loaded.known.contains(name) {
                    return Err(if self.aliases.contains(name) {
                        EncodeError::AliasApplied(name.clone())
                    } else {
                        EncodeError::SymbolUndeclared(name.clone())
                    });
                }
                let value_name = self.text(connective.unwrap_or(name))?;
                let value_sorts = self.sorts(sorts)?;
                let value_args = args
                    .iter()
                    .map(|arg| self.kore_checked(arg, check))
                    .collect::<Result<Vec<_>>>()?;
                let value_args = self.list(typ("kore"), value_args)?;
                if connective.is_some() {
                    Ok(case!(
                        self.arena,
                        "KML text sort* kore*",
                        [value_name, value_sorts, value_args],
                        "kore"
                    )?)
                } else {
                    Ok(case!(
                        self.arena,
                        "KAPP symid sort* kore*",
                        [value_name, value_sorts, value_args],
                        "kore"
                    )?)
                }
            }
        })
    }

    /// A pattern whose applications are of known symbols.
    pub fn kore(&mut self, pattern: &Pattern) -> Result<Value> {
        self.kore_checked(pattern, true)
    }

    // - Attributes

    /// An attribute; its arguments may name symbols that are not declared.
    fn kattr(&mut self, attr: &Attr) -> Result<Value> {
        let Pattern::App(name, sorts, args) = attr else {
            return Err(EncodeError::Malformed(format!("attribute {attr:?}")));
        };
        let value_name = self.text(name)?;
        let value_sorts = self.sorts(sorts)?;
        let value_args = args
            .iter()
            .map(|arg| self.kore_checked(arg, false))
            .collect::<Result<Vec<_>>>()?;
        let value_args = self.list(typ("kore"), value_args)?;
        Ok(case!(
            self.arena,
            "KATTR text sort* kore*",
            [value_name, value_sorts, value_args],
            "kattr"
        )?)
    }

    /// Attributes as a `kattr*` value.
    fn kattrs(&mut self, attrs: &[Attr]) -> Result<Value> {
        let values = attrs
            .iter()
            .map(|attr| self.kattr(attr))
            .collect::<Result<Vec<_>>>()?;
        self.list(typ("kattr"), values)
    }
}

// == Definitions

/// The names an application may use: the declared symbols.
pub fn known_symbols(definition: &Definition) -> HashSet<String> {
    definition
        .sentences()
        .filter_map(|sentence| match sentence {
            Sentence::SymbolDecl { name, .. } => Some(name.clone()),
            _ => None,
        })
        .collect()
}

/// The declared aliases, which an application may not use.
fn alias_names(definition: &Definition) -> HashSet<String> {
    definition
        .sentences()
        .filter_map(|sentence| match sentence {
            Sentence::AliasDecl { name } => Some(name.clone()),
            _ => None,
        })
        .collect()
}

/// A definition as a `kdefinition` value, and its number of axioms.
pub fn definition(
    arena: &mut Arena,
    loaded: &Loaded,
    definition: &Definition,
) -> Result<(Value, usize)> {
    // Each sentence goes to its part, in file order
    let aliases = alias_names(definition);
    let mut builder = Builder::new(arena, loaded, &aliases);
    let mut sorts = Vec::new();
    let mut symbols = Vec::new();
    let mut axioms = Vec::new();
    for sentence in definition.sentences() {
        match sentence {
            Sentence::SortDecl { name, attrs, .. } => {
                let value_name = builder.text(name)?;
                let value_attrs = builder.kattrs(attrs)?;
                sorts.push((value_name, value_attrs));
            }
            Sentence::SymbolDecl { name, params, args, result, attrs } => {
                let value_name = builder.text(name)?;
                let value_params = params
                    .iter()
                    .map(|param| builder.text(param))
                    .collect::<Result<Vec<_>>>()?;
                let value_params = builder.list(typ("sortid"), value_params)?;
                let value_args = builder.sorts(args)?;
                let value_result = builder.sort(result)?;
                let value_attrs = builder.kattrs(attrs)?;
                let value_decl = case!(
                    builder.arena,
                    "KSYMBOL sortid* sort* '->' sort kattr*",
                    [value_params, value_args, value_result, value_attrs],
                    "ksymdecl"
                )?;
                symbols.push((value_name, value_decl));
            }
            Sentence::Axiom { pattern, attrs } => {
                let value_pattern = builder.kore(pattern)?;
                let value_attrs = builder.kattrs(attrs)?;
                axioms.push(case!(
                    builder.arena,
                    "AXIOM kore kattr*",
                    [value_pattern, value_attrs],
                    "kaxiom"
                )?);
            }
            Sentence::Import(_) | Sentence::Claim { .. } | Sentence::AliasDecl { .. } => {}
        }
    }
    // The parts, as the fields of a kdefinition
    let count = axioms.len();
    let value_sorts = builder.map(typ("sortid"), typ::make::list(typ("kattr")), sorts)?;
    let value_symbols = builder.map(typ("symid"), typ("ksymdecl"), symbols)?;
    let value_axioms = builder.list(typ("kaxiom"), axioms)?;
    let field = |name: &str, value| ValueField {
        atom: phrase!(node: Atom::Keyword(name.to_owned()), span: Span::default()),
        value,
    };
    let fields = vec![
        field("KSORTS", value_sorts),
        field("KSYMBOLS", value_symbols),
        field("AXIOMS", value_axioms),
    ];
    let value =
        value::make::structure(builder.arena, rc(typ("kdefinition")), fields, Span::default())?;
    Ok((value, count))
}

/// A pattern, such as krun's initial term, as a `kore` value.
pub fn kore(arena: &mut Arena, loaded: &Loaded, pattern: &Pattern) -> Result<Value> {
    Builder::new(arena, loaded, &HashSet::new()).kore(pattern)
}

/// The input of `Krun`, a definition and an initial term, as one tuple.
pub fn program(arena: &mut Arena, value_definition: Value, value_init: Value) -> Result<Value> {
    let typ = typ::make::tuple(vec![typ("kdefinition"), typ("kore")]);
    let values = vec![value_definition, value_init];
    Ok(value::make::tuple(arena, typ.node.into(), values, Span::default())?)
}

// == Domain values and the K runtime's answers

fn make_case(arena: &mut Arena, mixop: Mixop, args: Vec<Value>, typ: Typ) -> Answer<Value> {
    let mixfix = Mixfix::new(&arena.mixop, mixop, args).expect("mixop arity matches");
    let typ = Rc::new(typ.node);
    value::make::case_flat(arena, typ, mixfix, Span::default()).map_err(value_error)
}

fn make_list(arena: &mut Arena, typ_elem: Typ, values: Vec<Value>) -> Answer<Value> {
    let typ = Rc::new(typ::make::list(typ_elem).node);
    value::make::list(arena, typ, values, Span::default()).map_err(value_error)
}

fn make_text(arena: &mut Arena, text: &str) -> Answer<Value> {
    value::make::text(arena, text.to_owned(), Span::default()).map_err(value_error)
}

fn make_sort(arena: &mut Arena, mixops: &Mixops, sort: &Sort) -> Answer<Value> {
    match sort {
        Sort::Sort(name, sorts) => {
            let name = make_text(arena, name)?;
            let sorts = sorts
                .iter()
                .map(|sort| make_sort(arena, mixops, sort))
                .collect::<Answer<_>>()?;
            let sorts = make_list(arena, typ("sort"), sorts)?;
            make_case(arena, mixops.sort, vec![name, sorts], typ("sort"))
        }
        Sort::Var(name) => {
            let name = make_text(arena, name)?;
            make_case(arena, mixops.sortvar, vec![name], typ("sort"))
        }
    }
}

/// A domain value `\dv{S}("text")` by the hook of `S`, as a case of
/// `typ_name`, `term` or `pattern`, which share these cases. `float` writes a
/// Float in the runtime's output format; a KVar, and a value of a sort
/// without a hook, stay text.
pub fn domain_value(
    arena: &mut Arena,
    info: &Info,
    mixops: &Mixops,
    typ_name: &str,
    sort: &Sort,
    t: &str,
    float: impl FnOnce(&str) -> Answer<String>,
) -> Answer<Value> {
    let bad = || format!("bad domain value {t:?} of {sort}");
    let hook = match sort {
        Sort::Sort(name, _) => info.sort_hook(name),
        Sort::Var(_) => return Err(bad()),
    };
    // The hook of the sort says how its text reads
    let (mixop, arg) = match hook {
        Some("INT.Int") => {
            let i = super::texts::parse_base(t, 10).ok_or_else(bad)?;
            let i = value::make::int(arena, i, Span::default()).map_err(value_error)?;
            (mixops.int, i)
        }
        Some("BOOL.Bool") => {
            let b = match t {
                "true" => true,
                "false" => false,
                _ => return Err(bad()),
            };
            let b = value::make::bool(arena, b, Span::default()).map_err(value_error)?;
            (mixops.bool, b)
        }
        Some("STRING.String") => (mixops.string, make_text(arena, t)?),
        Some("FLOAT.Float") => (mixops.float, make_text(arena, &float(t)?)?),
        Some("BYTES.Bytes") => {
            let bytes = t
                .chars()
                .map(|c| {
                    let n = Natural::try_from(BigInt::from(u32::from(c))).map_err(value_error)?;
                    value::make::nat(arena, n, Span::default()).map_err(value_error)
                })
                .collect::<Answer<_>>()?;
            (mixops.bytes, make_list(arena, typ::make::nat(), bytes)?)
        }
        Some("KVAR.KVar") | None => {
            let sort = make_sort(arena, mixops, sort)?;
            let t = make_text(arena, t)?;
            return make_case(arena, mixops.dv, vec![sort, t], typ(typ_name));
        }
        Some(hook) => return Err(format!("domain value of {sort}, hooked {hook}")),
    };
    make_case(arena, mixop, vec![arg], typ(typ_name))
}

/// A domain value, or a constructor applied to such terms, as
/// `G1Point(x, y)` of KRYPTO. The runtime writes a Float in its output
/// format already.
pub fn term(arena: &mut Arena, info: &Info, mixops: &Mixops, pattern: &Pattern) -> Answer<Value> {
    match pattern {
        Pattern::App(symbol, sorts, args) if symbol == "\\dv" => {
            match (sorts.as_slice(), args.as_slice()) {
                ([sort], [Pattern::Str(t)]) => {
                    domain_value(arena, info, mixops, "term", sort, t, |t| Ok(t.to_owned()))
                }
                _ => Err(format!("bad domain value {pattern}")),
            }
        }
        Pattern::App(symbol, sorts, args) => {
            let symbol = make_text(arena, symbol)?;
            let sorts = sorts
                .iter()
                .map(|sort| make_sort(arena, mixops, sort))
                .collect::<Answer<_>>()?;
            let sorts = make_list(arena, typ("sort"), sorts)?;
            let args = args
                .iter()
                .map(|arg| term(arena, info, mixops, arg))
                .collect::<Answer<_>>()?;
            let args = make_list(arena, typ("term"), args)?;
            make_case(arena, mixops.app, vec![symbol, sorts, args], typ("term"))
        }
        _ => Err(format!("the runtime gives {pattern}, not a term")),
    }
}

/// `OK term`, of `res<term>`.
pub fn ok(arena: &mut Arena, mixops: &Mixops, term: Value) -> Answer<Value> {
    let typ = typ::make::var(id("res"), vec![typ("term")]);
    make_case(arena, mixops.ok, vec![term], typ)
}

/// `ERROR`, of `res<term>`.
pub fn error(arena: &mut Arena, mixops: &Mixops) -> Answer<Value> {
    let typ = typ::make::var(id("res"), vec![typ("term")]);
    make_case(arena, mixops.error, vec![], typ)
}
