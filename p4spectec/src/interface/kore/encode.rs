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
        notation::flat::{Mixfix, Mixop},
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
    ffi::Runtime,
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
    /// A domain value that does not read by the hook of its sort, or an
    /// answer of the K runtime that is not a term.
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

/// A case of the given notation and type, from its arguments.
fn make_case(arena: &mut Arena, mixop: Mixop, args: Vec<Value>, typ: Typ) -> Result<Value> {
    let mixfix = Mixfix::new(&arena.mixop, mixop, args).expect("mixop arity matches");
    Ok(value::make::case_flat(arena, rc(typ), mixfix, Span::default())?)
}

/// A list value of the given element type.
fn make_list(arena: &mut Arena, typ_elem: Typ, values: Vec<Value>) -> Result<Value> {
    let typ = rc(typ::make::list(typ_elem));
    Ok(value::make::list(arena, typ, values, Span::default())?)
}

/// A text value.
fn make_text(arena: &mut Arena, text: &str) -> Result<Value> {
    Ok(value::make::text(arena, text.to_owned(), Span::default())?)
}

/// A sort as a `sort` value.
fn make_sort(arena: &mut Arena, mixops: &Mixops, sort: &Sort) -> Result<Value> {
    match sort {
        Sort::Sort(name, sorts) => {
            let name = make_text(arena, name)?;
            let sorts = make_sorts(arena, mixops, sorts)?;
            make_case(arena, mixops.sort, vec![name, sorts], typ("sort"))
        }
        Sort::Var(name) => {
            let name = make_text(arena, name)?;
            make_case(arena, mixops.sortvar, vec![name], typ("sort"))
        }
    }
}

/// Sorts as a `sort*` value.
fn make_sorts(arena: &mut Arena, mixops: &Mixops, sorts: &[Sort]) -> Result<Value> {
    let sorts = sorts
        .iter()
        .map(|sort| make_sort(arena, mixops, sort))
        .collect::<Result<_>>()?;
    make_list(arena, typ("sort"), sorts)
}

/// A map from its entries, in order.
fn make_map(
    arena: &mut Arena,
    mixops: &Mixops,
    typ_key: Typ,
    typ_value: Typ,
    entries: Vec<(Value, Value)>,
) -> Result<Value> {
    let typ_pair = typ_pair(typ_key.clone(), typ_value.clone());
    let pairs = entries
        .into_iter()
        .map(|(key, value)| make_case(arena, mixops.pair, vec![key, value], typ_pair.clone()))
        .collect::<Result<_>>()?;
    let list = make_list(arena, typ_pair, pairs)?;
    make_case(arena, mixops.braces, vec![list], typ_map(typ_key, typ_value))
}

/// A builder of the patterns of one definition, in one arena.
pub struct Builder<'a> {
    arena: &'a mut Arena,
    /// The definition: applications must be of its declared symbols, and
    /// domain values read by the hooks of their sorts.
    loaded: &'a Loaded,
    /// The definition's K runtime, which writes Float literals in its output format.
    runtime: &'a Runtime,
    /// Declared aliases, which may not be applied.
    aliases: &'a HashSet<String>,
}

impl<'a> Builder<'a> {
    /// A builder into an arena, for the definition `loaded`.
    pub fn new(
        arena: &'a mut Arena,
        loaded: &'a Loaded,
        runtime: &'a Runtime,
        aliases: &'a HashSet<String>,
    ) -> Self {
        Self { arena, loaded, runtime, aliases }
    }

    /// A pattern; with `check`, an application must be of a known symbol.
    fn kore_checked(&mut self, pattern: &Pattern, check: bool) -> Result<Value> {
        let Loaded { info, mixops, .. } = self.loaded;
        // Terms nest deeply, e.g. long K sequences and maps
        stacker::maybe_grow(64 * 1024, 16 * 1024 * 1024, || match pattern {
            Pattern::Var(name, sort) => {
                let value_name = make_text(self.arena, name)?;
                let value_sort = make_sort(self.arena, mixops, sort)?;
                make_case(self.arena, mixops.kvar, vec![value_name, value_sort], typ("kore"))
            }
            Pattern::SetVar(name, sort) => {
                let value_name = make_text(self.arena, name)?;
                let value_sort = make_sort(self.arena, mixops, sort)?;
                make_case(self.arena, mixops.ksetvar, vec![value_name, value_sort], typ("kore"))
            }
            Pattern::Str(text) => {
                let value_text = make_text(self.arena, text)?;
                make_case(self.arena, mixops.kstr, vec![value_text], typ("kore"))
            }
            Pattern::App(name, sorts, args) if name == "\\dv" => {
                let ([sort], [Pattern::Str(text)]) = (sorts.as_slice(), args.as_slice()) else {
                    return Err(EncodeError::Malformed(format!("domain value {pattern}")));
                };
                let float = |text: &str| offload::float_literal(self.runtime, info, text);
                let value = domain_value(self.arena, info, mixops, "pattern", sort, text, float)?;
                make_case(self.arena, mixops.kdv, vec![value], typ("kore"))
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
                let value_name = make_text(self.arena, connective.unwrap_or(name))?;
                let value_sorts = make_sorts(self.arena, mixops, sorts)?;
                let value_args = args
                    .iter()
                    .map(|arg| self.kore_checked(arg, check))
                    .collect::<Result<Vec<_>>>()?;
                let value_args = make_list(self.arena, typ("kore"), value_args)?;
                let mixop = if connective.is_some() { mixops.kml } else { mixops.kapp };
                make_case(self.arena, mixop, vec![value_name, value_sorts, value_args], typ("kore"))
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
        let mixops = &self.loaded.mixops;
        let value_name = make_text(self.arena, name)?;
        let value_sorts = make_sorts(self.arena, mixops, sorts)?;
        let value_args = args
            .iter()
            .map(|arg| self.kore_checked(arg, false))
            .collect::<Result<Vec<_>>>()?;
        let value_args = make_list(self.arena, typ("kore"), value_args)?;
        make_case(self.arena, mixops.kattr, vec![value_name, value_sorts, value_args], typ("kattr"))
    }

    /// Attributes as a `kattr*` value.
    fn kattrs(&mut self, attrs: &[Attr]) -> Result<Value> {
        let values = attrs
            .iter()
            .map(|attr| self.kattr(attr))
            .collect::<Result<Vec<_>>>()?;
        make_list(self.arena, typ("kattr"), values)
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

/// A definition as a `kdefinition` value.
pub fn definition(
    arena: &mut Arena,
    loaded: &Loaded,
    runtime: &Runtime,
    definition: &Definition,
) -> Result<Value> {
    // Each sentence goes to its part, in file order
    let aliases = alias_names(definition);
    let mixops = &loaded.mixops;
    let mut builder = Builder::new(arena, loaded, runtime, &aliases);
    let mut sorts = Vec::new();
    let mut symbols = Vec::new();
    let mut axioms = Vec::new();
    for sentence in definition.sentences() {
        match sentence {
            Sentence::SortDecl { name, attrs, .. } => {
                let value_name = make_text(builder.arena, name)?;
                let value_attrs = builder.kattrs(attrs)?;
                sorts.push((value_name, value_attrs));
            }
            Sentence::SymbolDecl { name, params, args, result, attrs } => {
                let value_name = make_text(builder.arena, name)?;
                let value_params = params
                    .iter()
                    .map(|param| make_text(builder.arena, param))
                    .collect::<Result<Vec<_>>>()?;
                let value_params = make_list(builder.arena, typ("sortid"), value_params)?;
                let value_args = make_sorts(builder.arena, mixops, args)?;
                let value_result = make_sort(builder.arena, mixops, result)?;
                let value_attrs = builder.kattrs(attrs)?;
                let values = vec![value_params, value_args, value_result, value_attrs];
                let value_decl = make_case(builder.arena, mixops.ksymbol, values, typ("ksymdecl"))?;
                symbols.push((value_name, value_decl));
            }
            Sentence::Axiom { pattern, attrs } => {
                let value_pattern = builder.kore(pattern)?;
                let value_attrs = builder.kattrs(attrs)?;
                let values = vec![value_pattern, value_attrs];
                axioms.push(make_case(builder.arena, mixops.kaxiom, values, typ("kaxiom"))?);
            }
            Sentence::Import(_) | Sentence::Claim { .. } | Sentence::AliasDecl { .. } => {}
        }
    }
    // The parts, as the fields of a kdefinition
    let arena = builder.arena;
    let value_sorts = make_map(arena, mixops, typ("sortid"), typ::make::list(typ("kattr")), sorts)?;
    let value_symbols = make_map(arena, mixops, typ("symid"), typ("ksymdecl"), symbols)?;
    let value_axioms = make_list(arena, typ("kaxiom"), axioms)?;
    let field = |name: &str, value| ValueField {
        atom: phrase!(node: Atom::Keyword(name.to_owned()), span: Span::default()),
        value,
    };
    let fields = vec![
        field("KSORTS", value_sorts),
        field("KSYMBOLS", value_symbols),
        field("AXIOMS", value_axioms),
    ];
    Ok(value::make::structure(arena, rc(typ("kdefinition")), fields, Span::default())?)
}

/// A pattern, such as krun's initial term, as a `kore` value.
pub fn kore(
    arena: &mut Arena,
    loaded: &Loaded,
    runtime: &Runtime,
    pattern: &Pattern,
) -> Result<Value> {
    Builder::new(arena, loaded, runtime, &HashSet::new()).kore(pattern)
}

/// The input of `Krun`, a definition and an initial term, as one tuple.
pub fn program(arena: &mut Arena, value_definition: Value, value_init: Value) -> Result<Value> {
    let typ = typ::make::tuple(vec![typ("kdefinition"), typ("kore")]);
    let values = vec![value_definition, value_init];
    Ok(value::make::tuple(arena, typ.node.into(), values, Span::default())?)
}

// == Domain values and the K runtime's answers

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
    float: impl FnOnce(&str) -> std::result::Result<String, String>,
) -> Result<Value> {
    let bad = || EncodeError::Literal(format!("bad domain value {t:?} of {sort}"));
    let hook = match sort {
        Sort::Sort(name, _) => info.sort_hook(name),
        Sort::Var(_) => return Err(bad()),
    };
    // The hook of the sort says how its text reads
    let (mixop, arg) = match hook {
        Some("INT.Int") => {
            let i = super::texts::parse_base(t, 10).ok_or_else(bad)?;
            let i = value::make::int(arena, i, Span::default())?;
            (mixops.int, i)
        }
        Some("BOOL.Bool") => {
            let b = match t {
                "true" => true,
                "false" => false,
                _ => return Err(bad()),
            };
            let b = value::make::bool(arena, b, Span::default())?;
            (mixops.bool, b)
        }
        Some("STRING.String") => (mixops.string, make_text(arena, t)?),
        Some("FLOAT.Float") => {
            let normal = float(t).map_err(EncodeError::Literal)?;
            (mixops.float, make_text(arena, &normal)?)
        }
        Some("BYTES.Bytes") => {
            let bytes = t
                .chars()
                .map(|c| {
                    let n = Natural::try_from(BigInt::from(u32::from(c)))
                        .map_err(|error| EncodeError::Literal(error.to_string()))?;
                    Ok(value::make::nat(arena, n, Span::default())?)
                })
                .collect::<Result<_>>()?;
            (mixops.bytes, make_list(arena, typ::make::nat(), bytes)?)
        }
        Some("KVAR.KVar") | None => {
            let sort = make_sort(arena, mixops, sort)?;
            let t = make_text(arena, t)?;
            return make_case(arena, mixops.dv, vec![sort, t], typ(typ_name));
        }
        Some(hook) => {
            return Err(EncodeError::Literal(format!("domain value of {sort}, hooked {hook}")));
        }
    };
    make_case(arena, mixop, vec![arg], typ(typ_name))
}

/// A domain value, or a constructor applied to such terms, as
/// `G1Point(x, y)` of KRYPTO. The runtime writes a Float in its output
/// format already.
pub fn term(arena: &mut Arena, info: &Info, mixops: &Mixops, pattern: &Pattern) -> Result<Value> {
    match pattern {
        Pattern::App(symbol, sorts, args) if symbol == "\\dv" => {
            match (sorts.as_slice(), args.as_slice()) {
                ([sort], [Pattern::Str(t)]) => {
                    domain_value(arena, info, mixops, "term", sort, t, |t| Ok(t.to_owned()))
                }
                _ => Err(EncodeError::Literal(format!("bad domain value {pattern}"))),
            }
        }
        Pattern::App(symbol, sorts, args) => {
            let symbol = make_text(arena, symbol)?;
            let sorts = make_sorts(arena, mixops, sorts)?;
            let args = args
                .iter()
                .map(|arg| term(arena, info, mixops, arg))
                .collect::<Result<_>>()?;
            let args = make_list(arena, typ("term"), args)?;
            make_case(arena, mixops.app, vec![symbol, sorts, args], typ("term"))
        }
        _ => Err(EncodeError::Literal(format!("the runtime gives {pattern}, not a term"))),
    }
}

/// `OK term`, of `res<term>`.
pub fn ok(arena: &mut Arena, mixops: &Mixops, term: Value) -> Result<Value> {
    let typ = typ::make::var(id("res"), vec![typ("term")]);
    make_case(arena, mixops.ok, vec![term], typ)
}

/// `ERROR`, of `res<term>`.
pub fn error(arena: &mut Arena, mixops: &Mixops) -> Result<Value> {
    let typ = typ::make::var(id("res"), vec![typ("term")]);
    make_case(arena, mixops.error, vec![], typ)
}
