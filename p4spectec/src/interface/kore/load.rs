//! KORE as values of the K specification
//!
//! The parsed text is passed on as it is (`kore` and `kdefinition`
//! in `spec-k/1-syntax.watsup`): every axiom with its attributes,
//! and the declarations of sorts, symbols, and aliases.
//! Connectives are named without their backslash.
//! Which axioms are executed and how they are read, down to domain values,
//! is for the specification to decide (`spec-k/3-load.watsup`).
//! For example, `\dv{SortInt{}}("1")` becomes
//! `KML "dv" [SORT "SortInt" []] [KSTR "1"]`.

use std::{collections::HashSet, rc::Rc};

use crate::lang::{
    common::{Id, notation::atom::Atom, source::Span},
    data::{
        arena::Arena,
        notation::flat::{
            Mixfix,
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

use super::ast::{Attr, Definition, Pattern, Sentence, Sort};

// == Errors

/// A failure while turning KORE into values.
#[derive(Debug)]
pub enum LoadError {
    /// A symbol that is neither declared nor an alias.
    SymbolUndeclared(String),
    /// A declaration or attribute of an unexpected form.
    Malformed(String),
    Value(ValueError),
}

impl From<ValueError> for LoadError {
    fn from(error: ValueError) -> Self {
        Self::Value(error)
    }
}

impl std::fmt::Display for LoadError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::SymbolUndeclared(name) => write!(f, "undeclared symbol {name}"),
            Self::Malformed(what) => write!(f, "malformed {what}"),
            Self::Value(error) => error.fmt(f),
        }
    }
}

/// A loading result.
type Result<T> = std::result::Result<T, LoadError>;

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
    /// Declared symbols and aliases; applications must be of one of these.
    known: &'a HashSet<String>,
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
    /// A builder into an arena, checking applications against `known`.
    pub fn new(arena: &'a mut Arena, known: &'a HashSet<String>) -> Self {
        Self { arena, known }
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
            Pattern::App(name, sorts, args) => {
                let connective = name.strip_prefix('\\');
                if connective.is_none() && check && !self.known.contains(name) {
                    return Err(LoadError::SymbolUndeclared(name.clone()));
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

    /// `OK kore` or `ERROR`, of `res<kore>`: the K runtime's answer, or its
    /// failure.
    pub fn res_kore(&mut self, pattern: Option<&Pattern>) -> Result<Value> {
        let typ = typ::make::var(id("res"), vec![typ("kore")]);
        match pattern {
            Some(pattern) => {
                let value = self.kore(pattern)?;
                self.case_typed("OK x", vec![value], typ)
            }
            None => self.case_typed("ERROR", vec![], typ),
        }
    }

    // - Attributes

    /// An attribute; its arguments may name symbols that are not declared.
    fn kattr(&mut self, attr: &Attr) -> Result<Value> {
        let Pattern::App(name, sorts, args) = attr else {
            return Err(LoadError::Malformed(format!("attribute {attr:?}")));
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

/// The names an application may use: declared symbols and aliases.
pub fn known_symbols(definition: &Definition) -> HashSet<String> {
    definition
        .sentences()
        .filter_map(|sentence| match sentence {
            Sentence::SymbolDecl { name, .. } | Sentence::AliasDecl { name, .. } => {
                Some(name.clone())
            }
            _ => None,
        })
        .collect()
}

/// A definition as a `kdefinition` value, and its number of axioms.
pub fn load_definition(
    arena: &mut Arena,
    known: &HashSet<String>,
    definition: &Definition,
) -> Result<(Value, usize)> {
    // Each sentence goes to its part, in file order
    let mut builder = Builder::new(arena, known);
    let mut sorts = Vec::new();
    let mut symbols = Vec::new();
    let mut aliases = Vec::new();
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
            Sentence::AliasDecl { name, lhs, body } => {
                let Pattern::App(_, _, params) = lhs else {
                    return Err(LoadError::Malformed(format!("alias {name}")));
                };
                let value_params = params
                    .iter()
                    .map(|param| match param {
                        Pattern::Var(x, _) | Pattern::SetVar(x, _) => builder.text(x),
                        _ => Err(LoadError::Malformed(format!(
                            "alias {name}: a parameter is not a variable"
                        ))),
                    })
                    .collect::<Result<Vec<_>>>()?;
                let value_params = builder.list(typ("varid"), value_params)?;
                let value_body = builder.kore(body)?;
                let value_name = builder.text(name)?;
                let value_alias = case!(
                    builder.arena,
                    "ALIAS varid* ':=' kore",
                    [value_params, value_body],
                    "kalias"
                )?;
                aliases.push((value_name, value_alias));
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
            Sentence::Import(_) | Sentence::Claim { .. } => {}
        }
    }
    // The parts, as the fields of a kdefinition
    let count = axioms.len();
    let value_sorts = builder.map(typ("sortid"), typ::make::list(typ("kattr")), sorts)?;
    let value_symbols = builder.map(typ("symid"), typ("ksymdecl"), symbols)?;
    let value_aliases = builder.map(typ("symid"), typ("kalias"), aliases)?;
    let value_axioms = builder.list(typ("kaxiom"), axioms)?;
    let field = |name: &str, value| ValueField {
        atom: phrase!(node: Atom::Keyword(name.to_owned()), span: Span::default()),
        value,
    };
    let fields = vec![
        field("KSORTS", value_sorts),
        field("KSYMBOLS", value_symbols),
        field("KALIASES", value_aliases),
        field("AXIOMS", value_axioms),
    ];
    let value =
        value::make::structure(builder.arena, rc(typ("kdefinition")), fields, Span::default())?;
    Ok((value, count))
}

/// A term, such as krun's initial term, as a `kore` value.
pub fn load_term(arena: &mut Arena, known: &HashSet<String>, pattern: &Pattern) -> Result<Value> {
    Builder::new(arena, known).kore(pattern)
}
