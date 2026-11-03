//! What reading and writing KORE needs to know of a definition
//!
//! The declared symbols and aliases, sorts by their hook (`INT.Int` is
//! `SortInt`), the constructors of collection sorts from their `element`,
//! `concat`, and `unit` attributes, and the symbols that bind a variable.
//! For example, `SortMap` has the element `_|->_`,
//! the concatenation `_Map_`, and the unit `.Map`.

use std::collections::{HashMap, HashSet};

use super::ast::{Attr, Definition, Pattern, Sentence};

/// The constructors of a collection sort.
#[derive(Clone, Debug)]
pub struct Constructors {
    pub element: String,
    pub concat: String,
    pub unit: String,
}

/// Facts about a definition for reading and writing KORE.
#[derive(Debug, Default)]
pub struct Info {
    /// The declared symbols, which an application may use.
    symbols: HashSet<String>,
    /// The declared aliases, which an application may not use.
    aliases: HashSet<String>,
    /// The hook of each hooked sort.
    sort_hooks: HashMap<String, String>,
    /// The constructors of each collection sort.
    pub collections: HashMap<String, Constructors>,
    /// The symbols with the `binder` attribute, e.g. lambda.
    binders: HashSet<String>,
}

/// The string an attribute holds, as in `hook{}("INT.Int")`.
fn string_attr<'a>(name: &str, attrs: &'a [Attr]) -> Option<&'a str> {
    attrs.iter().find_map(|attr| match attr {
        Pattern::App(attr_name, _, args) if attr_name == name => match args.as_slice() {
            [Pattern::Str(text)] => Some(text.as_str()),
            _ => None,
        },
        _ => None,
    })
}

/// The symbol an attribute names, as in `element{}(Lbl'UndsPipe'-'-GT-Unds'{}())`.
fn symbol_attr<'a>(name: &str, attrs: &'a [Attr]) -> Option<&'a str> {
    attrs.iter().find_map(|attr| match attr {
        Pattern::App(attr_name, _, args) if attr_name == name => match args.as_slice() {
            [Pattern::App(symbol, _, _)] => Some(symbol.as_str()),
            _ => None,
        },
        _ => None,
    })
}

impl Info {
    /// The facts of a definition, from its sort and symbol declarations.
    pub fn new(definition: &Definition) -> Self {
        let mut info = Self::default();
        for sentence in definition.sentences() {
            match sentence {
                Sentence::SymbolDecl { name, attrs, .. } => {
                    info.symbols.insert(name.clone());
                    if attrs.iter().any(
                        |attr| matches!(attr, Pattern::App(attr_name, _, _) if attr_name == "binder"),
                    ) {
                        info.binders.insert(name.clone());
                    }
                }
                Sentence::AliasDecl { name } => {
                    info.aliases.insert(name.clone());
                }
                Sentence::SortDecl { name, attrs, .. } => {
                    if let Some(hook) = string_attr("hook", attrs) {
                        info.sort_hooks.insert(name.clone(), hook.to_owned());
                    }
                    if let (Some(element), Some(concat), Some(unit)) = (
                        symbol_attr("element", attrs),
                        symbol_attr("concat", attrs),
                        symbol_attr("unit", attrs),
                    ) {
                        let constructors = Constructors {
                            element: element.to_owned(),
                            concat: concat.to_owned(),
                            unit: unit.to_owned(),
                        };
                        info.collections.insert(name.clone(), constructors);
                    }
                }
                Sentence::Import(_) | Sentence::Axiom { .. } => {}
            }
        }
        info
    }

    /// Whether a symbol is declared.
    pub fn is_symbol(&self, name: &str) -> bool {
        self.symbols.contains(name)
    }

    /// Whether an alias is declared.
    pub fn is_alias(&self, name: &str) -> bool {
        self.aliases.contains(name)
    }

    /// The hook of a sort.
    pub fn sort_hook(&self, sort: &str) -> Option<&str> {
        self.sort_hooks.get(sort).map(String::as_str)
    }

    /// Whether a symbol binds its first argument.
    pub fn is_binder(&self, symbol: &str) -> bool {
        self.binders.contains(symbol)
    }

    /// Whether the definition has binders.
    pub fn has_binders(&self) -> bool {
        !self.binders.is_empty()
    }

    /// The one sort with a hook, e.g. `SortInt` for `INT.Int`.
    pub fn sort_of_hook(&self, hook: &str) -> Result<&str, String> {
        let mut sorts = self
            .sort_hooks
            .iter()
            .filter(|(_, sort_hook)| *sort_hook == hook)
            .map(|(sort, _)| sort.as_str());
        match (sorts.next(), sorts.next()) {
            (Some(sort), None) => Ok(sort),
            (None, _) => Err(format!("no sort is hooked to {hook}")),
            (Some(_), Some(_)) => Err(format!("several sorts are hooked to {hook}")),
        }
    }
}
