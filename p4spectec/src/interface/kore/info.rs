//! What the runtime boundary needs to know of a definition
//!
//! Sorts by their hook (`INT.Int` is `SortInt`), the constructors of
//! collection sorts from their `element`, `concat`, and `unit` attributes,
//! and the sorts that stand for a number (`Sort8` for MInt widths).
//! For example, `SortMap` has the hook `MAP.Map`, the element `_|->_`,
//! the concatenation `_Map_`, and the unit `.Map`.

use std::collections::HashMap;

use super::ast::{Attr, Definition, Pattern, Sentence, Sort};

/// The kinds of collection sorts.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Collection {
    Map,
    Set,
    List,
    RangeMap,
}

impl Collection {
    fn of_hook(hook: &str) -> Option<Self> {
        match hook {
            "MAP.Map" => Some(Self::Map),
            "SET.Set" => Some(Self::Set),
            "LIST.List" => Some(Self::List),
            "RANGEMAP.RangeMap" => Some(Self::RangeMap),
            _ => None,
        }
    }
}

/// The constructors of a collection sort.
#[derive(Clone, Debug)]
pub struct Constructors {
    pub collection: Collection,
    pub element: String,
    pub concat: String,
    pub unit: String,
}

/// A constructor's place in its collection sort.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Role {
    Element,
    Concat,
    Unit,
}

/// Facts about a definition for converting terms at the runtime boundary.
#[derive(Debug, Default)]
pub struct Info {
    /// The hook of each hooked sort.
    sort_hooks: HashMap<String, String>,
    /// The constructors of each collection sort.
    pub collections: HashMap<String, Constructors>,
    /// Each collection constructor: its sort and role.
    pub constructors: HashMap<String, (String, Role)>,
    /// The sort standing for each number, e.g. `8` for `Sort8`.
    pub nat_sorts: HashMap<String, String>,
}

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
    pub fn new(definition: &Definition) -> Self {
        let mut info = Self::default();
        for sentence in definition.sentences() {
            let Sentence::SortDecl { name, attrs, .. } = sentence else {
                continue;
            };
            if let Some(hook) = string_attr("hook", attrs) {
                info.sort_hooks.insert(name.clone(), hook.to_owned());
                let collection = Collection::of_hook(hook);
                let element = symbol_attr("element", attrs);
                let concat = symbol_attr("concat", attrs);
                let unit = symbol_attr("unit", attrs);
                if let (Some(collection), Some(element), Some(concat), Some(unit)) =
                    (collection, element, concat, unit)
                {
                    for (symbol, role) in
                        [(element, Role::Element), (concat, Role::Concat), (unit, Role::Unit)]
                    {
                        info.constructors
                            .insert(symbol.to_owned(), (name.clone(), role));
                    }
                    let constructors = Constructors {
                        collection,
                        element: element.to_owned(),
                        concat: concat.to_owned(),
                        unit: unit.to_owned(),
                    };
                    info.collections.insert(name.clone(), constructors);
                }
            }
            if let Some(n) = string_attr("nat", attrs) {
                info.nat_sorts.insert(n.to_owned(), name.clone());
            }
        }
        info
    }

    /// The hook of a sort.
    pub fn sort_hook(&self, sort: &str) -> Option<&str> {
        self.sort_hooks.get(sort).map(String::as_str)
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

    /// Whether a pattern has a KVar (`KVAR.KVar`) domain value.
    pub fn has_kvar(&self, pattern: &Pattern) -> bool {
        match pattern {
            Pattern::App(dv, sorts, _) if dv == "\\dv" => matches!(
                sorts.as_slice(),
                [Sort::Sort(name, _)] if self.sort_hook(name) == Some("KVAR.KVar")
            ),
            Pattern::App(_, _, args) => args.iter().any(|arg| self.has_kvar(arg)),
            _ => false,
        }
    }
}
