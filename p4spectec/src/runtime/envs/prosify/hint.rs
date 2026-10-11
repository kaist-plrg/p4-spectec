//! Prose rendering hints indexed by their definition
//!
//! Variant cases, meta-functions, and relations each own a `Hints`;
//! conversion looks them up by name when it meets a reference.

use std::collections::BTreeMap;

use crate::lang::data::notation::tree::Mixop;

use crate::lang::sl::ast::Id;

use crate::lang::pl::annot::Hints;

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
/// What a hint set belongs to.
enum HintKey {
    /// A variant case: type name and mixfix operator.
    Case(String, Mixop),
    /// A meta-function.
    Func(String),
    /// A relation.
    Rel(String),
}

#[derive(Debug, Default)]
/// Prose hints by definition.
pub struct HEnv(BTreeMap<HintKey, Hints>);

impl HEnv {
    /// Records the hints of a variant case.
    pub fn insert_case(&mut self, id_typ: &Id, mixop: &Mixop, hints: Hints) {
        let key = HintKey::Case(id_typ.node.clone(), mixop.clone());
        self.0.insert(key, hints);
    }

    /// Records the hints of a meta-function.
    pub fn insert_func(&mut self, id_func: &Id, hints: Hints) {
        let key = HintKey::Func(id_func.node.clone());
        self.0.insert(key, hints);
    }

    /// Records the hints of a relation.
    pub fn insert_rel(&mut self, id_rel: &Id, hints: Hints) {
        let key = HintKey::Rel(id_rel.node.clone());
        self.0.insert(key, hints);
    }

    /// The located hints of a variant case, if any.
    pub fn get_case(&self, id_typ: &Id, mixop: &Mixop) -> Option<&Hints> {
        let key = HintKey::Case(id_typ.node.clone(), mixop.clone());
        self.0.get(&key)
    }

    /// The located hints of a meta-function, if any.
    pub fn get_func(&self, id_func: &Id) -> Option<&Hints> {
        let key = HintKey::Func(id_func.node.clone());
        self.0.get(&key)
    }

    /// The located hints of a relation, if any.
    pub fn get_rel(&self, id_rel: &Id) -> Option<&Hints> {
        let key = HintKey::Rel(id_rel.node.clone());
        self.0.get(&key)
    }
}
