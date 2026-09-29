//! Prose rendering hints indexed by their definition
//!
//! Variant cases, meta-functions, and relations each own a `Hints`;
//! conversion looks them up by name when it meets a reference.

use std::cmp::Ordering;

use crate::lang::{
    common::{
        ds::map::PhraseMap,
        notation::mixop::Mixop,
        source::{Phrase, Span},
    },
    pl::annot::Hints,
    sl::ast::Id,
    traits::{cmp::SyntaxCmp, eq::SyntaxEq},
};

type HintKey = Phrase<HintKeyKind>;

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
/// What a hint set belongs to.
enum HintKeyKind {
    /// A variant case: type name and mixfix operator.
    Case(String, Mixop),
    /// A meta-function.
    Func(String),
    /// A relation.
    Rel(String),
}

impl SyntaxEq for HintKeyKind {
    fn syntax_eq(&self, other: &Self) -> bool {
        self == other
    }
}

impl SyntaxCmp for HintKeyKind {
    fn syntax_cmp(&self, other: &Self) -> Ordering {
        self.cmp(other)
    }
}

#[derive(Debug, Default)]
/// Prose hints by definition.
pub struct HEnv(PhraseMap<HintKey, Hints>);

impl HEnv {
    /// Records the hints of a variant case.
    pub fn insert_case(&mut self, span_decl: &Span, id_typ: &Id, mixop: &Mixop, hints: Hints) {
        let key = crate::phrase! { node: HintKeyKind::Case(id_typ.node.clone(), mixop.clone()), span: span_decl.clone() };
        self.0.insert(key, hints);
    }

    /// Records the hints of a meta-function.
    pub fn insert_func(&mut self, id_func: &Id, hints: Hints) {
        let key = crate::phrase! { node: HintKeyKind::Func(id_func.node.clone()), span: id_func.span.clone() };
        self.0.insert(key, hints);
    }

    /// Records the hints of a relation.
    pub fn insert_rel(&mut self, id_rel: &Id, hints: Hints) {
        let key = crate::phrase! { node: HintKeyKind::Rel(id_rel.node.clone()), span: id_rel.span.clone() };
        self.0.insert(key, hints);
    }

    /// The declaration location and hints of a variant case, if any.
    pub fn get_case(&self, id_typ: &Id, mixop: &Mixop) -> Option<(&Span, &Hints)> {
        let key = crate::phrase! { node: HintKeyKind::Case(id_typ.node.clone(), mixop.clone()), span: id_typ.span.clone() };
        self.0
            .get_key_value(&key)
            .map(|(key, hints)| (&key.span, hints))
    }

    /// The declaration location and hints of a meta-function, if any.
    pub fn get_func(&self, id_func: &Id) -> Option<(&Span, &Hints)> {
        let key = crate::phrase! { node: HintKeyKind::Func(id_func.node.clone()), span: id_func.span.clone() };
        self.0
            .get_key_value(&key)
            .map(|(key, hints)| (&key.span, hints))
    }

    /// The declaration location and hints of a relation, if any.
    pub fn get_rel(&self, id_rel: &Id) -> Option<(&Span, &Hints)> {
        let key = crate::phrase! { node: HintKeyKind::Rel(id_rel.node.clone()), span: id_rel.span.clone() };
        self.0
            .get_key_value(&key)
            .map(|(key, hints)| (&key.span, hints))
    }
}
