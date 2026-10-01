//! Syntax-key wrappers shared by language collections
//!
//! `ByKey` gives a key the `Eq`/`Ord` of its `SyntaxCmp`,
//! so two identifiers with different spans collide in a map or set.
//! Identifier keys also borrow as their bare `String`, for lookups by name.

use std::{borrow::Borrow, cmp::Ordering};

use crate::lang::{common::Id, traits::cmp::SyntaxCmp};

/// A collection key compared by syntax.
#[repr(transparent)]
#[derive(Clone, Debug)]
pub(crate) struct ByKey<K: ?Sized>(pub(crate) K);

impl Borrow<String> for ByKey<Id> {
    fn borrow(&self) -> &String {
        &self.0.node
    }
}

impl<K: SyntaxCmp + ?Sized> PartialEq for ByKey<K> {
    fn eq(&self, key_other: &Self) -> bool {
        self.cmp(key_other) == Ordering::Equal
    }
}

impl<K: SyntaxCmp + ?Sized> Eq for ByKey<K> {}

impl<K: SyntaxCmp + ?Sized> PartialOrd for ByKey<K> {
    fn partial_cmp(&self, key_other: &Self) -> Option<Ordering> {
        Some(self.cmp(key_other))
    }
}

impl<K: SyntaxCmp + ?Sized> Ord for ByKey<K> {
    fn cmp(&self, key_other: &Self) -> Ordering {
        self.0.syntax_cmp(&key_other.0)
    }
}
