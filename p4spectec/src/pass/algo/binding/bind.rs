//! Singular and repeated identifier bindings
//!
//! `BEnv` records, for each identifier bound by a pattern,
//! whether it occurs once (`Single`)
//! or at several parallel positions (`Multiple`),
//! together with its dimension.
//! In `let (x, x) = e`, `x` is `Multiple`
//! and later gets renamed with an equality side condition.

use crate::lang::common::{Id, ds::map::IdMap};

use crate::lang::il::ast;

use crate::runtime::{dim::Dim, envs::algo::VEnv};

use super::super::{AlgoError, error};

/// One binding occurrence or multiple parallel occurrences.
#[derive(Clone, Debug, PartialEq)]
pub enum Binding {
    /// The identifier is bound at one position.
    Single(Dim),
    /// The identifier is bound at several parallel positions.
    Multiple(Dim),
}

impl Binding {
    pub fn dim(&self) -> &Dim {
        match self {
            Self::Single(dim) | Self::Multiple(dim) => dim,
        }
    }

    pub fn add_iter(self, iter: ast::Iter) -> Self {
        match self {
            Self::Single(dim) => Self::Single(dim.add_iter(iter)),
            Self::Multiple(dim) => Self::Multiple(dim.add_iter(iter)),
        }
    }
}

/// Binding environment keyed by source-insensitive identifier identity.
#[derive(Clone, Debug)]
pub struct BEnv(IdMap<Binding>);

impl BEnv {
    pub fn new() -> Self {
        Self(IdMap::new())
    }

    /// A single binding of `id` at dimension zero.
    pub fn singleton(id: Id, typ: ast::Typ) -> Self {
        let mut benv = Self::new();
        benv.insert(id, Binding::Single(Dim::new(typ, vec![])));
        benv
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    fn insert(&mut self, id: Id, binding: Binding) {
        self.0.insert(id, binding);
    }

    pub fn iter(&self) -> impl Iterator<Item = (&Id, &Binding)> {
        self.0.iter()
    }

    /// Drops the single/multiple distinction, keeping each dimension.
    pub fn flatten(&self) -> VEnv {
        self.iter()
            .map(|(id, binding)| (id.clone(), binding.dim().clone()))
            .collect()
    }

    /// Adds one more iteration to every binding.
    pub fn add_iter(self, iter: ast::Iter) -> Self {
        let entries = self
            .iter()
            .map(|(id, binding)| (id.clone(), binding.clone().add_iter(iter)))
            .collect();
        Self(entries)
    }

    /// Combines parallel bindings; an identifier on both sides is `Multiple`.
    pub fn union(mut self, other: Self) -> Result<Self, AlgoError> {
        for (id, binding_r) in other.iter() {
            let binding_l = self
                .iter()
                .find(|(stored, _)| stored.node == id.node)
                .map(|(_, binding)| binding.clone());
            let Some(binding_l) = binding_l else {
                self.insert(id.clone(), binding_r.clone());
                continue;
            };
            // Both sides must agree on the dimension
            let dim_l = binding_l.dim();
            let dim_r = binding_r.dim();
            if !(dim_l.sub(dim_r) && dim_r.sub(dim_l)) {
                return Err(error::binding::binding_dimension_mismatch(id, dim_l, dim_r));
            }
            let dim = dim_l.clone();
            self.insert(id.clone(), Binding::Multiple(dim));
        }
        Ok(self)
    }
}
