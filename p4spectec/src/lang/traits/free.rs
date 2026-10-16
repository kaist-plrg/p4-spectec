//! Free identifiers and dimension-aware variables shared across language stages
//!
//! `FreeIds` collects names, while `FreeVars` keeps type and iteration data.

use std::rc::Rc;

use crate::lang::{
    common::{ds::set::IdSet, source::NotePhrase},
    traits::eq::SyntaxEq,
};

use crate::lang::il::ast::Var;

// == Free identifiers

/// Collects free term identifiers from syntax.
pub trait FreeIds {
    /// Returns the free term identifiers contained in `self`.
    fn free_ids(&self) -> IdSet {
        let mut ids_free = IdSet::new();
        self.free_ids_into(&mut ids_free);
        ids_free
    }

    /// Adds the free term identifiers contained in `self` to `ids_free`.
    fn free_ids_into(&self, ids_free: &mut IdSet) {
        ids_free.append(self.free_ids());
    }
}

// - Text

impl FreeIds for String {
    fn free_ids(&self) -> IdSet {
        IdSet::new()
    }
}

impl FreeIds for str {
    fn free_ids(&self) -> IdSet {
        IdSet::new()
    }
}

// - Source annotations

impl<T: FreeIds, N, S> FreeIds for NotePhrase<T, N, S> {
    fn free_ids_into(&self, ids_free: &mut IdSet) {
        self.node.free_ids_into(ids_free);
    }
}

// - Containers

impl<T: FreeIds + ?Sized> FreeIds for Box<T> {
    fn free_ids_into(&self, ids_free: &mut IdSet) {
        self.as_ref().free_ids_into(ids_free);
    }
}

impl<T: FreeIds + ?Sized> FreeIds for Rc<T> {
    fn free_ids_into(&self, ids_free: &mut IdSet) {
        self.as_ref().free_ids_into(ids_free);
    }
}

impl<T: FreeIds> FreeIds for Option<T> {
    fn free_ids_into(&self, ids_free: &mut IdSet) {
        if let Some(value) = self {
            value.free_ids_into(ids_free);
        }
    }
}

impl<T: FreeIds> FreeIds for [T] {
    fn free_ids_into(&self, ids_free: &mut IdSet) {
        for item in self {
            item.free_ids_into(ids_free);
        }
    }
}

// == Free variables

/// Collects dimension-aware free variables from syntax.
pub trait FreeVars {
    /// Returns the free variables contained in `self`.
    fn free_vars(&self) -> Vec<Var>;

    /// Adds the free variables contained in `self` to `vars_free`.
    fn free_vars_into(&self, vars_free: &mut Vec<Var>) {
        for var in self.free_vars() {
            if !vars_free.iter().any(|var_free| var_free.syntax_eq(&var)) {
                vars_free.push(var);
            }
        }
    }
}

// - Containers

impl<T: FreeVars + ?Sized> FreeVars for Box<T> {
    fn free_vars(&self) -> Vec<Var> {
        self.as_ref().free_vars()
    }

    fn free_vars_into(&self, vars_free: &mut Vec<Var>) {
        self.as_ref().free_vars_into(vars_free);
    }
}

impl<T: FreeVars + ?Sized> FreeVars for Rc<T> {
    fn free_vars(&self) -> Vec<Var> {
        self.as_ref().free_vars()
    }

    fn free_vars_into(&self, vars_free: &mut Vec<Var>) {
        self.as_ref().free_vars_into(vars_free);
    }
}

impl<T: FreeVars> FreeVars for Option<T> {
    fn free_vars(&self) -> Vec<Var> {
        self.as_ref().map(FreeVars::free_vars).unwrap_or_default()
    }

    fn free_vars_into(&self, vars_free: &mut Vec<Var>) {
        if let Some(value) = self {
            value.free_vars_into(vars_free);
        }
    }
}

impl<T: FreeVars> FreeVars for [T] {
    fn free_vars(&self) -> Vec<Var> {
        let mut vars_free = Vec::new();
        self.free_vars_into(&mut vars_free);
        vars_free
    }

    fn free_vars_into(&self, vars_free: &mut Vec<Var>) {
        for item in self {
            item.free_vars_into(vars_free);
        }
    }
}
