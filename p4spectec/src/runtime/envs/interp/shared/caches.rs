//! Standard hash-map caches used by dynamic evaluation
//!
//! Value computations key directly by immutable runtime values;
//! function calls key by a shared function name and argument slice.
//! Inserting the same call twice therefore replaces its previous result
//! with normal `HashMap` semantics.

use std::{collections::HashMap, rc::Rc};

use crate::lang::data::value::Value;

// == Value cache

/// Results keyed by an immutable value.
pub type ValueCache<V> = HashMap<Value, V>;

// == Call cache

/// A call identity: function name and argument values, shared to avoid copies.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct CallKey {
    /// Function name.
    pub id: Rc<str>,
    /// Argument values.
    pub values: Rc<[Value]>,
}

/// Results keyed by call.
pub type CallCache<V> = HashMap<CallKey, V>;
