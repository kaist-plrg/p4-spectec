//! Value field JSON as an atom/value pair
//!
//! Named Rust fields serialize as a two-element tuple, preserving the payload format.

use serde::{Deserialize, Deserializer, Serialize, Serializer};

use crate::lang::data::notation::AtomPhrase;

use super::super::{Value, ValueField};

// = Encode

impl Serialize for ValueField {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        (&self.atom, &self.value).serialize(serializer)
    }
}

// = Decode

impl<'de> Deserialize<'de> for ValueField {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let (atom, value) = <(AtomPhrase, Value)>::deserialize(deserializer)?;
        Ok(Self { atom, value })
    }
}
