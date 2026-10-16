//! Value field JSON as an atom/value pair
//!
//! Named Rust fields serialize as a two-element tuple, preserving the payload format.

use serde::{Deserializer, Serializer};
use serde_state::{DeserializeState, SerializeState};

use crate::lang::data::notation::AtomPhrase;

use super::super::{Value, ValueField};
use super::{DecodeContext, EncodeContext};

// = Encode

impl SerializeState<EncodeContext<'_>> for ValueField {
    fn serialize_state<S: Serializer>(
        &self,
        serializer: S,
        ctx: &EncodeContext<'_>,
    ) -> Result<S::Ok, S::Error> {
        (&self.atom, &self.value).serialize_state(serializer, ctx)
    }
}

// = Decode

impl<'de> DeserializeState<'de, DecodeContext<'_>> for ValueField {
    fn deserialize_state<D: Deserializer<'de>>(
        ctx: &mut DecodeContext<'_>,
        deserializer: D,
    ) -> Result<Self, D::Error> {
        let (atom, value) = <(AtomPhrase, Value)>::deserialize_state(ctx, deserializer)?;
        Ok(Self { atom, value })
    }
}
