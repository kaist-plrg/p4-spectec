//! Filled-case JSON encoding through the value arena
//!
//! Both encodings retain the filled notation's variant layout.
//! `CaseRef` borrows atoms and values for serialization;
//! `Case` owns decoded atoms and values before interning the mixop.
//! Argument values use the encoding selected by the context.

use std::slice;

use serde::{Deserializer, Serializer};
use serde_derive_state::{DeserializeState, SerializeState};
use serde_state::{DeserializeState, SerializeState};

use crate::lang::data::notation::{self, AtomPhrase, MixopArena};

use super::{
    super::{Value, ValueCase},
    DecodeContext, EncodeContext,
};

// = Encode

/// A case written as its filled notation tree.
#[derive(SerializeState)]
#[serde(rename = "Mixfix")]
#[serde(serialize_state = "EncodeContext<'arena>", ser_parameters = "'arena")]
enum CaseRef<'a> {
    Arg(#[serde(state)] &'a Value),
    Atom(#[serde(state)] &'a AtomPhrase),
    Brack(
        #[serde(state)] &'a AtomPhrase,
        #[serde(state)] Box<CaseRef<'a>>,
        #[serde(state)] &'a AtomPhrase,
    ),
    Infix(
        #[serde(state)] Box<CaseRef<'a>>,
        #[serde(state)] &'a AtomPhrase,
        #[serde(state)] Box<CaseRef<'a>>,
    ),
    Seq(#[serde(state)] Vec<CaseRef<'a>>),
}

impl<'a> CaseRef<'a> {
    /// Fills a case's mixop, borrowing its atoms and arguments.
    fn from_flat(arena_mixop: &'a MixopArena, value_case: &'a ValueCase) -> Self {
        Self::from_flat_inner(arena_mixop, *value_case.mixop(), &mut value_case.args().iter())
    }

    /// Visits argument positions in notation order.
    fn from_flat_inner(
        arena_mixop: &'a MixopArena,
        mixop: notation::flat::Mixop,
        values: &mut slice::Iter<'a, Value>,
    ) -> Self {
        match arena_mixop.kind(mixop) {
            notation::flat::MixopKind::Arg => {
                Self::Arg(values.next().expect("a case fills every position"))
            }
            notation::flat::MixopKind::Atom(atom) => Self::Atom(atom),
            notation::flat::MixopKind::Brack(atom_l, mixop, atom_r) => {
                let case = Self::from_flat_inner(arena_mixop, *mixop, values);
                Self::Brack(atom_l, Box::new(case), atom_r)
            }
            notation::flat::MixopKind::Infix(mixop_l, atom, mixop_r) => {
                let case_l = Self::from_flat_inner(arena_mixop, *mixop_l, values);
                let case_r = Self::from_flat_inner(arena_mixop, *mixop_r, values);
                Self::Infix(Box::new(case_l), atom, Box::new(case_r))
            }
            notation::flat::MixopKind::Seq(mixops) => Self::Seq(
                mixops
                    .iter()
                    .map(|mixop| Self::from_flat_inner(arena_mixop, *mixop, values))
                    .collect(),
            ),
        }
    }
}

impl SerializeState<EncodeContext<'_>> for ValueCase {
    fn serialize_state<S: Serializer>(
        &self,
        serializer: S,
        ctx: &EncodeContext<'_>,
    ) -> Result<S::Ok, S::Error> {
        CaseRef::from_flat(&ctx.arena().mixop, self).serialize_state(serializer, ctx)
    }
}

// = Decode

/// A case read as its filled notation tree.
#[derive(DeserializeState)]
#[serde(rename = "Mixfix")]
#[serde(deserialize_state = "DecodeContext<'arena>", de_parameters = "'arena")]
enum Case {
    Arg(#[serde(state)] Value),
    Atom(#[serde(state)] AtomPhrase),
    Brack(#[serde(state)] AtomPhrase, #[serde(state)] Box<Case>, #[serde(state)] AtomPhrase),
    Infix(#[serde(state)] Box<Case>, #[serde(state)] AtomPhrase, #[serde(state)] Box<Case>),
    Seq(#[serde(state)] Vec<Case>),
}

impl Case {
    /// Splits a decoded case into its mixop and arguments in notation order.
    fn into_parts(self) -> (notation::tree::Mixop, Vec<Value>) {
        let mut values = Vec::new();
        let mixop = self.into_parts_inner(&mut values);
        (mixop, values)
    }

    /// Moves each argument into the output as its position is visited.
    fn into_parts_inner(self, values: &mut Vec<Value>) -> notation::tree::Mixop {
        match self {
            Self::Arg(value) => {
                values.push(value);
                notation::tree::Mixop::Arg
            }
            Self::Atom(atom) => notation::tree::Mixop::Atom(atom),
            Self::Brack(atom_l, case, atom_r) => notation::tree::Mixop::Brack(
                atom_l,
                Box::new(case.into_parts_inner(values)),
                atom_r,
            ),
            Self::Infix(case_l, atom, case_r) => {
                let mixop_l = case_l.into_parts_inner(values);
                let mixop_r = case_r.into_parts_inner(values);
                notation::tree::Mixop::Infix(Box::new(mixop_l), atom, Box::new(mixop_r))
            }
            Self::Seq(cases) => notation::tree::Mixop::Seq(
                cases
                    .into_iter()
                    .map(|case| case.into_parts_inner(values))
                    .collect(),
            ),
        }
    }
}

impl<'de> DeserializeState<'de, DecodeContext<'_>> for ValueCase {
    fn deserialize_state<D: Deserializer<'de>>(
        ctx: &mut DecodeContext<'_>,
        deserializer: D,
    ) -> Result<Self, D::Error> {
        // Decode arguments using the context's encoding
        let case = Case::deserialize_state(ctx, deserializer)?;
        let (mixop, values) = case.into_parts();

        // Intern the decoded notation in the target arena
        let arena_mixop = &mut ctx.arena_mut().mixop;
        let mixop = mixop
            .into_flat(arena_mixop)
            .map_err(::serde::de::Error::custom)?;
        notation::flat::Mixfix::new(arena_mixop, mixop, values).map_err(::serde::de::Error::custom)
    }
}
