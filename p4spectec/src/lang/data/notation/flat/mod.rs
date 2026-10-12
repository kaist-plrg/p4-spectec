//! Flat representation of notation
//!
//! `Mixop` is a handle to a `MixopKind` whose children are handles
//! in the same arena. Exact equality includes atom spans;
//! canonical equality ignores spans and compares canonical child identities.

use serde_derive_state::{DeserializeState, SerializeState};

use crate::lang::data::intern::Interned;

use super::{AtomPhrase, MixopTag};

use self::external::{DecodeContext, EncodeContext};

mod arena;
mod cmp;
mod convert;
pub mod external;
mod hash;
mod make;
mod print;
mod view;
mod visit;

pub use arena::MixopArena;
pub use view::MixopRef;

// = Notation forms

/// A notation handle valid only in its issuing arena.
pub type Mixop = Interned<MixopKind>;

/// An interned notation with one argument per position.
pub type Mixfix<T> = super::Mixfix<Mixop, T>;

/// One interned notation node, with child handles in the same arena.
#[derive(Debug, PartialEq, Eq, Hash, SerializeState, DeserializeState)]
#[serde(rename = "Mixop")]
#[serde(serialize_state = "EncodeContext<'arena>", ser_parameters = "'arena")]
#[serde(deserialize_state = "DecodeContext<'de>")]
pub enum MixopKind {
    Arg,
    Atom(#[serde(state)] AtomPhrase),
    Brack(#[serde(state)] AtomPhrase, #[serde(state)] Mixop, #[serde(state)] AtomPhrase),
    Infix(#[serde(state)] Mixop, #[serde(state)] AtomPhrase, #[serde(state)] Mixop),
    Seq(#[serde(state)] Vec<Mixop>),
}

// = Structural properties

impl MixopKind {
    /// The kind of this form.
    pub(super) fn tag(&self) -> MixopTag {
        match self {
            Self::Arg => MixopTag::Arg,
            Self::Atom(_) => MixopTag::Atom,
            Self::Brack(..) => MixopTag::Brack,
            Self::Infix(..) => MixopTag::Infix,
            Self::Seq(_) => MixopTag::Seq,
        }
    }

    /// Counts positions from the children's recorded counts.
    pub(super) fn arity(&self, arena_mixop: &MixopArena) -> usize {
        match self {
            Self::Arg => 1,
            Self::Atom(_) => 0,
            Self::Brack(_, mixop, _) => arena_mixop.arity(*mixop),
            Self::Infix(mixop_l, _, mixop_r) => {
                arena_mixop.arity(*mixop_l) + arena_mixop.arity(*mixop_r)
            }
            Self::Seq(mixops) => mixops.iter().map(|mixop| arena_mixop.arity(*mixop)).sum(),
        }
    }
}
