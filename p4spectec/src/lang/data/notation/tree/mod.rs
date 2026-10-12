//! Tree representation of notation
//!
//! `Mixop` owns atoms and child forms; `Mixfix` pairs a shared mixop
//! with one argument per position.
//! Comparison, traversal, conversion, and printing have separate modules.

use std::rc::Rc;

use serde::{Deserialize, Serialize};

use super::{AtomPhrase, MixopTag};

mod at;
mod cmp;
mod convert;
mod hash;
pub mod make;
pub mod parse;
mod print;
mod view;
mod visit;

pub use view::{MixfixRef, MixfixView};

// = Notation forms

/// An owned notation with an argument hole at each position.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum Mixop {
    Arg,
    Atom(AtomPhrase),
    Brack(AtomPhrase, Box<Mixop>, AtomPhrase),
    Infix(Box<Mixop>, AtomPhrase, Box<Mixop>),
    Seq(Vec<Mixop>),
}

/// A shared tree notation with one argument per position.
pub type Mixfix<T> = super::Mixfix<Rc<Mixop>, T>;

// = Structural properties

impl Mixop {
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

    /// Counts argument positions in notation order.
    pub fn arity(&self) -> usize {
        match self {
            Self::Arg => 1,
            Self::Atom(_) => 0,
            Self::Brack(_, mixop, _) => mixop.arity(),
            Self::Infix(mixop_l, _, mixop_r) => mixop_l.arity() + mixop_r.arity(),
            Self::Seq(mixops) => mixops.iter().map(Self::arity).sum(),
        }
    }
}
