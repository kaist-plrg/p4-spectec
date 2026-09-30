//! Prepare source syntax for slot-based execution
//!
//! `Prepare` walks IL syntax once
//! and replaces identifiers and variables by frame slots (`IdSlot`, `VarSlot`),
//! so evaluation indexes a frame instead of looking names up;
//! containers, phrases, and notation recurse structurally.

pub mod ast;

use crate::lang::{
    common::{Id, notation::mixfix::Mixfix, source::NotePhrase},
    data::var::{IdSlot, Var, VarSlot},
};

use crate::runtime::envs::interp::shared::frame::FrameLayout;

/// Slot resolution of one syntax node.
pub trait Prepare: Sized {
    /// The same node with identifiers resolved to slots.
    type Output;

    /// Resolves the node's identifiers in `layout`.
    fn prepare(self, layout: &mut FrameLayout) -> Self::Output;
}

impl Prepare for Id {
    type Output = IdSlot;

    fn prepare(self, layout: &mut FrameLayout) -> Self::Output {
        layout.resolve_id(self)
    }
}

impl Prepare for Var {
    type Output = VarSlot;

    fn prepare(self, layout: &mut FrameLayout) -> Self::Output {
        layout.resolve_var(self)
    }
}

// - Containers

impl<T: Prepare> Prepare for Vec<T> {
    type Output = Vec<T::Output>;

    fn prepare(self, layout: &mut FrameLayout) -> Self::Output {
        self.into_iter()
            .map(|syntax| syntax.prepare(layout))
            .collect()
    }
}

impl<T: Prepare> Prepare for Box<T> {
    type Output = Box<T::Output>;

    fn prepare(self, layout: &mut FrameLayout) -> Self::Output {
        Box::new((*self).prepare(layout))
    }
}

impl<T: Prepare> Prepare for Option<T> {
    type Output = Option<T::Output>;

    fn prepare(self, layout: &mut FrameLayout) -> Self::Output {
        self.map(|syntax| syntax.prepare(layout))
    }
}

// - Phrases

impl<T: Prepare, N, S> Prepare for NotePhrase<T, N, S> {
    type Output = NotePhrase<T::Output, N, S>;

    fn prepare(self, layout: &mut FrameLayout) -> Self::Output {
        // Grow the stack for deep syntax trees
        stacker::maybe_grow(64 * 1024, 1024 * 1024, || NotePhrase {
            node: self.node.prepare(layout),
            note: self.note,
            span: self.span,
        })
    }
}

// - Notation

impl<T: Prepare> Prepare for Mixfix<T> {
    type Output = Mixfix<T::Output>;

    fn prepare(self, layout: &mut FrameLayout) -> Self::Output {
        match self {
            Mixfix::Arg(arg_inner) => Mixfix::Arg(arg_inner.prepare(layout)),
            Mixfix::Atom(atom_inner) => Mixfix::Atom(atom_inner),
            Mixfix::Brack(atom_l, mixfix_inner, atom_r) => {
                Mixfix::Brack(atom_l, mixfix_inner.prepare(layout), atom_r)
            }
            Mixfix::Infix(mixfix_l, atom_inner, mixfix_r) => {
                Mixfix::Infix(mixfix_l.prepare(layout), atom_inner, mixfix_r.prepare(layout))
            }
            Mixfix::Seq(mixfixes) => Mixfix::Seq(mixfixes.prepare(layout)),
        }
    }
}
