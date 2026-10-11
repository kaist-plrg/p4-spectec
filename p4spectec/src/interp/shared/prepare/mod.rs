//! Prepare source syntax for execution
//!
//! Each language supplies its preparation context.
//! Containers and phrases preserve metadata while recursively preparing nodes.

pub(crate) mod il;
pub(crate) mod mixop;

use crate::lang::common::source::NotePhrase;

/// Preparation of one syntax node using a domain-specific context.
pub trait Prepare<C>: Sized {
    /// The executable representation of the node.
    type Output;

    /// Prepares the node through `ctx`.
    fn prepare(self, ctx: &mut C) -> Self::Output;
}

// - Containers

impl<C, T: Prepare<C>> Prepare<C> for Vec<T> {
    type Output = Vec<T::Output>;

    fn prepare(self, ctx: &mut C) -> Self::Output {
        self.into_iter().map(|syntax| syntax.prepare(ctx)).collect()
    }
}

impl<C, T: Prepare<C>> Prepare<C> for Box<T> {
    type Output = Box<T::Output>;

    fn prepare(self, ctx: &mut C) -> Self::Output {
        Box::new((*self).prepare(ctx))
    }
}

impl<C, T: Prepare<C>> Prepare<C> for Option<T> {
    type Output = Option<T::Output>;

    fn prepare(self, ctx: &mut C) -> Self::Output {
        self.map(|syntax| syntax.prepare(ctx))
    }
}

// - Phrases

impl<C, T: Prepare<C>, N, S> Prepare<C> for NotePhrase<T, N, S> {
    type Output = NotePhrase<T::Output, N, S>;

    fn prepare(self, ctx: &mut C) -> Self::Output {
        // Grow the stack for deep syntax trees
        stacker::maybe_grow(64 * 1024, 1024 * 1024, || NotePhrase {
            node: self.node.prepare(ctx),
            note: self.note,
            span: self.span,
        })
    }
}
