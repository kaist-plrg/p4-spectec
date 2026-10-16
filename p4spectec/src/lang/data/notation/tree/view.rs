//! Borrowed views of mixfix forms for structural inspection
//!
//! `MixfixRef` pairs a subtree with the arguments of its positions.
//! `MixfixView` exposes one level with those arguments in place.

use super::super::AtomPhrase;
use super::{Mixfix, Mixop};

// = Borrowed views

/// A borrowed mixfix: a tree mixop and the arguments of its positions.
#[derive(Debug)]
pub struct MixfixRef<'a, T> {
    mixop: &'a Mixop,
    args: &'a [T],
}

/// One level of a mixfix tree, with arguments in place.
#[derive(Debug)]
pub enum MixfixView<'a, T> {
    /// An argument.
    Arg(&'a T),
    /// A literal atom.
    Atom(&'a AtomPhrase),
    /// A form between two atoms.
    Brack(&'a AtomPhrase, MixfixRef<'a, T>, &'a AtomPhrase),
    /// Two forms around an atom.
    Infix(MixfixRef<'a, T>, &'a AtomPhrase, MixfixRef<'a, T>),
    /// Forms in sequence.
    Seq(Vec<MixfixRef<'a, T>>),
}

impl<T> Clone for MixfixRef<'_, T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Copy for MixfixRef<'_, T> {}

// = Construction

impl<T> Mixfix<T> {
    /// Borrows the mixfix, for viewing its parts.
    pub fn as_ref(&self) -> MixfixRef<'_, T> {
        MixfixRef { mixop: self.mixop.as_ref(), args: &self.args }
    }
}

// = Inspection

impl<'a, T> MixfixRef<'a, T> {
    /// The top level of the tree, splitting arguments among the children.
    pub fn view(&self) -> MixfixView<'a, T> {
        // Each child takes as many arguments as it has positions
        let mut args = self.args;
        let mut take = |mixop: &'a Mixop| {
            let (args_child, args_rest) = args.split_at(mixop.arity());
            args = args_rest;
            MixfixRef { mixop, args: args_child }
        };
        match self.mixop {
            Mixop::Arg => MixfixView::Arg(&self.args[0]),
            Mixop::Atom(atom) => MixfixView::Atom(atom),
            Mixop::Brack(atom_l, mixop_inner, atom_r) => {
                let mixfix_inner = take(mixop_inner);
                MixfixView::Brack(atom_l, mixfix_inner, atom_r)
            }
            Mixop::Infix(mixop_l, atom, mixop_r) => {
                let mixfix_l = take(mixop_l);
                let mixfix_r = take(mixop_r);
                MixfixView::Infix(mixfix_l, atom, mixfix_r)
            }
            Mixop::Seq(mixops) => {
                let mixfixes = mixops.iter().map(take).collect();
                MixfixView::Seq(mixfixes)
            }
        }
    }
}
