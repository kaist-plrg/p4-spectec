//! Constructors for tree mixfix forms
//!
//! `new` checks the argument count; `fill` supplies each position.

use std::{cmp::Ordering, rc::Rc};

use super::super::{ArityMismatch, AtomPhrase};
use super::{Mixfix, Mixop};

// - General

impl<T> Mixfix<T> {
    /// Pairs a mixop with its arguments.
    ///
    /// Fails unless there is exactly one argument per position.
    pub fn new(mixop: Rc<Mixop>, args: Vec<T>) -> Result<Self, ArityMismatch> {
        let arity = mixop.arity();
        match args.len().cmp(&arity) {
            Ordering::Less => Err(ArityMismatch::ArgumentCountTooFew),
            Ordering::Greater => Err(ArityMismatch::ArgumentCountTooMany),
            Ordering::Equal => Ok(Self { mixop, args }),
        }
    }

    /// Fills each position of a mixop, calling `fill_arg` once per position.
    pub fn fill(mixop: Rc<Mixop>, fill_arg: impl FnMut(usize) -> T) -> Self {
        let arity = mixop.arity();
        let args = (0..arity).map(fill_arg).collect();
        Self { mixop, args }
    }
}

// - Parts

/// A lone argument.
pub fn arg<T>(arg: T) -> Mixfix<T> {
    Mixfix { mixop: Rc::new(Mixop::Arg), args: vec![arg] }
}

/// A lone atom.
pub fn atom<T>(atom: AtomPhrase) -> Mixfix<T> {
    Mixfix { mixop: Rc::new(Mixop::Atom(atom)), args: Vec::new() }
}

/// A form between two atoms.
pub fn brack<T>(atom_l: AtomPhrase, mixfix_inner: Mixfix<T>, atom_r: AtomPhrase) -> Mixfix<T> {
    let mixop_inner = Rc::unwrap_or_clone(mixfix_inner.mixop);
    let mixop = Mixop::Brack(atom_l, Box::new(mixop_inner), atom_r);
    Mixfix { mixop: Rc::new(mixop), args: mixfix_inner.args }
}

/// Two forms around an atom.
pub fn infix<T>(mixfix_l: Mixfix<T>, atom: AtomPhrase, mixfix_r: Mixfix<T>) -> Mixfix<T> {
    let mixop_l = Rc::unwrap_or_clone(mixfix_l.mixop);
    let mixop_r = Rc::unwrap_or_clone(mixfix_r.mixop);
    let mixop = Mixop::Infix(Box::new(mixop_l), atom, Box::new(mixop_r));
    let mut args = mixfix_l.args;
    args.extend(mixfix_r.args);
    Mixfix { mixop: Rc::new(mixop), args }
}

/// Forms in sequence.
pub fn seq<T>(mixfixes: Vec<Mixfix<T>>) -> Mixfix<T> {
    let mut mixops = Vec::with_capacity(mixfixes.len());
    let mut args = Vec::new();
    for mixfix in mixfixes {
        mixops.push(Rc::unwrap_or_clone(mixfix.mixop));
        args.extend(mixfix.args);
    }
    Mixfix { mixop: Rc::new(Mixop::Seq(mixops)), args }
}
