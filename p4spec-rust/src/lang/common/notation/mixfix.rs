//! Mixfix forms, literal atoms interleaved with typed argument holes
//!
//! `Mixfix<T>` is the notation form with arguments of type `T`:
//! types for a notation type, expressions for a notation expression,
//! values for a case value, `()` for the bare shape.
//! Comparison, hashing, and equality look at atoms and arguments,
//! never at atom spans;
//! `eq_shape` compares atoms only.

use serde::{Deserialize, Serialize};
use serde_derive_state::{DeserializeState, SerializeState};

use std::{
    cmp::Ordering,
    fmt,
    hash::{Hash, Hasher},
};

use crate::lang::{
    common::ds::set::IdSet,
    traits::{
        at::At,
        cmp::SyntaxCmp,
        eq::SyntaxEq,
        free::FreeIds,
        print::{Print, Printer},
    },
};

use super::{
    super::source::{Phrase, Span},
    atom::Atom,
};

// == Types

/// An atom paired with its source span.
pub type AtomPhrase = Phrase<Atom>;

/// A mixfix expression: literal atoms interleaved with argument holes of `T`.
///
/// For example `_ + _` is infix with two holes and `[ _ ]` brackets one.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum Mixfix<T> {
    /// Argument position.
    Arg(T),
    /// Literal atom.
    Atom(AtomPhrase),
    /// Bracketed expression.
    Brack(AtomPhrase, Box<Self>, AtomPhrase),
    /// Infix expression.
    Infix(Box<Self>, AtomPhrase, Box<Self>),
    /// Sequence of expressions.
    Seq(Vec<Self>),
}

// == Source locations

impl<T: At> At for Mixfix<T> {
    fn at(&self) -> Span {
        // Collect actual occurrences so empty sequences add no default span
        fn collect<T: At>(mixfix: &Mixfix<T>, spans: &mut Vec<Span>) {
            match mixfix {
                Mixfix::Arg(arg) => spans.push(arg.at()),
                Mixfix::Atom(atom) => spans.push(atom.at()),
                Mixfix::Brack(atom_l, mixfix_inner, atom_r) => {
                    spans.push(atom_l.at());
                    collect(mixfix_inner, spans);
                    spans.push(atom_r.at());
                }
                Mixfix::Infix(mixfix_l, atom, mixfix_r) => {
                    collect(mixfix_l, spans);
                    spans.push(atom.at());
                    collect(mixfix_r, spans);
                }
                Mixfix::Seq(mixfixes) => {
                    for mixfix in mixfixes {
                        collect(mixfix, spans);
                    }
                }
            }
        }

        // Cover the complete token set rather than only the argument positions
        let mut spans = Vec::new();
        collect(self, &mut spans);
        spans.at()
    }
}

// == Equality and comparison

impl<T> Mixfix<T> {
    // - Tagging for comparison

    /// Orders the variants for comparison across shapes.
    fn tag(&self) -> u8 {
        match self {
            Self::Arg(_) => 0,
            Self::Atom(_) => 1,
            Self::Brack(_, _, _) => 2,
            Self::Infix(_, _, _) => 3,
            Self::Seq(_) => 4,
        }
    }

    // - Comparison

    /// Compares structure and atoms lexicographically,
    /// using `compare_arg` for arguments.
    pub fn cmp_by<U>(
        &self,
        mixfix_other: &Mixfix<U>,
        mut compare_arg: impl FnMut(&T, &U) -> Ordering,
    ) -> Ordering {
        self.cmp_by_inner(mixfix_other, &mut compare_arg)
    }

    /// Structural comparison, threading the argument comparator.
    fn cmp_by_inner<U>(
        &self,
        mixfix_other: &Mixfix<U>,
        compare_arg: &mut impl FnMut(&T, &U) -> Ordering,
    ) -> Ordering {
        match (self, mixfix_other) {
            (Self::Arg(arg_l), Mixfix::Arg(arg_r)) => compare_arg(arg_l, arg_r),
            (Self::Atom(atom_l), Mixfix::Atom(atom_r)) => atom_l.node.cmp(&atom_r.node),
            (
                Self::Brack(atom_l_l, mixfix_l, atom_l_r),
                Mixfix::Brack(atom_r_l, mixfix_r, atom_r_r),
            ) => atom_l_l
                .node
                .cmp(&atom_r_l.node)
                .then_with(|| mixfix_l.cmp_by_inner(mixfix_r, compare_arg))
                .then_with(|| atom_l_r.node.cmp(&atom_r_r.node)),
            (
                Self::Infix(mixfix_l_l, atom_l, mixfix_l_r),
                Mixfix::Infix(mixfix_r_l, atom_r, mixfix_r_r),
            ) => mixfix_l_l
                .cmp_by_inner(mixfix_r_l, compare_arg)
                .then_with(|| atom_l.node.cmp(&atom_r.node))
                .then_with(|| mixfix_l_r.cmp_by_inner(mixfix_r_r, compare_arg)),
            (Self::Seq(mixfixes_l), Mixfix::Seq(mixfixes_r)) => {
                for (mixfix_l, mixfix_r) in mixfixes_l.iter().zip(mixfixes_r) {
                    let ord = mixfix_l.cmp_by_inner(mixfix_r, compare_arg);
                    if ord != Ordering::Equal {
                        return ord;
                    }
                }
                mixfixes_l.len().cmp(&mixfixes_r.len())
            }
            // Different shapes order by variant
            _ => self.tag().cmp(&mixfix_other.tag()),
        }
    }

    /// Compares structure and atoms, using `eq_arg` for arguments.
    pub fn eq_by<U>(
        &self,
        mixfix_other: &Mixfix<U>,
        mut eq_arg: impl FnMut(&T, &U) -> bool,
    ) -> bool {
        self.eq_by_inner(mixfix_other, &mut eq_arg)
    }

    /// Structural equality, threading the argument predicate.
    fn eq_by_inner<U>(
        &self,
        mixfix_other: &Mixfix<U>,
        eq_arg: &mut impl FnMut(&T, &U) -> bool,
    ) -> bool {
        match (self, mixfix_other) {
            (Self::Arg(arg_l), Mixfix::Arg(arg_r)) => eq_arg(arg_l, arg_r),
            (Self::Atom(atom_l), Mixfix::Atom(atom_r)) => atom_l.node == atom_r.node,
            (
                Self::Brack(atom_l_l, mixfix_l, atom_l_r),
                Mixfix::Brack(atom_r_l, mixfix_r, atom_r_r),
            ) => {
                atom_l_l.node == atom_r_l.node
                    && mixfix_l.eq_by_inner(mixfix_r, eq_arg)
                    && atom_l_r.node == atom_r_r.node
            }
            (
                Self::Infix(mixfix_l_l, atom_l, mixfix_l_r),
                Mixfix::Infix(mixfix_r_l, atom_r, mixfix_r_r),
            ) => {
                mixfix_l_l.eq_by_inner(mixfix_r_l, eq_arg)
                    && atom_l.node == atom_r.node
                    && mixfix_l_r.eq_by_inner(mixfix_r_r, eq_arg)
            }
            (Self::Seq(mixfixes_l), Mixfix::Seq(mixfixes_r)) => {
                mixfixes_l.len() == mixfixes_r.len()
                    && mixfixes_l
                        .iter()
                        .zip(mixfixes_r)
                        .all(|(mixfix_l, mixfix_r)| mixfix_l.eq_by_inner(mixfix_r, eq_arg))
            }
            // Different shapes
            _ => false,
        }
    }

    /// Tests whether two mixfixes have the same atoms and argument positions.
    pub fn eq_shape<U>(&self, mixfix_other: &Mixfix<U>) -> bool {
        self.eq_by(mixfix_other, |_, _| true)
    }
}

impl<T: PartialEq> PartialEq for Mixfix<T> {
    fn eq(&self, mixfix_other: &Self) -> bool {
        self.eq_by(mixfix_other, PartialEq::eq)
    }
}

impl<T: Eq> Eq for Mixfix<T> {}

impl<T: SyntaxEq> SyntaxEq for Mixfix<T> {
    fn syntax_eq(&self, other: &Self) -> bool {
        self.eq_by(other, SyntaxEq::syntax_eq)
    }
}

impl<T: SyntaxCmp> SyntaxCmp for Mixfix<T> {
    fn syntax_cmp(&self, other: &Self) -> Ordering {
        self.cmp_by(other, SyntaxCmp::syntax_cmp)
    }
}

// == Ordering

impl<T: Ord> Ord for Mixfix<T> {
    fn cmp(&self, mixfix_other: &Self) -> Ordering {
        self.cmp_by(mixfix_other, Ord::cmp)
    }
}

impl<T: Ord> PartialOrd for Mixfix<T> {
    fn partial_cmp(&self, mixfix_other: &Self) -> Option<Ordering> {
        Some(self.cmp(mixfix_other))
    }
}

// == Hashing

impl<T: Hash> Hash for Mixfix<T> {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        // Hash the shape first so different variants rarely collide
        self.tag().hash(hasher);
        match self {
            Self::Arg(arg) => arg.hash(hasher),
            Self::Atom(atom) => atom.node.hash(hasher),
            Self::Brack(atom_l, mixfix, atom_r) => {
                atom_l.node.hash(hasher);
                mixfix.hash(hasher);
                atom_r.node.hash(hasher);
            }
            Self::Infix(mixfix_l, atom, mixfix_r) => {
                mixfix_l.hash(hasher);
                atom.node.hash(hasher);
                mixfix_r.hash(hasher);
            }
            Self::Seq(mixfixes) => mixfixes.hash(hasher),
        }
    }
}

// == Free identifiers

impl<T: FreeIds> FreeIds for Mixfix<T> {
    fn free_ids_into(&self, free: &mut IdSet) {
        match self {
            Self::Arg(arg) => arg.free_ids_into(free),
            Self::Atom(_) => {}
            Self::Brack(_, mixfix, _) => mixfix.free_ids_into(free),
            Self::Infix(mixfix_l, _, mixfix_r) => {
                mixfix_l.free_ids_into(free);
                mixfix_r.free_ids_into(free);
            }
            Self::Seq(mixfixes) => mixfixes.as_slice().free_ids_into(free),
        }
    }
}

// == Fold, map, and iter

impl<T> Mixfix<T> {
    /// Folds arguments from left to right.
    pub fn fold<A>(&self, acc: A, mut fold_arg: impl FnMut(A, &T) -> A) -> A {
        self.fold_inner(acc, &mut fold_arg)
    }

    /// Folds this subtree's arguments left to right.
    fn fold_inner<A>(&self, acc: A, fold_arg: &mut impl FnMut(A, &T) -> A) -> A {
        match self {
            Self::Arg(arg) => fold_arg(acc, arg),
            Self::Atom(_) => acc,
            Self::Brack(_, mixfix, _) => mixfix.fold_inner(acc, fold_arg),
            Self::Infix(mixfix_l, _, mixfix_r) => {
                let acc = mixfix_l.fold_inner(acc, fold_arg);
                mixfix_r.fold_inner(acc, fold_arg)
            }
            Self::Seq(mixfixes) => mixfixes
                .iter()
                .fold(acc, |acc, mixfix| mixfix.fold_inner(acc, fold_arg)),
        }
    }

    /// Maps arguments while preserving mixfix structure and atoms.
    pub fn map<U>(&self, mut map_arg: impl FnMut(&T) -> U) -> Mixfix<U> {
        self.map_inner(&mut map_arg)
    }

    /// Maps this subtree's arguments, cloning atoms.
    fn map_inner<U>(&self, map_arg: &mut impl FnMut(&T) -> U) -> Mixfix<U> {
        match self {
            Self::Arg(arg) => Mixfix::Arg(map_arg(arg)),
            Self::Atom(atom) => Mixfix::Atom(atom.clone()),
            Self::Brack(atom_l, mixfix, atom_r) => {
                Mixfix::Brack(atom_l.clone(), Box::new(mixfix.map_inner(map_arg)), atom_r.clone())
            }
            Self::Infix(mixfix_l, atom, mixfix_r) => Mixfix::Infix(
                Box::new(mixfix_l.map_inner(map_arg)),
                atom.clone(),
                Box::new(mixfix_r.map_inner(map_arg)),
            ),
            Self::Seq(mixfixes) => Mixfix::Seq(
                mixfixes
                    .iter()
                    .map(|mixfix| mixfix.map_inner(map_arg))
                    .collect(),
            ),
        }
    }

    /// Maps arguments in order, stopping at the first error and retaining atoms.
    pub fn try_map<U, E>(
        &self,
        mut map_arg: impl FnMut(&T) -> Result<U, E>,
    ) -> Result<Mixfix<U>, E> {
        self.try_map_inner(&mut map_arg)
    }

    /// Maps this subtree with the same callback and early error propagation.
    fn try_map_inner<U, E>(
        &self,
        map_arg: &mut impl FnMut(&T) -> Result<U, E>,
    ) -> Result<Mixfix<U>, E> {
        Ok(match self {
            Self::Arg(arg) => Mixfix::Arg(map_arg(arg)?),
            Self::Atom(atom) => Mixfix::Atom(atom.clone()),
            Self::Brack(atom_l, mixfix, atom_r) => Mixfix::Brack(
                atom_l.clone(),
                Box::new(mixfix.try_map_inner(map_arg)?),
                atom_r.clone(),
            ),
            Self::Infix(mixfix_l, atom, mixfix_r) => Mixfix::Infix(
                Box::new(mixfix_l.try_map_inner(map_arg)?),
                atom.clone(),
                Box::new(mixfix_r.try_map_inner(map_arg)?),
            ),
            Self::Seq(mixfixes) => Mixfix::Seq(
                mixfixes
                    .iter()
                    .map(|mixfix| mixfix.try_map_inner(map_arg))
                    .collect::<Result<_, _>>()?,
            ),
        })
    }

    /// Visits arguments from left to right.
    pub fn iter(&self, mut visit_arg: impl FnMut(&T)) {
        self.fold((), |(), arg| visit_arg(arg));
    }
}

// == Utilities using fold, map, and iter

impl<T> Mixfix<T> {
    // - Arity

    /// Returns the number of argument positions.
    pub fn arity(&self) -> usize {
        self.fold(0, |arity, _| arity + 1)
    }

    // - Atoms and args

    /// Collects arguments in left-to-right tree order.
    pub fn args(&self) -> Vec<&T> {
        let mut args = Vec::with_capacity(self.arity());
        self.collect_args(&mut args);
        args
    }

    /// Appends this subtree's arguments in tree order.
    fn collect_args<'a>(&'a self, args: &mut Vec<&'a T>) {
        match self {
            Self::Arg(arg) => args.push(arg),
            Self::Atom(_) => {}
            Self::Brack(_, mixfix, _) => mixfix.collect_args(args),
            Self::Infix(mixfix_l, _, mixfix_r) => {
                mixfix_l.collect_args(args);
                mixfix_r.collect_args(args);
            }
            Self::Seq(mixfixes) => {
                for mixfix in mixfixes {
                    mixfix.collect_args(args);
                }
            }
        }
    }

    /// Collects owned arguments in left-to-right tree order.
    pub fn into_args(self) -> Vec<T> {
        let mut args = Vec::with_capacity(self.arity());
        self.collect_into_args(&mut args);
        args
    }

    /// Moves this subtree's arguments out in tree order.
    fn collect_into_args(self, args: &mut Vec<T>) {
        match self {
            Self::Arg(arg) => args.push(arg),
            Self::Atom(_) => {}
            Self::Brack(_, mixfix, _) => mixfix.collect_into_args(args),
            Self::Infix(mixfix_l, _, mixfix_r) => {
                mixfix_l.collect_into_args(args);
                mixfix_r.collect_into_args(args);
            }
            Self::Seq(mixfixes) => {
                for mixfix in mixfixes {
                    mixfix.collect_into_args(args);
                }
            }
        }
    }
}

// == Printing

impl<T> Mixfix<T> {
    /// Writes atoms and arguments, separating non-empty pieces with spaces.
    pub fn print_with(
        &self,
        printer: &mut Printer<'_>,
        mut print_arg: impl FnMut(&T, &mut Printer<'_>) -> fmt::Result,
    ) -> fmt::Result {
        let mut is_first = true;
        self.print_with_inner(printer, &mut print_arg, &mut is_first)
    }

    /// Prints this subtree, tracking whether a separator is due.
    fn print_with_inner(
        &self,
        printer: &mut Printer<'_>,
        print_arg: &mut impl FnMut(&T, &mut Printer<'_>) -> fmt::Result,
        is_first: &mut bool,
    ) -> fmt::Result {
        // A space before every piece but the first
        let print_sep = |printer: &mut Printer<'_>, is_first: &mut bool| {
            if *is_first {
                *is_first = false;
                Ok(())
            } else {
                printer.write(" ")
            }
        };

        // Empty keyword atoms print nothing, not even a space
        let print_atom = |atom: &AtomPhrase, printer: &mut Printer<'_>, is_first: &mut bool| {
            if matches!(&atom.node, Atom::Keyword(keyword) if keyword.is_empty()) {
                Ok(())
            } else {
                print_sep(printer, is_first)?;
                atom.print(printer)
            }
        };

        match self {
            Self::Arg(arg) => {
                print_sep(printer, is_first)?;
                print_arg(arg, printer)
            }
            Self::Atom(atom) => print_atom(atom, printer, is_first),
            Self::Brack(atom_l, mixfix, atom_r) => {
                print_atom(atom_l, printer, is_first)?;
                mixfix.print_with_inner(printer, print_arg, is_first)?;
                print_atom(atom_r, printer, is_first)
            }
            Self::Infix(mixfix_l, atom, mixfix_r) => {
                mixfix_l.print_with_inner(printer, print_arg, is_first)?;
                print_atom(atom, printer, is_first)?;
                mixfix_r.print_with_inner(printer, print_arg, is_first)
            }
            Self::Seq(mixfixes) => {
                for mixfix in mixfixes {
                    mixfix.print_with_inner(printer, print_arg, is_first)?;
                }
                Ok(())
            }
        }
    }
}

// == Serialization

// - Encode

// Recursive mixfix boxes are not separated by phrase nodes,
// so grow the stack here rather than relying on `NotePhrase`
impl<T, State> serde_state::SerializeState<State> for Mixfix<T>
where
    T: serde_state::SerializeState<State>,
{
    fn serialize_state<Serializer>(
        &self,
        serializer: Serializer,
        state: &State,
    ) -> Result<Serializer::Ok, Serializer::Error>
    where
        Serializer: serde::Serializer,
    {
        #[derive(SerializeState)]
        #[serde(rename = "Mixfix")]
        #[serde(serialize_state = "State", ser_parameters = "State")]
        #[serde(bound(serialize = "T: serde_state::SerializeState<State>"))]
        enum MixfixState<'a, T> {
            Arg(#[serde(state)] &'a T),
            Atom(#[serde(state)] &'a AtomPhrase),
            Brack(
                #[serde(state)] &'a AtomPhrase,
                #[serde(state)] &'a Mixfix<T>,
                #[serde(state)] &'a AtomPhrase,
            ),
            Infix(
                #[serde(state)] &'a Mixfix<T>,
                #[serde(state)] &'a AtomPhrase,
                #[serde(state)] &'a Mixfix<T>,
            ),
            Seq(#[serde(state)] &'a [Mixfix<T>]),
        }

        stacker::maybe_grow(64 * 1024, 1024 * 1024, || {
            let mixfix = match self {
                Self::Arg(arg) => MixfixState::Arg(arg),
                Self::Atom(atom) => MixfixState::Atom(atom),
                Self::Brack(atom_l, mixfix, atom_r) => MixfixState::Brack(atom_l, mixfix, atom_r),
                Self::Infix(mixfix_l, atom, mixfix_r) => {
                    MixfixState::Infix(mixfix_l, atom, mixfix_r)
                }
                Self::Seq(mixfixes) => MixfixState::Seq(mixfixes),
            };
            mixfix.serialize_state(serializer, state)
        })
    }
}

// - Decode

impl<'de, T, State> serde_state::DeserializeState<'de, State> for Mixfix<T>
where
    T: serde_state::DeserializeState<'de, State>,
{
    fn deserialize_state<Deserializer>(
        state: &mut State,
        deserializer: Deserializer,
    ) -> Result<Self, Deserializer::Error>
    where
        Deserializer: serde::Deserializer<'de>,
    {
        // Recursive children use Mixfix so only this level needs conversion
        #[derive(DeserializeState)]
        #[serde(rename = "Mixfix")]
        #[serde(deserialize_state = "State", de_parameters = "State")]
        #[serde(bound(deserialize = "T: serde_state::DeserializeState<'de, State>"))]
        enum MixfixState<T> {
            Arg(#[serde(state)] T),
            Atom(#[serde(state)] AtomPhrase),
            Brack(
                #[serde(state)] AtomPhrase,
                #[serde(state)] Box<Mixfix<T>>,
                #[serde(state)] AtomPhrase,
            ),
            Infix(
                #[serde(state)] Box<Mixfix<T>>,
                #[serde(state)] AtomPhrase,
                #[serde(state)] Box<Mixfix<T>>,
            ),
            Seq(#[serde(state)] Vec<Mixfix<T>>),
        }

        Ok(match MixfixState::deserialize_state(state, deserializer)? {
            MixfixState::Arg(arg) => Self::Arg(arg),
            MixfixState::Atom(atom) => Self::Atom(atom),
            MixfixState::Brack(atom_l, mixfix, atom_r) => Self::Brack(atom_l, mixfix, atom_r),
            MixfixState::Infix(mixfix_l, atom, mixfix_r) => Self::Infix(mixfix_l, atom, mixfix_r),
            MixfixState::Seq(mixfixes) => Self::Seq(mixfixes),
        })
    }
}
