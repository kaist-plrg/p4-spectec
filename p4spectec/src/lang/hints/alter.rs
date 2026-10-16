//! Alteration hints for prose rendering
//!
//! `hint(prose "the type of" % "is" %)` is a template:
//! text and atoms print as they are, `%` holes take the items being described.
//! `alternate` renders one through a `Renderer` for the output format.

use thiserror::Error;

use crate::lang::{common::source::Phrase, hints::input::InputHint};

use crate::lang::el::ast::{Atom, Exp, ExpKind, Hole as ElHole, Text};

// == Alteration hints

/// A prose rendering template at its original source location.
pub type AlterHint = Phrase<AlterHintKind>;

/// The syntax of a prose rendering template.
///
/// `Hole::Next` consumes items in cursor order;
/// `Hole::Num` selects an explicit item index.
#[derive(Clone, Debug, PartialEq)]
pub enum AlterHintKind {
    /// Literal text.
    Text(Text),
    /// A notation atom.
    Atom(Atom),
    /// Pieces in order.
    Seq(Vec<AlterHint>),
    /// A piece between bracket atoms.
    Brack(Atom, Box<AlterHint>, Atom),
    /// An item placeholder at its original source location.
    Hole(Hole),
    /// Two pieces joined without a separator.
    Fuse(Box<AlterHint>, Box<AlterHint>),
    /// Any other expression, rendered by the caller.
    Other(Exp),
}

/// A positional hole in an alteration hint.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Hole {
    /// `%`, the next item in cursor order.
    Next,
    /// `%N`, the N-th item.
    Num(usize),
}

/// A failure rendering an alteration hint.
#[derive(Clone, Debug, Error, PartialEq, Eq)]
pub enum AlterationError {
    /// A hole asked for an item that does not exist.
    #[error("alteration hint index {index} is out of bounds for {item_count} items")]
    IndexOutOfBounds { hole: Box<Phrase<Hole>>, index: usize, item_count: usize },
}

// Creating hints

/// Reads a template from a hint expression; unknown forms become `Other`.
pub fn init(exp: &Exp) -> AlterHint {
    let hint_kind = match &exp.node {
        // Text, atoms, sequences, brackets, holes, and fuses map directly
        ExpKind::Text(text) => AlterHintKind::Text(text.clone()),
        ExpKind::Atom(atom) => AlterHintKind::Atom(atom.clone()),
        ExpKind::Seq(exps) => AlterHintKind::Seq(exps.iter().map(init).collect()),
        ExpKind::Brack(atom_l, exp, atom_r) => {
            AlterHintKind::Brack(atom_l.clone(), Box::new(init(exp)), atom_r.clone())
        }
        // `%` and `%N`; `%%` and `!%` have no template meaning
        ExpKind::Hole(ElHole::Next) => AlterHintKind::Hole(Hole::Next),
        ExpKind::Hole(ElHole::Num(index)) => AlterHintKind::Hole(Hole::Num(*index)),
        ExpKind::Fuse(exp_l, _, exp_r) => {
            AlterHintKind::Fuse(Box::new(init(exp_l)), Box::new(init(exp_r)))
        }
        // Anything else is kept as an expression for the renderer
        _ => AlterHintKind::Other(exp.clone()),
    };
    crate::phrase! { node: hint_kind, span: exp.span }
}

// == Validation

/// Validates every hole against an item count.
pub fn validate(hint: &AlterHint, item_count: usize) -> Result<(), AlterationError> {
    /// Walks the template, advancing the cursor at `%` and checking `%N`.
    fn validate_at(
        hint: &AlterHint,
        item_count: usize,
        cursor: usize,
    ) -> Result<usize, AlterationError> {
        match &hint.node {
            AlterHintKind::Text(_) | AlterHintKind::Atom(_) | AlterHintKind::Other(_) => Ok(cursor),
            AlterHintKind::Seq(hints) => hints
                .iter()
                .try_fold(cursor, |cursor, hint| validate_at(hint, item_count, cursor)),
            AlterHintKind::Brack(_, hint, _) => validate_at(hint, item_count, cursor),
            // `%` takes the item at the cursor
            AlterHintKind::Hole(Hole::Next) if cursor < item_count => Ok(cursor + 1),
            AlterHintKind::Hole(hole @ Hole::Next) => Err(AlterationError::IndexOutOfBounds {
                hole: Box::new(crate::phrase! { node: hole.clone(), span: hint.span }),
                index: cursor,
                item_count,
            }),
            // `%N` leaves the cursor alone
            AlterHintKind::Hole(Hole::Num(idx)) if *idx < item_count => Ok(cursor),
            AlterHintKind::Hole(hole @ Hole::Num(idx)) => Err(AlterationError::IndexOutOfBounds {
                hole: Box::new(crate::phrase! { node: hole.clone(), span: hint.span }),
                index: *idx,
                item_count,
            }),
            // The right piece continues the left's cursor
            AlterHintKind::Fuse(hint_l, hint_r) => {
                validate_at(hint_r, item_count, validate_at(hint_l, item_count, cursor)?)
            }
        }
    }

    validate_at(hint, item_count, 0).map(|_| ())
}

// == Index realignment

/// Renumbers output holes after relation inputs, preserving source locations.
pub fn realign(hint: &AlterHint, hint_input: &InputHint) -> AlterHint {
    /// Gathers every `%N` index in the template.
    fn collect(hint: &AlterHint, indices_output: &mut Vec<usize>) {
        match &hint.node {
            AlterHintKind::Seq(hints) => {
                for hint in hints {
                    collect(hint, indices_output);
                }
            }
            AlterHintKind::Brack(_, hint, _) => collect(hint, indices_output),
            AlterHintKind::Hole(Hole::Num(idx)) => indices_output.push(*idx),
            AlterHintKind::Fuse(hint_l, hint_r) => {
                collect(hint_l, indices_output);
                collect(hint_r, indices_output);
            }
            _ => {}
        }
    }

    /// Rewrites every `%N` by the pairs.
    fn apply(hint: &AlterHint, idx_pairs: &[(usize, usize)]) -> AlterHint {
        let hint_kind = match &hint.node {
            AlterHintKind::Seq(hints) => {
                AlterHintKind::Seq(hints.iter().map(|hint| apply(hint, idx_pairs)).collect())
            }
            AlterHintKind::Brack(atom_l, hint, atom_r) => AlterHintKind::Brack(
                atom_l.clone(),
                Box::new(apply(hint, idx_pairs)),
                atom_r.clone(),
            ),
            AlterHintKind::Hole(Hole::Num(idx)) => {
                let idx_realigned = idx_pairs
                    .iter()
                    .find_map(|(idx_source, idx_realigned)| {
                        (idx_source == idx).then_some(*idx_realigned)
                    })
                    .expect("every numbered hole is collected before realignment");
                AlterHintKind::Hole(Hole::Num(idx_realigned))
            }
            AlterHintKind::Fuse(hint_l, hint_r) => AlterHintKind::Fuse(
                Box::new(apply(hint_l, idx_pairs)),
                Box::new(apply(hint_r, idx_pairs)),
            ),
            _ => hint.node.clone(),
        };
        crate::phrase! { node: hint_kind, span: hint.span }
    }

    let mut indices_output = Vec::new();
    collect(hint, &mut indices_output);
    // Output holes are renumbered by their order among the output positions
    let mut indices_all = hint_input
        .indices()
        .iter()
        .map(|idx| idx.node)
        .collect::<Vec<_>>();
    indices_all.extend(&indices_output);
    indices_all.sort_unstable();
    let mut idx_pairs = Vec::new();
    for idx in indices_all {
        if indices_output.contains(&idx) {
            idx_pairs.push((idx, idx_pairs.len()));
        }
    }
    apply(hint, &idx_pairs)
}

// == Rendering

/// Renders alteration pieces into a caller-defined output.
pub trait Renderer<Item> {
    /// The rendered form.
    type Output: Clone;
    /// Nothing.
    fn empty(&self) -> Self::Output;
    /// Literal text, or nothing if the text renders empty.
    fn text(&self, text: &str) -> Option<Self::Output>;
    /// A notation atom.
    fn atom(&self, atom: &Atom) -> Self::Output;
    /// Pieces in sequence.
    fn join(&self, items: Vec<Self::Output>) -> Self::Output;
    /// Two pieces without a separator.
    fn fuse(&self, output_l: Self::Output, output_r: Self::Output) -> Self::Output;
    /// An expression the template did not understand.
    fn other(&self, exp: &Exp) -> Self::Output;
    /// One of the items being described.
    fn item(&self, item: &Item) -> Self::Output;
}

/// Renders an alteration hint
///
/// Returns an error when a hole cannot select an item.
pub fn alternate<Item, R: Renderer<Item>>(
    hint: &AlterHint,
    items: &[Item],
    renderer: &R,
) -> Result<R::Output, AlterationError> {
    /// Renders a piece, returning the advanced cursor and the output if any.
    fn go<Item, R: Renderer<Item>>(
        hint: &AlterHint,
        items: &[Item],
        cursor: usize,
        renderer: &R,
    ) -> Result<(usize, Option<R::Output>), AlterationError> {
        Ok(match &hint.node {
            AlterHintKind::Text(text) => (cursor, renderer.text(text)),
            AlterHintKind::Atom(atom) => (cursor, Some(renderer.atom(atom))),
            // Pieces share one cursor, so `%` holes take successive items
            AlterHintKind::Seq(hints) => {
                let mut cursor_next = cursor;
                let mut outputs = Vec::new();
                for hint in hints {
                    let (cursor_after, output) = go(hint, items, cursor_next, renderer)?;
                    cursor_next = cursor_after;
                    outputs.push(output.unwrap_or_else(|| renderer.empty()));
                }
                (cursor_next, Some(renderer.join(outputs)))
            }
            // Brackets surround the inner piece, dropped if it rendered nothing
            AlterHintKind::Brack(atom_l, hint, atom_r) => {
                let (cursor_next, output) = go(hint, items, cursor, renderer)?;
                let mut outputs = vec![renderer.atom(atom_l)];
                if let Some(output) = output {
                    outputs.push(output);
                }
                outputs.push(renderer.atom(atom_r));
                (cursor_next, Some(renderer.join(outputs)))
            }
            // The next item, advancing the cursor
            AlterHintKind::Hole(hole @ Hole::Next) => {
                let item = items
                    .get(cursor)
                    .ok_or_else(|| AlterationError::IndexOutOfBounds {
                        hole: Box::new(crate::phrase! { node: hole.clone(), span: hint.span }),
                        index: cursor,
                        item_count: items.len(),
                    })?;
                (cursor + 1, Some(renderer.item(item)))
            }
            // A specific item, leaving the cursor alone
            AlterHintKind::Hole(hole @ Hole::Num(index)) => {
                let item = items
                    .get(*index)
                    .ok_or_else(|| AlterationError::IndexOutOfBounds {
                        hole: Box::new(crate::phrase! { node: hole.clone(), span: hint.span }),
                        index: *index,
                        item_count: items.len(),
                    })?;
                (cursor, Some(renderer.item(item)))
            }
            // Both sides render, the right one continuing the left's cursor
            AlterHintKind::Fuse(hint_l, hint_r) => {
                let (cursor_mid, output_l) = go(hint_l, items, cursor, renderer)?;
                let (cursor_next, output_r) = go(hint_r, items, cursor_mid, renderer)?;
                (
                    cursor_next,
                    Some(renderer.fuse(
                        output_l.unwrap_or_else(|| renderer.empty()),
                        output_r.unwrap_or_else(|| renderer.empty()),
                    )),
                )
            }
            AlterHintKind::Other(exp) => (cursor, Some(renderer.other(exp))),
        })
    }
    Ok(go(hint, items, 0, renderer)?
        .1
        .unwrap_or_else(|| renderer.empty()))
}
