//! Prose-language node annotations
//!
//! `Annotated<N>` wraps a node with the `Hints` its `prose*` hints attach,
//! so the prose backend can alter or replace the rendered text of that node.
//! Equality, free identifiers, and call detection see through the wrapper.
//! Nodes share their hints, and nodes without hints share one empty value.

use std::rc::Rc;

use crate::lang::{
    common::{ds::set::IdSet, source::Phrase},
    hints::{alter, fields},
    traits::{eq::SyntaxEq, free::FreeIds},
};

use crate::lang::sl;

// Hints

/// Prose metadata with the location of its owning declaration.
pub type Hints = Phrase<HintsKind>;

/// Optional prose metadata for a PL node.
#[derive(Clone, Debug, PartialEq, Default)]
pub struct HintsKind {
    /// Replaces the node's prose.
    pub prose: Option<alter::AlterHint>,
    /// Replaces the prose of the node's inputs.
    pub prose_in: Option<alter::AlterHint>,
    /// Replaces the prose of the node's outputs.
    pub prose_out: Option<alter::AlterHint>,
    /// Replaces the prose when a condition holds.
    pub prose_true: Option<alter::AlterHint>,
    /// Replaces the prose when a condition does not hold.
    pub prose_false: Option<alter::AlterHint>,
    /// Names the fields of a value being destructured.
    pub prose_fields: Option<fields::FieldHint>,
    /// Input expressions to show in place of the node's own.
    pub prose_input_exps: Option<Vec<sl::ast::Exp>>,
    /// Output expressions to show in place of the node's own.
    pub prose_output_exps: Option<Vec<sl::ast::Exp>>,
}

impl Default for Hints {
    fn default() -> Self {
        crate::phrase! { node: HintsKind::default(), span: Default::default() }
    }
}

impl Hints {
    /// Returns the empty hints shared by every node without prose metadata.
    pub fn empty() -> Rc<Self> {
        thread_local! {
            // Reuse one empty value instead of allocating per node
            static HINTS_EMPTY: Rc<Hints> = Rc::new(Hints::default());
        }
        HINTS_EMPTY.with(Rc::clone)
    }

    /// Shares the hints, reusing the empty value when they equal it.
    ///
    /// Hints that carry only a declaration span stay distinct.
    pub fn into_shared(self) -> Rc<Self> {
        if self == Self::default() { Self::empty() } else { Rc::new(self) }
    }
}

/// A PL node paired with prose metadata
///
/// Does not implement `Deref`;
/// access node and hints explicitly.
/// Hints are shared, so cloning a node does not copy them.
#[derive(Clone, Debug, PartialEq)]
pub struct Annotated<N> {
    /// The syntax node.
    pub node: N,
    /// Its prose hints, empty by default.
    pub hints: Rc<Hints>,
}

impl<N: SyntaxEq> SyntaxEq for Annotated<N> {
    fn syntax_eq(&self, other: &Self) -> bool {
        self.node.syntax_eq(&other.node)
    }
}

impl<N: FreeIds> FreeIds for Annotated<N> {
    fn free_ids(&self) -> IdSet {
        self.node.free_ids()
    }
}

impl<N> Annotated<N> {
    /// Builds a node with no prose hints.
    pub fn new(node: N) -> Self {
        Self { node, hints: Hints::empty() }
    }
}

/// Builds a syntax node paired with prose metadata.
#[macro_export]
macro_rules! annotated {
    (node: $node:expr, hints: $hints:expr $(,)?) => {
        $crate::lang::pl::annot::Annotated { node: $node, hints: $hints }
    };
    (node: $node:expr, span: $span:expr $(,)?) => {
        $crate::annotated! {
            node: $crate::phrase! {
                node: $node,
                span: $span.span.clone(),
            },
            hints: $crate::lang::pl::annot::Hints::empty(),
        }
    };
}

/// Builds a source-annotated syntax node paired with prose metadata.
#[macro_export]
macro_rules! annotated_note_phrase {
    (
        node: $node:expr,
        note: $note:expr,
        span: $span:expr,
        hints: $hints:expr $(,)?
    ) => {
        $crate::annotated! {
            node: $crate::note_phrase! {
                node: $node,
                note: $note,
                span: $span,
            },
            hints: $hints,
        }
    };
    (
        node: $node:expr,
        note: $note:expr,
        span: $span:expr $(,)?
    ) => {
        $crate::annotated_note_phrase! {
            node: $node,
            note: $note,
            span: $span,
            hints: $crate::lang::pl::annot::Hints::empty(),
        }
    };
}
