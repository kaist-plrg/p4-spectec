//! Source positions, spans, and the spanned syntax node `NotePhrase`
//!
//! Every syntax node is a `NotePhrase { node, note, span }`;
//! `Phrase<T>` is the common case with no note.
//! Spans print as `file:line.col-line.col`;
//! the default span, used for generated syntax, prints only its file.
//! The `serde_state` impls thread an encoding context through nested nodes.

use std::{fmt, rc::Rc};

use serde::{Deserialize, Serialize};
use serde_derive_state::DeserializeState;

// == Positions

/// A source position.
#[derive(Clone, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct Position {
    /// Source file, shared across positions.
    pub file: Rc<str>,
    /// One-based line.
    pub line: usize,
    /// Zero-based column; printed one-based.
    pub column: usize,
}

impl Position {
    /// Constructs a source position.
    pub fn new(file: impl Into<Rc<str>>, line: usize, column: usize) -> Self {
        Self { file: file.into(), line, column }
    }
}

impl fmt::Display for Position {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "{}.{}", self.line, self.column + 1)
    }
}

// == Spans

/// A source span between two positions.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct Span {
    /// Start of the span.
    pub left: Position,
    /// End of the span.
    pub right: Position,
}

impl Default for Span {
    fn default() -> Self {
        thread_local! {
            // Reuse the empty file names in generated source annotations
            static SPAN_EMPTY: Span = Span::new(Position::default(), Position::default());
        }
        SPAN_EMPTY.with(Clone::clone)
    }
}

impl Span {
    /// Constructs a span from its endpoints.
    pub fn new(left: Position, right: Position) -> Self {
        Self { left, right }
    }

    /// Covers all supplied spans.
    pub fn over(spans: &[Self]) -> Self {
        Self::over_iter(spans.iter().cloned())
    }

    /// Covers a span iterator without collecting its elements.
    pub fn over_iter(spans: impl IntoIterator<Item = Self>) -> Self {
        // Use the first actual span, including a generated source annotation
        let mut spans = spans.into_iter();
        let Some(span) = spans.next() else { return Self::default() };
        // Cover endpoints using the source position ordering
        spans.fold(span, |span_over, span| {
            Self::new(span_over.left.min(span.left), span_over.right.max(span.right))
        })
    }
}

impl fmt::Display for Span {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        // The default span prints only its file
        if self.left.line == 0 && self.left.column == 0 && self.left == self.right {
            return fmt.write_str(&self.left.file);
        }

        write!(fmt, "{}:{}", self.left.file, self.left)?;
        // A one-position span prints no end
        if self.left != self.right {
            write!(fmt, "-{}", self.right)?;
        }
        Ok(())
    }
}

// == Phrases

/// A syntax node paired with semantic and source annotations.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct NotePhrase<T, N = (), S = Span> {
    /// The syntax itself.
    pub node: T,
    /// A semantic annotation, such as the type of an expression.
    pub note: N,
    /// Where the node came from.
    pub span: S,
}

/// A syntax node paired with its source span.
pub type Phrase<T> = NotePhrase<T>;

impl<T: fmt::Display, N, S: fmt::Display> fmt::Display for NotePhrase<T, N, S> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "{} at {}", self.node, self.span)
    }
}

impl<T: std::error::Error, N: fmt::Debug, S: fmt::Debug + fmt::Display> std::error::Error
    for NotePhrase<T, N, S>
{
}

// == Constructors

/// Builds a syntax node with an explicit source span.
#[macro_export]
macro_rules! phrase {
    (node: $node:expr, span: $span:expr $(,)?) => {
        $crate::lang::common::source::NotePhrase { node: $node, note: (), span: $span }
    };
}

/// Builds a syntax node with semantic and source annotations.
#[macro_export]
macro_rules! note_phrase {
    (node: $node:expr, note: $note:expr, span: $span:expr $(,)?) => {
        $crate::lang::common::source::NotePhrase { node: $node, note: ($note).into(), span: $span }
    };
}

// == Serialization

// - Encode

impl<State> serde_state::SerializeState<State> for Span {
    fn serialize_state<Serializer>(
        &self,
        serializer: Serializer,
        _state: &State,
    ) -> Result<Serializer::Ok, Serializer::Error>
    where
        Serializer: serde::Serializer,
    {
        self.serialize(serializer)
    }
}

// Check the stack at every phrase before traversing recursive value/type nodes
impl<T, N, S, State> serde_state::SerializeState<State> for NotePhrase<T, N, S>
where
    T: serde_state::SerializeState<State>,
    N: serde_state::SerializeState<State>,
    S: serde_state::SerializeState<State>,
{
    fn serialize_state<Serializer>(
        &self,
        serializer: Serializer,
        state: &State,
    ) -> Result<Serializer::Ok, Serializer::Error>
    where
        Serializer: serde::Serializer,
    {
        use serde_state::ser::Seeded;

        stacker::maybe_grow(64 * 1024, 1024 * 1024, || {
            NotePhrase {
                node: Seeded::new(state, &self.node),
                note: Seeded::new(state, &self.note),
                span: Seeded::new(state, &self.span),
            }
            .serialize(serializer)
        })
    }
}

// - Decode

impl<'de, State> serde_state::DeserializeState<'de, State> for Span {
    fn deserialize_state<Deserializer>(
        _state: &mut State,
        deserializer: Deserializer,
    ) -> Result<Self, Deserializer::Error>
    where
        Deserializer: serde::Deserializer<'de>,
    {
        Self::deserialize(deserializer)
    }
}

impl<'de, T, N, S, State> serde_state::DeserializeState<'de, State> for NotePhrase<T, N, S>
where
    T: serde_state::DeserializeState<'de, State>,
    N: serde_state::DeserializeState<'de, State>,
    S: serde_state::DeserializeState<'de, State>,
{
    fn deserialize_state<Deserializer>(
        state: &mut State,
        deserializer: Deserializer,
    ) -> Result<Self, Deserializer::Error>
    where
        Deserializer: serde::Deserializer<'de>,
    {
        // Keep state-only attributes off the ordinary serde derives
        #[derive(DeserializeState)]
        #[serde(rename = "NotePhrase")]
        #[serde(deserialize_state = "State", de_parameters = "State")]
        #[serde(bound(
            deserialize = "T: serde_state::DeserializeState<'de, State>, N: serde_state::DeserializeState<'de, State>, S: serde_state::DeserializeState<'de, State>"
        ))]
        struct NotePhraseState<T, N, S> {
            #[serde(state)]
            node: T,
            #[serde(state)]
            note: N,
            #[serde(state)]
            span: S,
        }

        let NotePhraseState { node, note, span } =
            NotePhraseState::deserialize_state(state, deserializer)?;
        Ok(Self { node, note, span })
    }
}
