//! Source positions, spans, and the spanned syntax node `NotePhrase`
//!
//! Every syntax node is a `NotePhrase { node, note, span }`;
//! `Phrase<T>` is the common case with no note.
//! Positions name their file by an interned `FileId`, so spans are `Copy`.
//! Spans print as `file:line.col-line.col`;
//! the default span, used for generated syntax, prints only its file.
//! The `serde_state` impls thread an encoding context through nested nodes.

use std::{
    cmp::Ordering,
    collections::HashMap,
    fmt,
    sync::{Mutex, OnceLock, PoisonError},
};

use serde::{Deserialize, Serialize};
use serde_derive_state::DeserializeState;

// == Files

/// A source file name, interned once per process.
///
/// Positions hold the handle, so copying a span touches no reference count;
/// id 0 is the empty name of generated syntax.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash)]
pub struct FileId(u32);

/// Interned names in id order, and the id of each name.
struct FileTable {
    names: Vec<&'static str>,
    ids: HashMap<&'static str, FileId>,
}

/// The process-wide file table, starting with the empty name.
fn file_table() -> &'static Mutex<FileTable> {
    static FILES: OnceLock<Mutex<FileTable>> = OnceLock::new();
    FILES.get_or_init(|| {
        let names = vec![""];
        let ids = HashMap::from([("", FileId(0))]);
        Mutex::new(FileTable { names, ids })
    })
}

impl FileId {
    /// Interns a file name; the same name always yields the same id.
    pub fn intern(name: &str) -> Self {
        let mut table = file_table().lock().unwrap_or_else(PoisonError::into_inner);
        if let Some(file) = table.ids.get(name) {
            return *file;
        }
        // Names live as long as the process, so they are handed out as static strs
        let name: &'static str = Box::leak(name.into());
        let file = FileId(u32::try_from(table.names.len()).expect("fewer than 2^32 source files"));
        table.names.push(name);
        table.ids.insert(name, file);
        file
    }

    /// Returns the interned name.
    pub fn name(self) -> &'static str {
        let table = file_table().lock().unwrap_or_else(PoisonError::into_inner);
        table.names[self.0 as usize]
    }
}

// == Positions

/// A source position.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash)]
pub struct Position {
    /// Source file.
    pub file: FileId,
    /// One-based line.
    pub line: u32,
    /// Zero-based column; printed one-based.
    pub column: u32,
}

impl Position {
    /// Constructs a source position.
    pub fn new(file: FileId, line: usize, column: usize) -> Self {
        let line = u32::try_from(line).expect("source line fits in u32");
        let column = u32::try_from(column).expect("source column fits in u32");
        Self { file, line, column }
    }
}

/// Orders by file name, then line and column.
///
/// Comparing names rather than ids keeps the order independent of
/// which file was interned first, so covering spans keeps its endpoints.
impl Ord for Position {
    fn cmp(&self, other: &Self) -> Ordering {
        // The same file needs no name lookup
        let file_order = if self.file == other.file {
            Ordering::Equal
        } else {
            self.file.name().cmp(other.file.name())
        };
        file_order
            .then(self.line.cmp(&other.line))
            .then(self.column.cmp(&other.column))
    }
}

impl PartialOrd for Position {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl fmt::Display for Position {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "{}.{}", self.line, u64::from(self.column) + 1)
    }
}

// == Spans

/// A source span between two positions.
#[derive(
    Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct Span {
    /// Start of the span.
    pub left: Position,
    /// End of the span.
    pub right: Position,
}

impl Span {
    /// Constructs a span from its endpoints.
    pub fn new(left: Position, right: Position) -> Self {
        Self { left, right }
    }

    /// Covers all supplied spans.
    pub fn over(spans: &[Self]) -> Self {
        Self::over_iter(spans.iter().copied())
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
            return fmt.write_str(self.left.file.name());
        }

        write!(fmt, "{}:{}", self.left.file.name(), self.left)?;
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

/// The encoded shape of a position, naming its file.
#[derive(Serialize)]
#[serde(rename = "Position")]
struct PositionEncode<'a> {
    file: &'a str,
    line: u32,
    column: u32,
}

// Encode the file by name, so payloads keep their shape
impl Serialize for Position {
    fn serialize<Serializer>(
        &self,
        serializer: Serializer,
    ) -> Result<Serializer::Ok, Serializer::Error>
    where
        Serializer: serde::Serializer,
    {
        let position =
            PositionEncode { file: self.file.name(), line: self.line, column: self.column };
        position.serialize(serializer)
    }
}

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

/// The decoded shape of a position.
#[derive(Deserialize)]
#[serde(rename = "Position")]
struct PositionDecode {
    file: String,
    line: u32,
    column: u32,
}

// Intern the decoded file name
impl<'de> Deserialize<'de> for Position {
    fn deserialize<Deserializer>(deserializer: Deserializer) -> Result<Self, Deserializer::Error>
    where
        Deserializer: serde::Deserializer<'de>,
    {
        let PositionDecode { file, line, column } = PositionDecode::deserialize(deserializer)?;
        Ok(Self { file: FileId::intern(&file), line, column })
    }
}

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
