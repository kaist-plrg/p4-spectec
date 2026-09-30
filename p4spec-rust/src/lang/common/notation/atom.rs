//! Atoms, the literal pieces of mixfix notation
//!
//! Keywords and tags are identifiers, operators are quoted text,
//! and the rest are the fixed symbols of the specification language.
//! Tags must be upper identifiers and operators must be printable on one line;
//! the constructors enforce that.

use serde::{Deserialize, Serialize};

use std::{error::Error, fmt};

use crate::lang::{
    common::{ds::set::IdSet, source::Phrase},
    traits::{
        eq::SyntaxEq,
        free::FreeIds,
        print::{Print, Printer},
    },
};

// == Types

/// A literal piece of notation.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub enum Atom {
    /// Concrete object word such as `INT`
    Keyword(String),
    /// Silent meta case label such as `_NUM`
    Tag(String),
    /// Concrete operator such as `'+'`, `'->'`, or `'#'`
    Operator(String),
    /// `<:`
    Sub,
    /// `:>`
    Sup,
    /// `|-`
    Turnstile,
    /// `-|`
    Tilesturn,
    /// `->`
    Arrow,
    /// `->_`
    ArrowSub,
    /// `=>_`
    DoubleArrowSub,
    /// `==>`
    DoubleArrowLong,
    /// `~>`
    SqArrow,
    /// `~>*`
    SqArrowStar,
    /// `.`
    Dot,
    /// `..`
    Dot2,
    /// `...`
    Dot3,
    /// `;`
    Semicolon,
    /// `:`
    Colon,
    /// `:=`
    ColonEq,
    /// `~~`
    Tilde2,
    /// `\`
    Backslash,
    /// `` `< ``
    LAngle,
    /// `` `> ``
    RAngle,
    /// `` `( ``
    LParen,
    /// `` `) ``
    RParen,
    /// `` `[ ``
    LBrack,
    /// `` `] ``
    RBrack,
    /// `` `{ ``
    LBrace,
    /// `` `} ``
    RBrace,
}

// == Errors

/// A rejected atom constructor argument.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum AtomError {
    /// A tag that is not an upper identifier.
    TagInvalid(String),
    /// An operator containing a quote or newline.
    OperatorInvalid(String),
}

impl fmt::Display for AtomError {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::TagInvalid(id) => {
                write!(fmt, "invalid tag identifier {id:?}: expected an uppercase identifier")
            }
            Self::OperatorInvalid(op) => {
                write!(fmt, "invalid operator {op:?}: must not contain a quote or newline")
            }
        }
    }
}

impl Error for AtomError {}

// == Printing

impl Print for Atom {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        match self {
            Self::Keyword(id) => printer.write(id),
            Self::Tag(id) => write!(printer, "_{id}"),
            Self::Operator(op) => write!(printer, "'{op}'"),
            Self::Sub => printer.write("<:"),
            Self::Sup => printer.write(":>"),
            Self::Turnstile => printer.write("|-"),
            Self::Tilesturn => printer.write("-|"),
            Self::Arrow => printer.write("->"),
            Self::ArrowSub => printer.write("->_"),
            Self::DoubleArrowSub => printer.write("=>_"),
            Self::DoubleArrowLong => printer.write("==>"),
            Self::SqArrow => printer.write("~>"),
            Self::SqArrowStar => printer.write("~>*"),
            Self::Dot => printer.write("."),
            Self::Dot2 => printer.write(".."),
            Self::Dot3 => printer.write("..."),
            Self::Semicolon => printer.write(";"),
            Self::Colon => printer.write(":"),
            Self::ColonEq => printer.write(":="),
            Self::Tilde2 => printer.write("~~"),
            Self::Backslash => printer.write("\\"),
            Self::LAngle => printer.write("`<"),
            Self::RAngle => printer.write("`>"),
            Self::LParen => printer.write("`("),
            Self::RParen => printer.write("`)"),
            Self::LBrack => printer.write("`["),
            Self::RBrack => printer.write("`]"),
            Self::LBrace => printer.write("`{"),
            Self::RBrace => printer.write("`}"),
        }
    }
}

impl Print for Phrase<Atom> {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        self.node.print(printer)
    }
}

// == Syntax operations

impl SyntaxEq for Atom {
    fn syntax_eq(&self, other: &Self) -> bool {
        self == other
    }
}

impl FreeIds for Atom {
    fn free_ids(&self) -> IdSet {
        IdSet::new()
    }
}

// == Constructors

impl Atom {
    // - Keyword

    // - Tag

    /// Whether the identifier starts uppercase and continues alphanumerically.
    fn is_upid(id: &str) -> bool {
        // The empty string is not an identifier
        let Some((c_first, s_rest)) = id.as_bytes().split_first() else {
            return false;
        };

        c_first.is_ascii_uppercase()
            && s_rest
                .iter()
                .all(|byte| byte.is_ascii_alphanumeric() || matches!(byte, b'_' | b'\''))
    }

    /// Constructs a tag atom when the identifier is a valid upper identifier.
    pub fn tag(id: impl Into<String>) -> Result<Self, AtomError> {
        let id = id.into();
        if Self::is_upid(&id) { Ok(Self::Tag(id)) } else { Err(AtomError::TagInvalid(id)) }
    }

    // - Operator

    /// Constructs an operator atom when it can be represented in source syntax.
    pub fn operator(op: impl Into<String>) -> Result<Self, AtomError> {
        let op = op.into();
        if op.contains(['\'', '\n']) {
            Err(AtomError::OperatorInvalid(op))
        } else {
            Ok(Self::Operator(op))
        }
    }
}

// == Serialization

// - Encode

impl<State> serde_state::SerializeState<State> for Atom {
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

// - Decode

impl<'de, State> serde_state::DeserializeState<'de, State> for Atom {
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
