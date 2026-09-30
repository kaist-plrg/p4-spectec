//! String and filesystem entry points for the STF parser
//!
//! `parse_file` reads a source before delegating to `parse_str`. Located lexer
//! tokens become LALRPOP input, statements retain their source spans, and
//! parse failures become typed errors. For example, `wait` becomes one located
//! `Statement::Wait`.

use std::{fs, path::Path, rc::Rc};

use lalrpop_util::ParseError;

use crate::lang::common::source::{Phrase, Position, Span};

use super::{
    ast::Program,
    error::{StfError, StfErrorKind},
    lexer::{Lexer, Token},
    parser,
};

const MAX_PRIORITY: i64 = i64::MAX / 2;

// == Parsing

// - LALRPOP bridge

/// A copyable line and column within the source being parsed.
#[derive(Clone, Copy, Debug, Default)]
pub(crate) struct Location {
    line: usize,
    column: usize,
}

impl Location {
    /// The line and column of a source position.
    fn from_position(position: Position) -> Self {
        Self { line: position.line, column: position.column }
    }

    /// A source position in `file` at this location.
    fn into_position(self, file: Rc<str>) -> Position {
        Position::new(file, self.line, self.column)
    }
}

/// The span between two locations in `file`.
pub(crate) fn location_span(file: &Rc<str>, loc_l: Location, loc_r: Location) -> Span {
    let pos_l = loc_l.into_position(Rc::clone(file));
    let pos_r = loc_r.into_position(Rc::clone(file));
    Span::new(pos_l, pos_r)
}

/// Adapts located lexer tokens into the triples LALRPOP expects.
fn parser_input<I>(tokens: I) -> impl Iterator<Item = Result<(Location, Token, Location), StfError>>
where
    I: Iterator<Item = Result<Phrase<Token>, StfError>>,
{
    tokens.map(|token| {
        token.map(|token| {
            let span = token.span;
            let loc_l = Location::from_position(span.left);
            let loc_r = Location::from_position(span.right);
            (loc_l, token.node, loc_r)
        })
    })
}

/// Turns a LALRPOP error into a typed STF error with its span.
fn translate_lalrpop_error(
    file: &Rc<str>,
    error: ParseError<Location, Token, StfError>,
) -> StfError {
    let (kind, span) = match error {
        ParseError::InvalidToken { location: loc } => {
            let span = location_span(file, loc, loc);
            (StfErrorKind::TokenInvalid, span)
        }
        ParseError::UnrecognizedEof { location: loc, .. } => {
            let span = location_span(file, loc, loc);
            (StfErrorKind::InputIncomplete, span)
        }
        ParseError::UnrecognizedToken { token: (loc_l, _, loc_r), .. } => {
            (StfErrorKind::TokenUnexpected, location_span(file, loc_l, loc_r))
        }
        ParseError::ExtraToken { token: (loc_l, _, loc_r) } => {
            (StfErrorKind::TokenExtra, location_span(file, loc_l, loc_r))
        }
        ParseError::User { error } => return error,
    };
    StfError::new(span, kind)
}

/// Parses an add priority, rejecting values above the half-range cap.
pub(crate) fn parse_priority(spelling: String, span: Span) -> Result<i64, StfError> {
    let priority = spelling.parse::<i64>().ok();
    match priority.filter(|priority| *priority <= MAX_PRIORITY) {
        Some(priority) => Ok(priority),
        None => {
            let kind = StfErrorKind::PriorityOutOfBounds(spelling);
            Err(StfError::new(span, kind))
        }
    }
}

// - Source strings

/// Parses an STF source string and retains statement source spans.
pub fn parse_str(file: impl Into<Rc<str>>, source: &str) -> Result<Program, StfError> {
    let file = file.into();
    let lexer = Lexer::new(Rc::clone(&file), source);
    let input = parser_input(lexer);
    let result = parser::StatementsParser::new().parse(&file, input);
    result.map_err(|error| translate_lalrpop_error(&file, error))
}

// - Source files

/// Reads and parses an STF file.
pub fn parse_file(path: impl AsRef<Path>) -> Result<Program, StfError> {
    let path = path.as_ref();
    let file = Rc::<str>::from(path.to_string_lossy().into_owned());
    let source = fs::read_to_string(path).map_err(|error| {
        let position = Position::new(Rc::clone(&file), 0, 0);
        StfError::new(Span::new(position.clone(), position), StfErrorKind::Io(error))
    })?;
    parse_str(file, &source)
}
