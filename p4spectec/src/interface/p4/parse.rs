//! Preprocessing and parsing P4 into runtime values
//!
//! `parse_file` preprocesses includes before delegating to `parse_string`,
//! which creates a fresh name-resolution context, lexes the source,
//! adapts located tokens for LALRPOP, and builds the mixfix value tree.
//! Parse failures retain lexer locations through the same context.
//! For example, an empty source produces the grammar's empty `p4program` case.

use std::{
    path::{Path, PathBuf},
    rc::Rc,
};

use lalrpop_util::ParseError;

use crate::lang::{
    common::source::{Phrase, Position, Span},
    data::{arena::Arena, value::flat::Value},
};

use super::{
    context::{Context, Location},
    error::P4Error,
    lexer::{Lexer, Token},
    parser::p4programParser,
    preprocessor::preprocess,
};

// == Parsing

// - LALRPOP bridge

/// Interns each token's positions into copyable locations for LALRPOP.
fn parser_input<'a, I>(
    ctx: &'a Context,
    tokens: I,
    position: Position,
) -> impl Iterator<Item = Result<(Location, Token, Location), P4Error>> + 'a
where
    I: Iterator<Item = Result<Phrase<Token>, P4Error>> + 'a,
{
    // Each left location remembers the previous right one, for empty spans
    let mut location_prev = ctx.location_add(position, None);
    tokens.map(move |token| {
        token.map(|token| {
            let location_l = ctx.location_add(token.span.left, Some(location_prev));
            let location_r = ctx.location_add(token.span.right, None);
            location_prev = location_r;
            (location_l, token.node, location_r)
        })
    })
}

/// Maps a LALRPOP error to a syntax error with a resolved span.
fn translate_lalrpop_error(ctx: &Context, error: ParseError<Location, Token, P4Error>) -> P4Error {
    let span = match error {
        // Point errors span one location
        ParseError::InvalidToken { location } | ParseError::UnrecognizedEof { location, .. } => {
            let position = ctx.location_get(location);
            Span::new(position.clone(), position)
        }
        // Token errors span the token
        ParseError::UnrecognizedToken { token: (location_l, _, location_r), .. }
        | ParseError::ExtraToken { token: (location_l, _, location_r) } => {
            ctx.location_span(location_l, location_r)
        }
        // Lexer errors pass through unchanged
        ParseError::User { error } => return error,
    };
    P4Error::new(span, super::error::P4SyntaxError::GrammarInvalid)
}

// - Source strings

/// Parses an already preprocessed P4 source string into a value tree.
pub fn parse_string(
    arena: &mut Arena,
    path: impl AsRef<Path>,
    source: &str,
) -> Result<Value, P4Error> {
    let file: Rc<str> = Rc::from(path.as_ref().to_string_lossy().into_owned());
    // The lexer and parser share one context for name classification
    let ctx = Rc::new(Context::new(arena));
    let position = Position::new(Rc::clone(&file), 1, 0);
    let mut lexer = Lexer::new(Rc::clone(&ctx), file, source);
    let input = parser_input(ctx.as_ref(), &mut lexer, position);

    let result = p4programParser::new().parse(ctx.as_ref(), input);
    result.map_err(|error| translate_lalrpop_error(ctx.as_ref(), error))
}

// - Source files

/// Preprocesses and parses a P4 source file.
pub fn parse_file(
    arena: &mut Arena,
    includes: &[PathBuf],
    path: impl AsRef<Path>,
) -> Result<Value, P4Error> {
    let path = path.as_ref();
    let source = preprocess(includes, path)?;
    parse_string(arena, path, &source)
}
