//! Contextual token adaptation between the lexer and LALRPOP
//!
//! `parser_tokens` wraps a lexer iterator in [`ParserTokens`].
//! Each `ParserTokens::next` call
//! converts source positions to [`Location`] handles
//! and forwards lexical failures as [`FrontendError`].
//! Outside arithmetic mode it relabels `Star` as `IterStar`
//! for postfix iteration.
//!
//! `ends_sequence` and `starts_sequence` identify adjacent notation atoms.
//! If both predicates match,
//! `ParserTokens::next` returns a synthetic `Sequence`
//! and stores the real lookahead in `pending` for the following call.
//!
//! # Examples
//!
//! ```text
//! lexer:  UpperId("A"), UpperId("B")
//! parser: UpperId("A"), Sequence, UpperId("B")
//!
//! expression mode: Star -> IterStar
//! arithmetic mode: Star -> Star
//! ```

use crate::lang::common::source::{Phrase, Position};

use super::{
    ctx::{Context, Location},
    error::{FrontendError, LexError},
    lexer::Token,
};

// = Helpers

// - Sequence boundaries

/// Whether a token can begin a notation atom that follows another.
fn starts_sequence(token: &Token) -> bool {
    matches!(
        token,
        Token::TagUpperId(_)
            | Token::Operator(_)
            | Token::TickLeftParen
            | Token::TickLeftBracket
            | Token::TickLeftBrace
            | Token::TickLeftAngle
            | Token::Dollar
            | Token::DoubleHash
            | Token::LeftParen
            | Token::LeftBrace
            | Token::Hole
            | Token::NumberedHole(_)
            | Token::MultipleHole
            | Token::EmptyHole
            | Token::Latex
            | Token::Bool
            | Token::Nat
            | Token::Int
            | Token::Text
            | Token::Epsilon
            | Token::BoolLiteral(_)
            | Token::NaturalLiteral(_)
            | Token::HexLiteral(_)
            | Token::TextLiteral(_)
            | Token::UpperId(_)
            | Token::LowerId(_)
            | Token::UpperIdLeftParen(_)
    )
}

/// Whether a token can end a notation atom that another follows.
fn ends_sequence(token: &Token) -> bool {
    matches!(
        token,
        Token::TagUpperId(_)
            | Token::Operator(_)
            | Token::TickRightParen
            | Token::TickRightBracket
            | Token::TickRightBrace
            | Token::TickRightAngle
            | Token::RightParen
            | Token::RightBracket
            | Token::RightBrace
            | Token::Question
            | Token::Star
            | Token::IterStar
            | Token::Epsilon
            | Token::Bool
            | Token::Nat
            | Token::Int
            | Token::Text
            | Token::BoolLiteral(_)
            | Token::NaturalLiteral(_)
            | Token::HexLiteral(_)
            | Token::TextLiteral(_)
            | Token::UpperId(_)
            | Token::LowerId(_)
            | Token::DotId(_)
            | Token::Hole
            | Token::NumberedHole(_)
            | Token::MultipleHole
            | Token::EmptyHole
    )
}

// = Token stream

/// The adapted token stream.
pub(crate) struct ParserTokens<'ctx, I: Iterator> {
    /// Parser state: modes and position interning.
    ctx: &'ctx Context,
    /// The lexer.
    lexemes: I,
    /// Where the last emitted token ended, for a `Sequence` span.
    previous_right: Option<Position>,
    /// The last emitted token, to test `ends_sequence`.
    previous_token: Option<Token>,
    /// A lexeme held back while a `Sequence` is emitted first.
    pending: Option<Phrase<Token>>,
}

impl<I> Iterator for ParserTokens<'_, I>
where
    I: Iterator<Item = Result<Phrase<Token>, LexError>>,
{
    type Item = Result<(Location, Token, Location), FrontendError>;

    fn next(&mut self) -> Option<Self::Item> {
        // A held-back lexeme comes before the next one from the lexer
        let mut lexeme = match self.pending.take() {
            Some(lexeme) => lexeme,
            None => match self.lexemes.next()? {
                Ok(lexeme) => lexeme,
                Err(error) => return Some(Err(error)),
            },
        };

        // `*` is iteration unless the parser is inside arithmetic
        if lexeme.node == Token::Star && !self.ctx.in_arith() {
            lexeme.node = Token::IterStar;
        }

        // Two adjacent atoms get a `Sequence` between them; the lexeme waits
        if self.previous_token.as_ref().is_some_and(ends_sequence) && starts_sequence(&lexeme.node)
        {
            let pos_l = self
                .previous_right
                .clone()
                .expect("previous token position");
            let pos_r = lexeme.span.left.clone();
            self.pending = Some(lexeme);
            self.previous_token = Some(Token::Sequence);
            self.previous_right = Some(pos_r.clone());
            return Some(Ok((self.ctx.location(pos_l), Token::Sequence, self.ctx.location(pos_r))));
        }

        // Intern both ends and remember this token for the next call
        let loc_l = self.ctx.location(lexeme.span.left);
        self.previous_right = Some(lexeme.span.right.clone());
        let loc_r = self.ctx.location(lexeme.span.right);
        self.previous_token = Some(lexeme.node.clone());
        Some(Ok((loc_l, lexeme.node, loc_r)))
    }
}

// = Entry point

/// Wraps a lexeme stream for the parser.
pub(crate) fn parser_tokens<I>(ctx: &Context, lexemes: I) -> ParserTokens<'_, I>
where
    I: Iterator,
{
    ParserTokens { ctx, lexemes, previous_right: None, previous_token: None, pending: None }
}
