//! Lazy source-level tokenization for SpecTec
//!
//! `Iterator::next` requests one lexeme from `Lexer::scan_token`,
//! the main lexer state.
//! It dispatches on the current byte and transitions to `scan_after_newline`,
//! `scan_after_two_newlines`, `scan_comment`, or `scan_text`
//! when those inputs need their own state.
//! Each state advances the UTF-8 byte cursor
//! and attaches the traversed [`Span`] to its resulting [`Token`].
//!
//! Branches with a shared prefix implement maximal munch locally:
//! comments precede punctuation, comma-newline precedes comma,
//! and specialized tag, dot-identifier, and numbered-hole rules
//! precede their shorter fallbacks.
//! `scan_fixed` likewise checks longer punctuation before its prefixes.
//! `scan_identifier` recognizes keywords
//! and asks the parser-owned classifier
//! whether an uppercase name is a variable,
//! while `scan_text` delegates escapes to `scan_escape`.
//!
//! The downstream `tokens::parser_tokens` adapter
//! inserts implicit `Sequence` tokens,
//! distinguishes postfix iteration from arithmetic multiplication,
//! and interns source positions for LALRPOP.
//!
//! Source and decoded text literals are UTF-8 strings.
//! Hex byte escapes may combine into a valid UTF-8 sequence;
//! byte-only results are rejected with `parse/text-encoding-invalid`
//! so tokens fit the language model's `String` text representation.
//!
//! # Examples
//!
//! ```text
//! source: F(x)
//! lexer:  UpperIdLeftParen("F"), LowerId("x"), RightParen
//!
//! source: A B
//! lexer:  UpperId("A"), UpperId("B")
//! parser: UpperId("A"), Sequence, UpperId("B")
//!
//! source: X                  classifier("X") = variable
//! lexer:  LowerId("X")
//!
//! source: "\48\69"
//! lexer:  TextLiteral("Hi")
//! ```

use num_bigint::BigInt;

use crate::lang::common::{
    prim::num::Natural,
    source::{FileId, Phrase, Position, Span},
};

use super::error::{self, LexError};

/// A token consumed by the SpecTec grammar.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Token {
    TagUpperId(String),
    Operator(String),
    TickLeftParen,
    TickRightParen,
    TickLeftBracket,
    TickRightBracket,
    TickLeftBrace,
    TickRightBrace,
    TickLeftAngle,
    TickRightAngle,
    NewlineBar,
    Newline2,
    Newline3,
    /// Parser-only marker for juxtaposed grammar atoms.
    Sequence,
    Subtype,
    Turnstile,
    Tilesturn,
    Arrow,
    ArrowSub,
    DoubleArrow,
    DoubleArrowSub,
    DoubleArrowBoth,
    DoubleArrowLong,
    SquigglyArrow,
    SquigglyArrowStar,
    And,
    Or,
    Dot,
    DoubleDot,
    TripleDot,
    Comma,
    CommaNewline,
    Semicolon,
    Colon,
    DoubleColon,
    ColonSlash,
    ColonEquals,
    Hash,
    DoubleHash,
    Dollar,
    Question,
    Tilde,
    DoubleTilde,
    LeftAngle,
    LeftAngleDash,
    LeftAngleEquals,
    RightAngle,
    RightAngleEquals,
    RightAngleLeftParen,
    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    LeftBrace,
    RightBrace,
    Plus,
    DoublePlus,
    Minus,
    Dash,
    Star,
    /// Parser-only spelling of `*` when it closes an iterated expression.
    IterStar,
    Slash,
    Backslash,
    Hole,
    NumberedHole(usize),
    MultipleHole,
    EmptyHole,
    Equals,
    NotEquals,
    Up,
    Bar,
    Latex,
    Bool,
    Nat,
    Int,
    Text,
    Syntax,
    Extern,
    Table,
    Relation,
    RuleGroup,
    Rule,
    Var,
    Builtin,
    Dec,
    Def,
    If,
    Otherwise,
    Debug,
    HintLeftParen,
    Epsilon,
    BoolLiteral(bool),
    NaturalLiteral(Natural),
    HexLiteral(Natural),
    TextLiteral(String),
    UpperId(String),
    LowerId(String),
    DotId(String),
    UpperIdLeftParen(String),
    LowerIdLeftParen(String),
    UpperIdLeftAngle(String),
    LowerIdLeftAngle(String),
    Eof,
}

/// A byte cursor into a UTF-8 source string.
#[derive(Clone, Copy)]
struct Cursor {
    /// Byte offset into the source.
    offset: usize,
    /// One-based line of the offset.
    line: usize,
    /// Byte offset where the current line begins, for columns.
    line_start: usize,
}

/// A lazy SpecTec token stream
///
/// The classifier relabels uppercase identifiers
/// that are variables in the parser's current scope.
/// Input is valid UTF-8 by construction;
/// file entry points must report decoding failures before constructing a lexer.
pub struct Lexer<'input, Classify> {
    /// File of the positions.
    file: FileId,
    /// The whole source text.
    source: &'input str,
    /// Where the next lexeme starts.
    cursor: Cursor,
    /// Set after `Eof` or an error; the stream then ends.
    finished: bool,
    /// Whether scanning stopped inside an unfinished block comment.
    in_block_comment: bool,
    /// Whether an uppercase identifier is a variable in the parser's scope.
    classify_uppercase: Classify,
}

// - Construction

impl<'input, Classify> Lexer<'input, Classify>
where
    Classify: FnMut(&str) -> bool,
{
    /// Tokenizes `source` using the parser's uppercase-variable classifier.
    pub fn new(file: FileId, source: &'input str, classify_uppercase: Classify) -> Self {
        Self {
            file,
            source,
            cursor: Cursor { offset: 0, line: 1, line_start: 0 },
            finished: false,
            in_block_comment: false,
            classify_uppercase,
        }
    }
}

impl<Classify> Lexer<'_, Classify>
where
    Classify: FnMut(&str) -> bool,
{
    /// Reports whether tokenization stopped inside a block comment.
    pub(crate) fn in_block_comment(&self) -> bool {
        self.in_block_comment
    }

    // - Cursor movement

    /// Moves the cursor to an offset on the current line.
    fn advance_to(&mut self, offset: usize) {
        self.cursor.offset = offset;
    }

    /// Moves the cursor forward on the current line.
    fn advance_add(&mut self, length: usize) {
        self.cursor.offset += length;
    }

    /// Consumes a newline, starting a new line.
    fn advance_newline(&mut self) {
        self.cursor.offset += 1;
        self.cursor.line += 1;
        self.cursor.line_start = self.cursor.offset;
    }

    /// Moves to the newline ending the current line, or to the end.
    fn advance_to_line_end(&mut self) {
        self.cursor.offset = self.find_line_end(self.cursor.offset);
    }

    // - Cursor inspection

    /// The byte under the cursor.
    fn cursor_current(&self) -> Option<u8> {
        self.cursor_offset(self.cursor.offset)
    }

    /// The byte at an offset.
    fn cursor_offset(&self, offset: usize) -> Option<u8> {
        self.source.as_bytes().get(offset).copied()
    }

    /// Whether the source at the cursor begins with `prefix`.
    fn cursor_starts_with(&self, prefix: &str) -> bool {
        self.source[self.cursor.offset..].starts_with(prefix)
    }

    /// Whether the cursor is at the end.
    fn cursor_is_eof(&self) -> bool {
        self.cursor.offset == self.source.len()
    }

    // - Finders for token boundaries

    /// The offset after the spaces and tabs at the cursor.
    fn find_indentation_end(&self) -> usize {
        let mut end = self.cursor.offset;
        while matches!(self.cursor_offset(end), Some(b' ' | b'\t')) {
            end += 1;
        }
        end
    }

    /// The offset of the next newline from `start`, or the end.
    fn find_line_end(&self, start: usize) -> usize {
        self.source.as_bytes()[start..]
            .iter()
            .position(|byte| *byte == b'\n')
            .map_or(self.source.len(), |relative| start + relative)
    }

    /// The offset after the identifier bytes from `start`.
    fn find_identifier_end(&self, start: usize) -> usize {
        let mut end = start;
        while self
            .cursor_offset(end)
            .is_some_and(|byte| byte.is_ascii_alphanumeric() || matches!(byte, b'_' | b'\''))
        {
            end += 1;
        }
        end
    }

    /// The offset after digits from `start`, allowing single `_` separators.
    fn find_separated_digits_end(&self, start: usize, is_valid: fn(u8) -> bool) -> usize {
        let mut end = start + 1;
        while self.cursor_offset(end).is_some_and(is_valid) {
            end += 1;
        }
        // An underscore counts only when a digit follows
        while self.cursor_offset(end) == Some(b'_')
            && self.cursor_offset(end + 1).is_some_and(is_valid)
        {
            end += 2;
            while self.cursor_offset(end).is_some_and(is_valid) {
                end += 1;
            }
        }
        end
    }

    // - Source locations and results

    /// The source position of a cursor.
    fn position(&self, cursor: Cursor) -> Position {
        Position::new(self.file, cursor.line, cursor.offset - cursor.line_start)
    }

    /// The span from a start cursor to the current one.
    fn span(&self, cursor_start: Cursor) -> Span {
        Span::new(self.position(cursor_start), self.position(self.cursor))
    }

    /// A token spanning from `start` to the cursor.
    fn lexeme(&self, token: Token, start: Cursor) -> Phrase<Token> {
        crate::phrase! {
            node: token,
            span: self.span(start),
        }
    }

    // - Helpers

    /// Whether a byte can begin an identifier.
    fn is_identifier_start(byte: u8) -> bool {
        byte.is_ascii_alphabetic() || byte == b'_'
    }

    /// Whether a byte is a decimal digit.
    fn is_digit(byte: u8) -> bool {
        byte.is_ascii_digit()
    }

    /// Whether a byte is a hexadecimal digit; letters must be uppercase.
    fn is_hex_digit(byte: u8) -> bool {
        byte.is_ascii_digit() || matches!(byte, b'A'..=b'F')
    }

    /// Drops the `_` separators of a digit sequence.
    fn strip_underscores(digits: &str) -> String {
        digits
            .chars()
            .filter(|character| *character != '_')
            .collect()
    }

    // - Token state

    /// Scans the next lexeme, skipping whitespace and comments.
    fn scan_token(&mut self) -> Result<Phrase<Token>, LexError> {
        loop {
            let start = self.cursor;
            if self.cursor_is_eof() {
                return Ok(self.lexeme(Token::Eof, start));
            }

            let byte = self.cursor_current().expect("cursor is within source");
            match byte {
                // Block comment: skip it, nesting allowed
                b'(' if self.cursor_starts_with("(;") => {
                    self.advance_add(2);
                    self.scan_comment(start)?;
                    continue;
                }
                // Line comment: skip it, but the newline after it is layout
                b';' if self.cursor_starts_with(";;") => {
                    if let Some(lexeme) = self.scan_line_comment(start)? {
                        return Ok(lexeme);
                    }
                    continue;
                }
                // Escaped newline: line continuation
                b'\\' if self.cursor_starts_with("\\\n") => {
                    self.advance_add(1);
                    self.advance_newline();
                    continue;
                }
                // A newline may be layout: bar, double, or triple newline
                b'\n' => {
                    self.advance_newline();
                    if let Some(lexeme) = self.scan_after_newline()? {
                        return Ok(lexeme);
                    }
                    continue;
                }
                // Other whitespace is skipped
                b' ' | b'\t' | b'\r' => {
                    self.advance_add(1);
                    continue;
                }
                // Text literal
                b'"' => return self.scan_text(start),
                // Quoted operator
                b'\'' => return self.scan_operator(start),
                // A comma ending a line is its own token
                b',' => {
                    if let Some(lexeme) = self.scan_comma_newline(start) {
                        return Ok(lexeme);
                    }
                }
                // `_Name` is a tag atom
                b'_' => {
                    if let Some(lexeme) = self.scan_tag(start) {
                        return Ok(lexeme);
                    }
                }
                // `.name` is a field access
                b'.' => {
                    if let Some(lexeme) = self.scan_dot_identifier(start) {
                        return Ok(lexeme);
                    }
                }
                // `%N` is a numbered hole
                b'%' => {
                    if let Some(lexeme) = self.scan_numbered_hole(start)? {
                        return Ok(lexeme);
                    }
                }
                _ => {}
            }

            // Then numbers, identifiers, punctuation, in that order
            if Self::is_digit(byte) {
                return Ok(self.scan_number(start).expect("digit starts a number"));
            }
            if Self::is_identifier_start(byte) {
                return Ok(self
                    .scan_identifier(start)
                    .expect("identifier-start byte starts an identifier"));
            }
            if let Some(lexeme) = self.scan_fixed(start) {
                return Ok(lexeme);
            }

            // Nothing matched
            return Err(self.unrecognized_character(start));
        }
    }

    // - Newline states

    /// After one newline: a `| ` bar, a second newline, or nothing.
    fn scan_after_newline(&mut self) -> Result<Option<Phrase<Token>>, LexError> {
        if let Some(lexeme) = self.scan_newline_bar() {
            return Ok(Some(lexeme));
        }

        // A blank line means at least two newlines
        let end = self.find_indentation_end();
        if self.cursor_offset(end) == Some(b'\n') {
            self.advance_to(end);
            self.advance_newline();
            return self.scan_after_two_newlines().map(Some);
        }

        Ok(None)
    }

    /// After a blank line: a bar, a third newline, or `Newline2`;
    /// comment lines do not count.
    fn scan_after_two_newlines(&mut self) -> Result<Phrase<Token>, LexError> {
        loop {
            if let Some(lexeme) = self.scan_newline_bar() {
                return Ok(lexeme);
            }

            let start = self.cursor;
            let indent_end = self.find_indentation_end();
            // A second blank line is `Newline3`
            if self.cursor_offset(indent_end) == Some(b'\n') {
                self.advance_to(indent_end);
                self.advance_newline();
                return Ok(self.lexeme(Token::Newline3, start));
            }

            // A comment line is skipped without counting as a newline
            if self.source[indent_end..].starts_with(";;") {
                let line_end = self.find_line_end(indent_end);
                if self.cursor_offset(line_end) == Some(b'\n') {
                    self.advance_to(line_end);
                    self.advance_newline();
                    continue;
                }
                self.advance_to(line_end);
                return Ok(self.lexeme(Token::Eof, start));
            }

            // Blank lines before the end are just the end
            if indent_end == self.source.len() {
                self.advance_to(indent_end);
                return Ok(self.lexeme(Token::Eof, start));
            }

            // Otherwise the blank line separates definitions
            return Ok(self.lexeme(Token::Newline2, self.cursor));
        }
    }

    /// A `| ` at the start of a line, the rule-case separator.
    fn scan_newline_bar(&mut self) -> Option<Phrase<Token>> {
        // A bar must start the line and be followed by whitespace
        let start = self.cursor;
        let indent_end = self.find_indentation_end();
        if self.cursor_offset(indent_end) != Some(b'|')
            || !matches!(self.cursor_offset(indent_end + 1), Some(b' ' | b'\t'))
        {
            return None;
        }

        self.advance_to(indent_end + 2);
        Some(self.lexeme(Token::NewlineBar, start))
    }

    // - Comment state

    /// Skips a `(; ;)` block comment, which nests.
    fn scan_comment(&mut self, start: Cursor) -> Result<(), LexError> {
        self.in_block_comment = true;
        let mut starts = vec![start];
        while !starts.is_empty() {
            if self.cursor_is_eof() {
                let spans_open = starts
                    .into_iter()
                    .map(|start| {
                        Span::new(
                            Self::position(self, start),
                            Self::position(self, Cursor { offset: start.offset + 2, ..start }),
                        )
                    })
                    .collect();
                return Err(error::block_comment_incomplete(self.span(self.cursor), spans_open));
            }
            // Keep only the openers whose closing delimiters have not appeared
            if self.cursor_starts_with("(;") {
                starts.push(self.cursor);
                self.advance_add(2);
            } else if self.cursor_starts_with(";)") {
                starts.pop();
                self.advance_add(2);
            // Newlines keep the line count; other characters are skipped whole
            } else if self.cursor_current() == Some(b'\n') {
                self.advance_newline();
            } else {
                let character = self.source[self.cursor.offset..]
                    .chars()
                    .next()
                    .expect("nonempty comment source");
                self.advance_add(character.len_utf8());
            }
        }
        self.in_block_comment = false;
        Ok(())
    }

    // - Token-state layout rules

    /// Skips a `;;` comment and handles the newline after it as layout.
    fn scan_line_comment(&mut self, start: Cursor) -> Result<Option<Phrase<Token>>, LexError> {
        self.advance_to_line_end();
        if self.cursor_is_eof() {
            return Ok(Some(self.lexeme(Token::Eof, start)));
        }

        self.advance_newline();
        self.scan_after_newline()
    }

    /// A comma followed only by whitespace or a comment until the newline.
    fn scan_comma_newline(&mut self, start: Cursor) -> Option<Phrase<Token>> {
        if self.cursor_current() != Some(b',') {
            return None;
        }

        // Only whitespace and a comment may follow the comma
        let mut end = self.cursor.offset + 1;
        while matches!(self.cursor_offset(end), Some(b' ' | b'\t')) {
            end += 1;
        }
        if self.source[end..].starts_with(";;") {
            end = self.find_line_end(end);
        }
        // The newline is consumed with the comma
        if self.cursor_offset(end) != Some(b'\n') {
            return None;
        }

        self.advance_to(end);
        self.advance_newline();
        Some(self.lexeme(Token::CommaNewline, start))
    }

    // - Token-state identifier rules

    /// `_Name`, unless a `(` or `<` follows, which makes it a call.
    fn scan_tag(&mut self, start: Cursor) -> Option<Phrase<Token>> {
        // An underscore followed by an uppercase letter
        if self.cursor_current() != Some(b'_')
            || !self
                .cursor_offset(self.cursor.offset + 1)?
                .is_ascii_uppercase()
        {
            return None;
        }

        let id_start = self.cursor.offset + 1;
        let end = self.find_identifier_end(id_start);
        // `_Name(` and `_Name<` are calls on an identifier, not tags
        if matches!(self.cursor_offset(end), Some(b'(' | b'<')) {
            return None;
        }
        let id = self.source[id_start..end].to_owned();
        self.advance_to(end);
        Some(self.lexeme(Token::TagUpperId(id), start))
    }

    /// `.name`, but not the `...` ellipsis.
    fn scan_dot_identifier(&mut self, start: Cursor) -> Option<Phrase<Token>> {
        // A dot followed by an identifier start
        if self.cursor_current() != Some(b'.')
            || !Self::is_identifier_start(self.cursor_offset(self.cursor.offset + 1)?)
        {
            return None;
        }

        let id_start = self.cursor.offset + 1;
        let end = self.find_identifier_end(id_start);
        // But `...` is the ellipsis token
        if end - start.offset <= 3 && self.source[start.offset..].starts_with("...") {
            return None;
        }

        let id = self.source[id_start..end].to_owned();
        self.advance_to(end);
        Some(self.lexeme(Token::DotId(id), start))
    }

    /// The keyword token for an identifier spelling, if it is one.
    fn keyword(id: &str) -> Option<Token> {
        Some(match id {
            "bool" => Token::Bool,
            "nat" => Token::Nat,
            "int" => Token::Int,
            "text" => Token::Text,
            "syntax" => Token::Syntax,
            "extern" => Token::Extern,
            "tbl" => Token::Table,
            "relation" => Token::Relation,
            "rulegroup" => Token::RuleGroup,
            "rule" => Token::Rule,
            "var" => Token::Var,
            "builtin" => Token::Builtin,
            "dec" => Token::Dec,
            "def" => Token::Def,
            "if" => Token::If,
            "otherwise" => Token::Otherwise,
            "debug" => Token::Debug,
            "eps" => Token::Epsilon,
            "true" => Token::BoolLiteral(true),
            "false" => Token::BoolLiteral(false),
            _ => return None,
        })
    }

    /// An identifier, keyword, or `hint(`, fused with a following `(` or `<`.
    fn scan_identifier(&mut self, start: Cursor) -> Option<Phrase<Token>> {
        let first = self.cursor_current()?;
        if !Self::is_identifier_start(first) {
            return None;
        }

        let is_uppercase = first.is_ascii_uppercase();
        let end = self.find_identifier_end(self.cursor.offset);
        let id = self.source[self.cursor.offset..end].to_owned();
        let suffix = self.cursor_offset(end);
        // `hint(` is one token so hints cannot be confused with calls
        if id == "hint" && suffix == Some(b'(') {
            self.advance_to(end + 1);
            return Some(self.lexeme(Token::HintLeftParen, start));
        }

        // An uppercase name bound as a variable lexes as a lowercase one
        let uppercase_variable = is_uppercase && (self.classify_uppercase)(&id);
        let token = match suffix {
            // Fused call and type-application forms keep the case distinction
            Some(b'(') => {
                self.advance_to(end + 1);
                if is_uppercase && !uppercase_variable {
                    Token::UpperIdLeftParen(id)
                } else {
                    Token::LowerIdLeftParen(id)
                }
            }
            Some(b'<') => {
                self.advance_to(end + 1);
                if is_uppercase && !uppercase_variable {
                    Token::UpperIdLeftAngle(id)
                } else {
                    Token::LowerIdLeftAngle(id)
                }
            }
            // Bare: keyword first, then by case
            _ => {
                self.advance_to(end);
                if let Some(keyword) = Self::keyword(&id) {
                    keyword
                } else if is_uppercase && !uppercase_variable {
                    Token::UpperId(id)
                } else {
                    Token::LowerId(id)
                }
            }
        };

        Some(self.lexeme(token, start))
    }

    // - Token-state numbered holes

    /// `%N` with a decimal index; too large an index is an error.
    fn scan_numbered_hole(&mut self, start: Cursor) -> Result<Option<Phrase<Token>>, LexError> {
        // A percent followed by a digit
        if self.cursor_current() != Some(b'%')
            || !Self::is_digit(
                self.cursor_offset(self.cursor.offset + 1)
                    .unwrap_or_default(),
            )
        {
            return Ok(None);
        }

        // The index must fit a `usize`
        let end = self.find_separated_digits_end(self.cursor.offset + 1, Self::is_digit);
        let digits = Self::strip_underscores(&self.source[self.cursor.offset + 1..end]);
        self.advance_to(end);
        let num = digits
            .parse::<usize>()
            .map_err(|_| error::hole_index_out_of_bounds(self.span(start)))?;
        Ok(Some(self.lexeme(Token::NumberedHole(num), start)))
    }

    // - Token-state Numbers

    /// Parses cleaned digits in the radix.
    fn parse_natural(digits: &str, radix: u32) -> Natural {
        let int = BigInt::parse_bytes(digits.as_bytes(), radix).expect("nonempty digit sequence");
        Natural::try_from(int).expect("digit sequence is non-negative")
    }

    /// A decimal or `0x` hexadecimal natural literal.
    fn scan_number(&mut self, start: Cursor) -> Option<Phrase<Token>> {
        if !Self::is_digit(self.cursor_current()?) {
            return None;
        }

        // Hexadecimal only when a hex digit follows the prefix
        if self.cursor_starts_with("0x")
            && self
                .cursor_offset(self.cursor.offset + 2)
                .is_some_and(Self::is_hex_digit)
        {
            let end = self.find_separated_digits_end(self.cursor.offset + 2, Self::is_hex_digit);
            let digits = Self::strip_underscores(&self.source[self.cursor.offset + 2..end]);
            let nat = Self::parse_natural(&digits, 16);
            self.advance_to(end);
            return Some(self.lexeme(Token::HexLiteral(nat), start));
        }

        // Otherwise decimal
        let end = self.find_separated_digits_end(self.cursor.offset, Self::is_digit);
        let digits = Self::strip_underscores(&self.source[self.cursor.offset..end]);
        let nat = Self::parse_natural(&digits, 10);
        self.advance_to(end);
        Some(self.lexeme(Token::NaturalLiteral(nat), start))
    }

    // - Token-state fixed rules

    /// Fixed punctuation, longest spellings first.
    fn scan_fixed(&mut self, start: Cursor) -> Option<Phrase<Token>> {
        // Three-byte spellings, then two, then one
        let (length, token) = if self.cursor_starts_with("->_") {
            (3, Token::ArrowSub)
        } else if self.cursor_starts_with("=>_") {
            (3, Token::DoubleArrowSub)
        } else if self.cursor_starts_with("<=>") {
            (3, Token::DoubleArrowBoth)
        } else if self.cursor_starts_with("==>") {
            (3, Token::DoubleArrowLong)
        } else if self.cursor_starts_with("~>*") {
            (3, Token::SquigglyArrowStar)
        } else if self.cursor_starts_with("=/=") {
            (3, Token::NotEquals)
        } else if self.cursor_starts_with("%latex") {
            (6, Token::Latex)
        } else if self.cursor_starts_with("`(") {
            (2, Token::TickLeftParen)
        } else if self.cursor_starts_with("`)") {
            (2, Token::TickRightParen)
        } else if self.cursor_starts_with("`[") {
            (2, Token::TickLeftBracket)
        } else if self.cursor_starts_with("`]") {
            (2, Token::TickRightBracket)
        } else if self.cursor_starts_with("`{") {
            (2, Token::TickLeftBrace)
        } else if self.cursor_starts_with("`}") {
            (2, Token::TickRightBrace)
        } else if self.cursor_starts_with("`<") {
            (2, Token::TickLeftAngle)
        } else if self.cursor_starts_with("`>") {
            (2, Token::TickRightAngle)
        } else if self.cursor_starts_with("|-") {
            (2, Token::Turnstile)
        } else if self.cursor_starts_with("-|") {
            (2, Token::Tilesturn)
        } else if self.cursor_starts_with("->") {
            (2, Token::Arrow)
        } else if self.cursor_starts_with("=>") {
            (2, Token::DoubleArrow)
        } else if self.cursor_starts_with("~>") {
            (2, Token::SquigglyArrow)
        } else if self.cursor_starts_with("/\\") {
            (2, Token::And)
        } else if self.cursor_starts_with("\\/") {
            (2, Token::Or)
        } else if self.cursor_starts_with("...") {
            (3, Token::TripleDot)
        } else if self.cursor_starts_with("..") {
            (2, Token::DoubleDot)
        } else if self.cursor_starts_with("::") {
            (2, Token::DoubleColon)
        } else if self.cursor_starts_with(":/") {
            (2, Token::ColonSlash)
        } else if self.cursor_starts_with(":=") {
            (2, Token::ColonEquals)
        } else if self.cursor_starts_with("##") {
            (2, Token::DoubleHash)
        } else if self.cursor_starts_with("<:") {
            (2, Token::Subtype)
        } else if self.cursor_starts_with("~~") {
            (2, Token::DoubleTilde)
        } else if self.cursor_starts_with("<-") {
            (2, Token::LeftAngleDash)
        } else if self.cursor_starts_with("<=") {
            (2, Token::LeftAngleEquals)
        } else if self.cursor_starts_with(">=") {
            (2, Token::RightAngleEquals)
        } else if self.cursor_starts_with(">(") {
            (2, Token::RightAngleLeftParen)
        } else if self.cursor_starts_with("++") {
            (2, Token::DoublePlus)
        } else if self.cursor_starts_with("--") {
            (2, Token::Dash)
        } else if self.cursor_starts_with("%%") {
            (2, Token::MultipleHole)
        } else if self.cursor_starts_with("!%") {
            (2, Token::EmptyHole)
        } else {
            // Single bytes last
            let token = match self.cursor_current()? {
                b'.' => Token::Dot,
                b',' => Token::Comma,
                b';' => Token::Semicolon,
                b':' => Token::Colon,
                b'#' => Token::Hash,
                b'$' => Token::Dollar,
                b'?' => Token::Question,
                b'~' => Token::Tilde,
                b'<' => Token::LeftAngle,
                b'>' => Token::RightAngle,
                b'(' => Token::LeftParen,
                b')' => Token::RightParen,
                b'[' => Token::LeftBracket,
                b']' => Token::RightBracket,
                b'{' => Token::LeftBrace,
                b'}' => Token::RightBrace,
                b'+' => Token::Plus,
                b'-' => Token::Minus,
                b'*' => Token::Star,
                b'/' => Token::Slash,
                b'\\' => Token::Backslash,
                b'%' => Token::Hole,
                b'=' => Token::Equals,
                b'^' => Token::Up,
                b'|' => Token::Bar,
                _ => return None,
            };
            (1, token)
        };

        self.advance_add(length);
        Some(self.lexeme(token, start))
    }

    // - Token-state operator rule

    /// A `'...'` quoted operator; it must close on the same line.
    fn scan_operator(&mut self, start: Cursor) -> Result<Phrase<Token>, LexError> {
        // Everything up to the closing quote is the operator
        let content_start = self.cursor.offset + 1;
        let mut end = content_start;
        while let Some(byte) = self.cursor_offset(end) {
            if byte == b'\'' {
                let op = self.source[content_start..end].to_owned();
                self.advance_to(end + 1);
                return Ok(self.lexeme(Token::Operator(op), start));
            }
            // No closing quote on this line: a malformed token
            if byte == b'\n' {
                break;
            }
            end += 1;
        }

        self.advance_add(1);
        Err(error::character_invalid(self.span(start), '\''))
    }

    // - Text state

    /// A `"..."` text literal, decoding escapes into UTF-8 bytes.
    fn scan_text(&mut self, start: Cursor) -> Result<Phrase<Token>, LexError> {
        self.advance_add(1);
        let mut bytes = Vec::new();
        loop {
            let Some(byte) = self.cursor_current() else {
                return Err(error::text_literal_incomplete(self.span(self.cursor)));
            };
            match byte {
                // Closing quote: the bytes must form valid UTF-8
                b'"' => {
                    self.advance_add(1);
                    let text = String::from_utf8(bytes).map_err(|error_utf8| {
                        error::text_encoding_invalid(self.span(start), &error_utf8)
                    })?;
                    return Ok(self.lexeme(Token::TextLiteral(text), start));
                }
                // A literal cannot span lines
                b'\n' => {
                    return Err(error::text_literal_incomplete(self.span(self.cursor)));
                }
                // Control characters must be escaped
                0x00..=0x1f | 0x7f => {
                    return Err(error::text_character_invalid(
                        self.span(self.cursor),
                        char::from(byte),
                    ));
                }
                // Escape sequence
                b'\\' => self.scan_escape(start, &mut bytes)?,
                // Printable ASCII
                0x20..=0x7e => {
                    bytes.push(byte);
                    self.advance_add(1);
                }
                // Non-ASCII: copy the whole UTF-8 character
                _ => {
                    let character = self.source[self.cursor.offset..]
                        .chars()
                        .next()
                        .expect("non-ASCII byte begins a source character");
                    let end = self.cursor.offset + character.len_utf8();
                    bytes.extend_from_slice(&self.source.as_bytes()[self.cursor.offset..end]);
                    self.advance_to(end);
                }
            }
        }
    }

    /// One escape: a `\n` byte, a `\XX` hex byte, or a `\u{...}` code point.
    fn scan_escape(&mut self, start: Cursor, bytes: &mut Vec<u8>) -> Result<(), LexError> {
        let escape_start = self.cursor.offset;
        // Report a trailing backslash as an incomplete literal at EOF
        let Some(escape) = self.cursor_offset(escape_start + 1) else {
            self.advance_add(1);
            return Err(error::text_literal_incomplete(self.span(self.cursor)));
        };

        // Single-character escapes
        let simple = match escape {
            b'n' => Some(b'\n'),
            b'r' => Some(b'\r'),
            b't' => Some(b'\t'),
            b'\\' => Some(b'\\'),
            b'\'' => Some(b'\''),
            b'"' => Some(b'"'),
            _ => None,
        };
        if let Some(byte) = simple {
            bytes.push(byte);
            self.advance_add(2);
            return Ok(());
        }

        // `\XX` is a raw byte; UTF-8 validity is checked at the closing quote
        if Self::is_hex_digit(escape)
            && self
                .cursor_offset(escape_start + 2)
                .is_some_and(Self::is_hex_digit)
        {
            let digits = &self.source[escape_start + 1..escape_start + 3];
            bytes.push(u8::from_str_radix(digits, 16).expect("two hexadecimal digits"));
            self.advance_add(3);
            return Ok(());
        }

        // `\u{...}` encodes a code point
        if escape == b'u' && self.cursor_offset(escape_start + 2) == Some(b'{') {
            let digits_start = escape_start + 3;
            if self
                .cursor_offset(digits_start)
                .is_some_and(Self::is_hex_digit)
            {
                let digits_end = self.find_separated_digits_end(digits_start, Self::is_hex_digit);
                if self.cursor_offset(digits_end) == Some(b'}') {
                    let digits = Self::strip_underscores(&self.source[digits_start..digits_end]);
                    self.advance_to(digits_end + 1);
                    let character = u32::from_str_radix(&digits, 16)
                        .ok()
                        .and_then(char::from_u32)
                        .ok_or_else(|| {
                            error::text_escape_codepoint_invalid(
                                self.span(Cursor { offset: escape_start, ..start }),
                                &digits.to_uppercase(),
                            )
                        })?;
                    let mut bytes_char = [0; 4];
                    bytes.extend_from_slice(character.encode_utf8(&mut bytes_char).as_bytes());
                    return Ok(());
                }
            }
        }

        // Anything else: report the escape's own span
        let invalid_end = escape_start
            + 1
            + self.source[escape_start + 1..]
                .chars()
                .next()
                .expect("escape byte exists")
                .len_utf8();
        let pos = self.cursor;
        if escape == b'\n' {
            // A rejected escape still advances source positions across the newline
            self.advance_add(1);
            self.advance_newline();
        } else {
            self.advance_to(invalid_end);
        }
        Err(error::text_escape_invalid(self.span(pos), &self.source[escape_start..invalid_end]))
    }

    // - Errors

    /// Classifies a byte no rule accepted and steps over it.
    fn unrecognized_character(&mut self, start: Cursor) -> LexError {
        // The UTF-8 source and cursor movement guarantee a scalar boundary
        let character = self.source[self.cursor.offset..]
            .chars()
            .next()
            .expect("unrecognized character exists");
        self.advance_add(character.len_utf8());
        error::character_invalid(self.span(start), character)
    }
}

impl<Classify> Iterator for Lexer<'_, Classify>
where
    Classify: FnMut(&str) -> bool,
{
    type Item = Result<Phrase<Token>, LexError>;

    // - Iteration

    fn next(&mut self) -> Option<Self::Item> {
        if self.finished {
            return None;
        }

        let result = self.scan_token();
        // The stream ends after `Eof` or the first error
        if match &result {
            Ok(lexeme) => lexeme.node == Token::Eof,
            Err(_) => true,
        } {
            self.finished = true;
        }
        Some(result)
    }
}
