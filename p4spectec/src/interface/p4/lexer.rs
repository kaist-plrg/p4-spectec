//! Lazy context-sensitive tokenization for preprocessed P4
//!
//! `Iterator::next` asks `Lexer::lex` for one token.
//! `Lexer::tokenize` first skips ordinary whitespace and comments,
//! constructs literal values, and recognizes fixed tokens by maximal munch.
//! `lex` emits a name first
//! and its context-sensitive identifier or type-name classification
//! on the following iteration,
//! then distinguishes template angles.
//!
//! The two-part name token is a parser synchronization point:
//! an LR parser may request one token of lookahead
//! before reducing the preceding declaration.
//! Emitting the spelling first
//! lets that reduction update the name-resolution context
//! before the lexer classifies the same spelling.
//!
//! The grammar needs a few one-token lookahead distinctions
//! to remain deterministic.
//! `Lexer::classify_name` distinguishes type names that start expressions,
//! while `Lexer::disambiguate_token` marks block-leading colons,
//! trailing commas,
//! and `error` member accesses without changing their source spelling.
//! `Lexer::disambiguate_angles` splits the scanner's `ShiftRight` token
//! into the source parser's adjacent right-angle tokens.
//!
//! # Examples
//!
//! ```text
//! source: Header<bit<8>> value
//! lexer:  Name("Header"), TypeName, LeftAngleArgs, Bit, LeftAngleArgs,
//!         NumberInt(8), RightAngle, RightAngleShift, Name("value"), Identifier
//!
//! source: x >> 1
//! lexer:  Name("x"), Identifier, RightAngle, RightAngleShift, NumberInt(1)
//! ```

use std::{collections::VecDeque, rc::Rc};

use num_bigint::BigInt;

use crate::lang::{
    common::{
        notation::atom::Atom,
        prim::num::Natural,
        source::{FileId, Phrase, Position, Span},
    },
    data::{
        notation::{self, tree::Mixop},
        typ,
        value::{
            ValueError,
            flat::{self as value, Value, ValueKind},
        },
    },
};

use crate::phrase;

use super::{
    context::{Context, IdentKind},
    error::{LexErrorKind, P4Error},
};

// == Tokens

/// A token consumed by the P4 grammar.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Token {
    End,
    TypeName,
    /// Type name at the start of a postfix expression.
    TypeNameExpression,
    Identifier,
    Name(Value),
    StringLiteral(Value),
    NumberInt(Value, String),
    Number(Value, String),
    LessEqual,
    GreaterEqual,
    ShiftLeft,
    And,
    Or,
    NotEqual,
    Equal,
    /// Scanner-only spelling of adjacent `>` characters.
    ShiftRight,
    Plus,
    Minus,
    PlusSaturating,
    MinusSaturating,
    Multiply,
    Invalid,
    Divide,
    Modulo,
    BitOr,
    BitAnd,
    BitXor,
    Complement,
    LeftBracket,
    RightBracket,
    LeftBrace,
    RightBrace,
    LeftAngle,
    LeftAngleArgs,
    RightAngle,
    RightAngleShift,
    LeftParen,
    RightParen,
    Assign,
    Colon,
    /// Parser-only colon immediately before a block or annotation.
    BlockColon,
    Comma,
    /// Parser-only comma immediately before `}` or `]`
    TrailingComma,
    Question,
    Dot,
    Not,
    Semicolon,
    At,
    PlusPlus,
    PlusColon,
    DontCare,
    Mask,
    Dots,
    Range,
    True,
    False,
    Abstract,
    Action,
    Actions,
    Apply,
    Bool,
    Bit,
    Break,
    Const,
    Continue,
    Control,
    Default,
    Else,
    Entries,
    Enum,
    Error,
    /// Parser-only `error` immediately before member access.
    ErrorExpression,
    Exit,
    Extern,
    Header,
    HeaderUnion,
    If,
    In,
    InOut,
    For,
    Int,
    Key,
    List,
    Select,
    MatchKind,
    Out,
    Package,
    Parser,
    Priority,
    Return,
    State,
    String,
    Struct,
    Switch,
    Table,
    This,
    Transition,
    Tuple,
    Typedef,
    Type,
    ValueSet,
    Varbit,
    Void,
    Pragma,
    PragmaEnd,
    PlusAssign,
    PlusSaturatingAssign,
    MinusAssign,
    MinusSaturatingAssign,
    MultiplyAssign,
    DivideAssign,
    ModuloAssign,
    ShiftLeftAssign,
    ShiftRightAssign,
    BitAndAssign,
    BitXorAssign,
    BitOrAssign,
    UnexpectedToken(Value),
}

/// What the lexer expects next.
#[derive(Clone, Copy, Debug)]
enum LexerState {
    /// Ordinary tokens.
    Regular,
    /// Inside `@pragma ...`, which a newline ends.
    Pragma,
    /// After a type that may take `<...>` arguments.
    Template,
}

// == Lexer

/// A lazy P4 token stream sharing the parser's name-resolution context.
pub struct Lexer<'source, 'arena> {
    /// The preprocessed source.
    source: &'source str,
    /// Byte offset of the next character.
    index: usize,
    /// Current file, as set by `#` line markers.
    file: FileId,
    /// Current line, as set by `#` line markers.
    line: usize,
    /// Byte column on the current line.
    column: usize,
    /// Name-resolution state shared with the parser.
    ctx: Rc<Context<'arena>>,
    /// What is expected next.
    state: LexerState,
    /// Tokens split off an earlier one, emitted before scanning on.
    pending: VecDeque<Phrase<Token>>,
    /// A name whose classification token is emitted on the next call.
    deferred_classification: Option<(Value, Span, LexerState)>,
    /// Open template angle brackets.
    template_depth: usize,
    /// Set once `End` was emitted.
    finished: bool,
}

// - Construction

impl<'source, 'arena> Lexer<'source, 'arena> {
    /// Tokenizes preprocessed `source` using context-sensitive name classes.
    pub fn new(ctx: Rc<Context<'arena>>, file: FileId, source: &'source str) -> Self {
        Self {
            source,
            index: 0,
            file,
            line: 1,
            column: 0,
            ctx,
            state: LexerState::Regular,
            pending: VecDeque::new(),
            deferred_classification: None,
            template_depth: 0,
            finished: false,
        }
    }

    // - Source cursor

    /// Returns the next character's position, rejecting unrepresentable coordinates.
    fn source_position(&self) -> Result<Position, P4Error> {
        // An out-of-range position can only retain its logical file name
        let (Ok(line), Ok(column)) = (u32::try_from(self.line), u32::try_from(self.column)) else {
            let pos = Position::new(self.file, 0, 0);
            return Err(P4Error::new(Span::new(pos, pos), LexErrorKind::PositionOutOfRange));
        };
        Ok(Position { file: self.file, line, column })
    }

    /// The span from `pos_l` to the next character.
    fn span_from(&self, pos_l: Position) -> Result<Span, P4Error> {
        Ok(Span::new(pos_l, self.source_position()?))
    }

    /// An error spanning from `pos_l` to here.
    fn error(&self, kind: LexErrorKind, pos_l: Position) -> P4Error {
        match self.span_from(pos_l) {
            Ok(span) => P4Error::new(span, kind),
            Err(error) => error,
        }
    }

    /// Consumes one character, tracking line and column.
    fn bump(&mut self) -> Option<char> {
        let character = self.source[self.index..].chars().next()?;
        self.index += character.len_utf8();
        if character == '\n' {
            self.line += 1;
            self.column = 0;
        } else {
            self.column += character.len_utf8();
        }
        Some(character)
    }

    /// Consumes characters while the predicate holds.
    fn take_while(&mut self, mut predicate: impl FnMut(char) -> bool) -> &'source str {
        let start = self.index;
        while let Some(character) = self.source[self.index..].chars().next() {
            if !predicate(character) {
                break;
            }
            self.bump();
        }
        &self.source[start..self.index]
    }

    // - Token emission

    /// Produces the next parser token, running the state machine.
    fn lex(&mut self) -> Option<Result<Phrase<Token>, P4Error>> {
        // A name's classification comes right after the name itself
        if let Some((value, span, next)) = self.deferred_classification.take() {
            return Some(Ok(self.classify_name(&value, &span, next)));
        }
        // Then anything split off an earlier token
        if let Some(token) = self.pending.pop_front() {
            return Some(Ok(token));
        }
        loop {
            let token = match self.tokenize().transpose()? {
                Ok(token) => token,
                Err(error) => return Some(Err(error)),
            };
            let span = token.span;
            // Contextual respellings first, then the state machine
            let token = self.disambiguate_token(token.node);
            let token = match self.state {
                // Regular: names defer; pragmas and template types switch state
                LexerState::Regular => match token {
                    Token::Name(value) => {
                        self.defer_classification(&value, &span, LexerState::Regular);
                        Token::Name(value)
                    }
                    Token::Pragma => {
                        self.state = LexerState::Pragma;
                        Token::Pragma
                    }
                    token @ (Token::Bit
                    | Token::Int
                    | Token::Varbit
                    | Token::List
                    | Token::Tuple
                    | Token::ValueSet) => {
                        self.state = LexerState::Template;
                        token
                    }
                    // Newlines mean nothing outside a pragma
                    Token::PragmaEnd => continue,
                    token => token,
                },
                // Pragma: a newline ends it; names still classify
                LexerState::Pragma => match token {
                    Token::PragmaEnd => {
                        self.state = LexerState::Regular;
                        Token::PragmaEnd
                    }
                    Token::Name(value) => {
                        self.defer_classification(&value, &span, LexerState::Pragma);
                        Token::Name(value)
                    }
                    token => token,
                },
                // Template: `<` opens arguments; anything else is regular
                LexerState::Template => match token {
                    Token::LeftAngle | Token::LeftAngleArgs => {
                        self.state = LexerState::Regular;
                        Token::LeftAngleArgs
                    }
                    Token::Name(value) => {
                        self.defer_classification(&value, &span, LexerState::Regular);
                        Token::Name(value)
                    }
                    Token::Pragma => {
                        self.state = LexerState::Pragma;
                        Token::Pragma
                    }
                    Token::PragmaEnd => continue,
                    token => {
                        self.state = LexerState::Regular;
                        token
                    }
                },
            };
            // Finally split `>>` inside templates
            let (token, span) = self.disambiguate_angles(token, span);
            return Some(Ok(phrase!(node: token, span: span)));
        }
    }

    // - Template angle disambiguation

    /// Tracks template depth and splits `>>` into two right angles inside one.
    fn disambiguate_angles(&mut self, token: Token, span: Span) -> (Token, Span) {
        match token {
            Token::LeftAngleArgs => {
                self.template_depth += 1;
                (Token::LeftAngleArgs, span)
            }
            // A `>` closes a template when one is open
            Token::RightAngle if self.template_depth > 0 => {
                self.template_depth -= 1;
                (Token::RightAngle, span)
            }
            // `>>`: the first `>` closes; the second closes or shifts, by depth
            Token::ShiftRight => {
                let pos_middle = Position { column: span.left.column + 1, ..span.left };
                let token_r = if self.template_depth > 1 {
                    self.template_depth -= 2;
                    Token::RightAngleShift
                } else if self.template_depth == 1 {
                    self.template_depth = 0;
                    Token::RightAngle
                } else {
                    Token::RightAngleShift
                };
                let span_r = Span::new(pos_middle, span.right);
                let lexeme_r = phrase!(node: token_r, span: span_r);
                self.pending.push_back(lexeme_r);
                let span_l = Span::new(span.left, pos_middle);
                (Token::RightAngle, span_l)
            }
            token => (token, span),
        }
    }

    // - Contextual token disambiguation

    /// Respells `,`, `error`, and `:` by what follows, so the grammar is LR(1).
    fn disambiguate_token(&mut self, mut token: Token) -> Token {
        // A comma before a closing bracket is trailing
        if token == Token::Comma && self.remaining_significant().starts_with(['}', ']']) {
            token = Token::TrailingComma;
        } else if token == Token::Error && self.remaining_significant().starts_with('.') {
            token = Token::ErrorExpression;
        }
        // A colon before a block or annotation is a label colon
        if token == Token::Colon
            && (self.remaining_significant().starts_with(['{', '@'])
                || self.remaining_significant().starts_with("#pragma"))
        {
            token = Token::BlockColon;
        }
        token
    }

    // - Name classification

    /// Schedules a name's classification for the next call.
    fn defer_classification(&mut self, value: &Value, span: &Span, next: LexerState) {
        self.deferred_classification = Some((*value, *span, next));
    }

    /// Classifies a name as type or identifier
    /// and decides whether template arguments follow.
    fn classify_name(&mut self, value: &Value, span: &Span, next: LexerState) -> Phrase<Token> {
        let arena = self.ctx.arena();
        let name = match arena.value.kind(value.node) {
            ValueKind::Text(name) => name,
            _ => return phrase!(node: Token::Identifier, span: *span),
        };
        // Type names may start an expression; either kind may take `<...>`
        let (token, template_expected) = match self.ctx.ident_kind(name) {
            IdentKind::TypeName { has_params, .. } => {
                let token = if self.type_name_starts_expression() {
                    Token::TypeNameExpression
                } else {
                    Token::TypeName
                };
                let template_expected = has_params
                    || self.remaining_significant().starts_with('<')
                    || self.template_arguments_follow();
                (token, template_expected)
            }
            IdentKind::Ident { has_params, .. } => {
                (Token::Identifier, has_params || self.call_type_arguments_follow())
            }
        };
        // Expecting a template overrides the state the caller asked for
        self.state = if template_expected { LexerState::Template } else { next };
        phrase!(node: token, span: *span)
    }

    /// Whether a type name starts an expression: `T.x`, `T(...)`, `T<..>(...)`.
    fn type_name_starts_expression(&self) -> bool {
        let rest = self.remaining_significant();
        rest.starts_with(['.', '(']) || self.template_arguments_follow()
    }

    /// Whether a balanced `<...>` followed by `(` comes next.
    fn template_arguments_follow(&self) -> bool {
        angle_suffix(self.remaining_significant())
            .is_some_and(|suffix| significant(suffix).starts_with('('))
    }

    /// Whether `<` after an identifier starts type arguments, not a comparison.
    fn call_type_arguments_follow(&self) -> bool {
        if !self.template_arguments_follow() {
            return false;
        }
        // Immediately adjacent `<` is always type arguments
        if self.source[self.index..].starts_with('<') {
            return true;
        }
        // Otherwise the first argument must look like a type
        let rest = significant(&self.remaining_significant()[1..]);
        let len = rest
            .bytes()
            .take_while(|byte| byte.is_ascii_alphanumeric() || *byte == b'_')
            .count();
        let name = &rest[..len];
        // A real type argument cannot be the RHS of a relational expression
        if significant(&rest[len..]).starts_with('.') {
            return false;
        }
        matches!(
            keyword(name),
            Some(
                Token::Bool
                    | Token::Bit
                    | Token::Int
                    | Token::Varbit
                    | Token::String
                    | Token::Error
                    | Token::MatchKind
                    | Token::List
                    | Token::Tuple
                    | Token::Void
                    | Token::DontCare
            )
        ) || matches!(self.ctx.ident_find(name), Some(IdentKind::TypeName { .. }))
    }

    /// The source after the cursor, skipping whitespace and comments.
    fn remaining_significant(&self) -> &str {
        significant(&self.source[self.index..])
    }

    // - Raw scanning

    /// Scans one raw token, or `End` once at the end.
    fn tokenize(&mut self) -> Result<Option<Phrase<Token>>, P4Error> {
        loop {
            // `End` is emitted exactly once
            if self.index == self.source.len() {
                if self.finished {
                    return Ok(None);
                }
                self.finished = true;
                let position = self.source_position()?;
                let span = Span::new(position, position);
                return Ok(Some(phrase!(node: Token::End, span: span)));
            }
            let pos_l = self.source_position()?;
            let rest = &self.source[self.index..];

            // A block comment spanning lines counts as a newline, for pragmas
            if rest.starts_with("/*") {
                self.bump();
                self.bump();
                let mut crossed_newline = false;
                while self.index < self.source.len() && !self.source[self.index..].starts_with("*/")
                {
                    crossed_newline |= self.bump() == Some('\n');
                }
                if self.index == self.source.len() {
                    return Err(self.error(LexErrorKind::CommentUnterminated, pos_l));
                }
                self.bump();
                self.bump();
                if crossed_newline {
                    let span = self.span_from(pos_l)?;
                    return Ok(Some(phrase!(node: Token::PragmaEnd, span: span)));
                }
                continue;
            }
            // Line comments are skipped
            if rest.starts_with("//") {
                self.take_while(|character| character != '\n');
                continue;
            }
            // Newlines are tokens so pragmas can end
            if rest.starts_with('\n') {
                self.bump();
                let span = self.span_from(pos_l)?;
                return Ok(Some(phrase!(node: Token::PragmaEnd, span: span)));
            }
            // String literal
            if rest.starts_with('"') {
                return self.string_token(pos_l).map(Some);
            }
            // Other whitespace is skipped
            if rest.starts_with([' ', '\t', '\u{000c}', '\r']) {
                self.take_while(|character| matches!(character, ' ' | '\t' | '\u{000c}' | '\r'));
                continue;
            }
            // Preprocessor line markers update the file and line
            if rest.starts_with('#') {
                self.preprocessor_line();
                continue;
            }
            // `@pragma` starts a pragma
            if rest.starts_with("@pragma") {
                for _ in 0.."@pragma".len() {
                    self.bump();
                }
                let span = self.span_from(pos_l)?;
                return Ok(Some(phrase!(node: Token::Pragma, span: span)));
            }
            // Number literal
            if rest.as_bytes()[0].is_ascii_digit() {
                return self.number_token(pos_l).map(Some);
            }
            // Keyword or name; names are interned as text values
            if is_name_start(rest.as_bytes()[0]) {
                let start = self.index;
                self.bump();
                self.take_while(|character| character.is_ascii_alphanumeric() || character == '_');
                let text = &self.source[start..self.index];
                let span = self.span_from(pos_l)?;
                let token = match keyword(text) {
                    Some(token) => token,
                    None => {
                        match value::make::text(&mut self.ctx.arena_mut(), text.to_owned(), span) {
                            Ok(value) => Token::Name(value),
                            Err(error) => return Err(P4Error::new(span, error)),
                        }
                    }
                };
                return Ok(Some(phrase!(node: token, span: span)));
            }

            // Punctuation by longest match
            if let Some((spelling, token)) = fixed_token(rest) {
                for _ in 0..spelling.len() {
                    self.bump();
                }
                let span = self.span_from(pos_l)?;
                return Ok(Some(phrase!(node: token, span: span)));
            }

            // Anything else is passed to the parser as an unexpected token
            let text = self.bump().expect("source is not empty").to_string();
            let span = self.span_from(pos_l)?;
            let value = match value::make::text(&mut self.ctx.arena_mut(), text, span) {
                Ok(value) => value,
                Err(error) => return Err(P4Error::new(span, error)),
            };
            let token = Token::UnexpectedToken(value);
            return Ok(Some(phrase!(node: token, span: span)));
        }
    }

    // - String literals

    /// A `"..."` literal with `\"`, `\n`, and `\\` escapes.
    fn string_token(&mut self, pos_l: Position) -> Result<Phrase<Token>, P4Error> {
        // Read up to the closing quote, decoding escapes
        self.bump();
        let mut text = String::new();
        let pos_quote = loop {
            let pos_char = self.source_position()?;
            // The source may not end inside the literal
            let Some(character) = self.bump() else {
                return Err(self.error(LexErrorKind::StringUnterminated, pos_char));
            };
            match character {
                '"' => break pos_char,
                // Only `\"`, `\n`, and `\\` are escapes
                '\\' => {
                    let Some(escaped) = self.bump() else {
                        return Err(
                            self.error(LexErrorKind::StringUnterminated, self.source_position()?)
                        );
                    };
                    match escaped {
                        '"' => text.push('"'),
                        'n' => text.push('\n'),
                        '\\' => text.push('\\'),
                        escaped => {
                            return Err(self.error(
                                LexErrorKind::EscapeUnsupported(format!("\\{escaped}")),
                                pos_char,
                            ));
                        }
                    }
                }
                character => text.push(character),
            }
        };
        // The value spans the literal; the token ends at the closing quote
        let value = value::make::text(&mut self.ctx.arena_mut(), text, self.span_from(pos_l)?)?;
        let token = Token::StringLiteral(value);
        let span = self.span_from(pos_quote)?;
        Ok(phrase!(node: token, span: span))
    }

    // - Integer literals

    /// An integer literal, plain or sized as `WIDTHwDIGITS` / `WIDTHsDIGITS`.
    fn number_token(&mut self, pos_l: Position) -> Result<Phrase<Token>, P4Error> {
        let start = self.index;
        let rest = &self.source[start..];
        let int_len = integer_lexeme_len(rest);
        let sized = sized_integer_lexeme(rest);
        // Prefer the sized form when it extends past the plain integer
        let (lexeme_len, sign_index) = match sized {
            Some((sized_len, sign_index)) if sized_len > int_len => (sized_len, Some(sign_index)),
            _ => (int_len, None),
        };
        for _ in 0..lexeme_len {
            self.bump();
        }
        let spelling = &self.source[start..self.index];
        let (value, lexeme) = match sign_index {
            // Sized: `integerLiteral` case of width, `W` or `S` atom, and value
            Some(index) => {
                let width = &spelling[..index];
                let sign = spelling.as_bytes()[index] as char;
                let digits = &spelling[index + 1..];
                let int = parse_integer(digits).ok_or_else(|| {
                    self.error(LexErrorKind::IntegerInvalid(spelling.to_owned()), pos_l)
                })?;
                let int_width = parse_integer(width).ok_or_else(|| {
                    self.error(LexErrorKind::IntegerInvalid(spelling.to_owned()), pos_l)
                })?;
                // A signed literal needs a sign bit and a value bit
                if sign == 's' && int_width < BigInt::from(2) {
                    return Err(self.error(LexErrorKind::SignedWidthInvalid, pos_l));
                }
                let span = self.span_from(pos_l)?;
                let nat_width = Natural::try_from(int_width).map_err(|_| {
                    self.error(LexErrorKind::IntegerInvalid(spelling.to_owned()), pos_l)
                })?;
                let value_width = value::make::nat(&mut self.ctx.arena_mut(), nat_width, span)?;
                let value_int = value::make::int(&mut self.ctx.arena_mut(), int, span)?;
                let atom = phrase!(
                    node: Atom::Keyword(sign.to_ascii_uppercase().to_string()),
                    span: span
                );
                // Each literal's atom has its own span, so its shape is not shared
                let mixop = Mixop::Seq(vec![Mixop::Arg, Mixop::Atom(atom), Mixop::Arg]);
                let mut arena = self.ctx.arena_mut();
                let arena_mixop = &mut arena.mixop;
                let mixop = arena_mixop.intern(&mixop).map_err(ValueError::from)?;
                let value_case =
                    notation::flat::Mixfix::new(arena_mixop, mixop, vec![value_width, value_int])
                        .expect("the literal shape has two positions");
                let id_typ = phrase!(node: "integerLiteral".into(), span: Span::default());
                let value = Value::new(
                    &mut arena,
                    ValueKind::Case(value_case),
                    (typ::make::var(id_typ, vec![])).node.into(),
                    span,
                )?;
                (value, digits.to_owned())
            }
            // Plain: an integer value
            _ => {
                let int = parse_integer(spelling).ok_or_else(|| {
                    self.error(LexErrorKind::IntegerInvalid(spelling.to_owned()), pos_l)
                })?;
                let span = self.span_from(pos_l)?;
                (value::make::int(&mut self.ctx.arena_mut(), int, span)?, spelling.to_owned())
            }
        };
        let span = self.span_from(pos_l)?;
        let token = if sign_index.is_some() {
            Token::Number(value, lexeme)
        } else {
            Token::NumberInt(value, lexeme)
        };
        Ok(phrase!(node: token, span: span))
    }

    // - Preprocessor line markers

    /// Reads a `# line "file"` marker and resets the current position.
    fn preprocessor_line(&mut self) {
        self.take_while(|character| character != '\n');
        let text_line = self.source[..self.index]
            .rsplit_once('\n')
            .map_or(&self.source[..self.index], |(_, line)| line);
        let text_directive = &text_line[1..];
        let text_before_path = text_directive
            .split_once('"')
            .map_or(text_directive, |(before, _)| before);
        // The line number is the last number before the quoted path
        let line = text_before_path
            .split(|character: char| !character.is_ascii_digit() && character != '_')
            .filter_map(|text| text.replace('_', "").parse::<usize>().ok())
            .next_back();
        if let Some(line) = line {
            self.line = line;
        }
        // The quoted path, if any, becomes the current file
        if let Some(quote_l) = text_directive.find('"') {
            let path = &text_directive[quote_l + 1..];
            if let Some(quote_r) = path.find('"') {
                self.file = FileId::intern(&path[..quote_r]);
            }
        }
        if self.index < self.source.len() {
            self.index += 1;
            self.column = 0;
        }
    }
}

// == Token lookahead

/// Skips leading whitespace and comments.
fn significant(mut rest: &str) -> &str {
    loop {
        // Whitespace, then a line or block comment, repeatedly
        rest = rest.trim_start();
        if let Some(after) = rest.strip_prefix("//") {
            rest = after.find('\n').map_or("", |idx| &after[idx + 1..]);
        } else if let Some(after) = rest.strip_prefix("/*") {
            rest = after.find("*/").map_or("", |idx| &after[idx + 2..]);
        } else {
            return rest;
        }
    }
}

/// The text after a balanced `<...>` at the start, if there is one.
fn angle_suffix(rest: &str) -> Option<&str> {
    if !rest.starts_with('<') {
        return None;
    }
    // Count nesting until the opening `<` is closed
    let mut depth = 0usize;
    for (index, character) in rest.char_indices() {
        match character {
            '<' => depth += 1,
            '>' => {
                depth = depth.checked_sub(1)?;
                if depth == 0 {
                    return Some(&rest[index + character.len_utf8()..]);
                }
            }
            _ => {}
        }
    }
    None
}

impl Iterator for Lexer<'_, '_> {
    type Item = Result<Phrase<Token>, P4Error>;

    fn next(&mut self) -> Option<Self::Item> {
        let token = self.lex()?;
        Some(token)
    }
}

// == Integer literal helpers

/// The length of the integer literal at the start, honoring radix prefixes.
fn integer_lexeme_len(rest: &str) -> usize {
    let bytes = rest.as_bytes();
    if !bytes.first().is_some_and(u8::is_ascii_digit) {
        return 0;
    }

    // Plain decimal digits first
    let int_len = bytes
        .iter()
        .take_while(|byte| byte.is_ascii_digit() || **byte == b'_')
        .count();
    let mut len = int_len;
    // A radix prefix with at least one digit may extend the match
    for (prefix, valid_digit) in [
        ("0x", is_hex_digit as fn(u8) -> bool),
        ("0X", is_hex_digit),
        ("0d", is_decimal_digit),
        ("0D", is_decimal_digit),
        ("0o", is_octal_digit),
        ("0O", is_octal_digit),
        ("0b", is_binary_digit),
        ("0B", is_binary_digit),
    ] {
        let Some(digits) = rest.strip_prefix(prefix) else {
            continue;
        };
        let digits_len = digits
            .bytes()
            .take_while(|byte| valid_digit(*byte) || *byte == b'_')
            .count();
        if digits_len > 0 {
            len = len.max(prefix.len() + digits_len);
        }
    }
    len
}

/// The length and sign-letter offset of a `WIDTHw...` or `WIDTHs...` literal.
fn sized_integer_lexeme(rest: &str) -> Option<(usize, usize)> {
    let sign_index = rest.bytes().take_while(u8::is_ascii_digit).count();
    let sign = *rest.as_bytes().get(sign_index)?;
    if !matches!(sign, b'w' | b's') {
        return None;
    }
    let integer_len = integer_lexeme_len(&rest[sign_index + 1..]);
    (integer_len > 0).then_some((sign_index + 1 + integer_len, sign_index))
}

/// Hexadecimal digit.
fn is_hex_digit(byte: u8) -> bool {
    byte.is_ascii_hexdigit()
}

/// Decimal digit.
fn is_decimal_digit(byte: u8) -> bool {
    byte.is_ascii_digit()
}

/// Octal digit.
fn is_octal_digit(byte: u8) -> bool {
    matches!(byte, b'0'..=b'7')
}

/// Binary digit.
fn is_binary_digit(byte: u8) -> bool {
    matches!(byte, b'0' | b'1')
}

/// Parses an integer literal in any radix, ignoring `_` separators.
fn parse_integer(spelling: &str) -> Option<BigInt> {
    // Drop separators, pick the radix by prefix, then parse the digits
    let sanitized = spelling.replace('_', "");
    let (radix, digits) = if let Some(digits) = sanitized
        .strip_prefix("0x")
        .or_else(|| sanitized.strip_prefix("0X"))
    {
        (16, digits)
    } else if let Some(digits) = sanitized
        .strip_prefix("0o")
        .or_else(|| sanitized.strip_prefix("0O"))
    {
        (8, digits)
    } else if let Some(digits) = sanitized
        .strip_prefix("0b")
        .or_else(|| sanitized.strip_prefix("0B"))
    {
        (2, digits)
    } else if let Some(digits) = sanitized
        .strip_prefix("0d")
        .or_else(|| sanitized.strip_prefix("0D"))
    {
        (10, digits)
    } else {
        (10, sanitized.as_str())
    };
    BigInt::parse_bytes(digits.as_bytes(), radix)
}

// == Names and keywords

/// Whether a byte can begin a name.
fn is_name_start(byte: u8) -> bool {
    byte.is_ascii_alphabetic() || byte == b'_'
}

/// The keyword token for a spelling, if it is one.
fn keyword(text: &str) -> Option<Token> {
    Some(match text {
        "abstract" => Token::Abstract,
        "action" => Token::Action,
        "actions" => Token::Actions,
        "apply" => Token::Apply,
        "bool" => Token::Bool,
        "bit" => Token::Bit,
        "break" => Token::Break,
        "const" => Token::Const,
        "continue" => Token::Continue,
        "control" => Token::Control,
        "default" => Token::Default,
        "else" => Token::Else,
        "entries" => Token::Entries,
        "enum" => Token::Enum,
        "error" => Token::Error,
        "exit" => Token::Exit,
        "extern" => Token::Extern,
        "header" => Token::Header,
        "header_union" => Token::HeaderUnion,
        "true" => Token::True,
        "false" => Token::False,
        "for" => Token::For,
        "if" => Token::If,
        "in" => Token::In,
        "inout" => Token::InOut,
        "int" => Token::Int,
        "key" => Token::Key,
        "list" => Token::List,
        "match_kind" => Token::MatchKind,
        "out" => Token::Out,
        "parser" => Token::Parser,
        "package" => Token::Package,
        "pragma" => Token::Pragma,
        "priority" => Token::Priority,
        "return" => Token::Return,
        "select" => Token::Select,
        "state" => Token::State,
        "string" => Token::String,
        "struct" => Token::Struct,
        "switch" => Token::Switch,
        "table" => Token::Table,
        "this" => Token::This,
        "transition" => Token::Transition,
        "tuple" => Token::Tuple,
        "typedef" => Token::Typedef,
        "type" => Token::Type,
        "value_set" => Token::ValueSet,
        "varbit" => Token::Varbit,
        "void" => Token::Void,
        "_" => Token::DontCare,
        _ => return None,
    })
}

// == Fixed tokens

/// The longest fixed token at the start of `rest`.
fn fixed_token(rest: &str) -> Option<(&'static str, Token)> {
    // Longer prefixes precede their shorter alternatives
    Some(match rest.as_bytes() {
        [b'|', b'+', b'|', b'=', ..] => ("|+|=", Token::PlusSaturatingAssign),
        [b'|', b'-', b'|', b'=', ..] => ("|-|=", Token::MinusSaturatingAssign),
        [b'|', b'+', b'|', ..] => ("|+|", Token::PlusSaturating),
        [b'|', b'-', b'|', ..] => ("|-|", Token::MinusSaturating),
        [b'{', b'#', b'}', ..] => ("{#}", Token::Invalid),
        [b'&', b'&', b'&', ..] => ("&&&", Token::Mask),
        [b'.', b'.', b'.', ..] => ("...", Token::Dots),
        [b'<', b'<', b'=', ..] => ("<<=", Token::ShiftLeftAssign),
        [b'>', b'>', b'=', ..] => (">>=", Token::ShiftRightAssign),
        [b'<', b'=', ..] => ("<=", Token::LessEqual),
        [b'>', b'=', ..] => (">=", Token::GreaterEqual),
        [b'<', b'<', ..] => ("<<", Token::ShiftLeft),
        // LALRPOP's precedence grammar requires one expression token
        [b'>', b'>', ..] => (">>", Token::ShiftRight),
        [b'&', b'&', ..] => ("&&", Token::And),
        [b'|', b'|', ..] => ("||", Token::Or),
        [b'!', b'=', ..] => ("!=", Token::NotEqual),
        [b'=', b'=', ..] => ("==", Token::Equal),
        [b'+', b':', ..] => ("+:", Token::PlusColon),
        [b'+', b'+', ..] => ("++", Token::PlusPlus),
        [b'.', b'.', ..] => ("..", Token::Range),
        [b'+', b'=', ..] => ("+=", Token::PlusAssign),
        [b'-', b'=', ..] => ("-=", Token::MinusAssign),
        [b'*', b'=', ..] => ("*=", Token::MultiplyAssign),
        [b'/', b'=', ..] => ("/=", Token::DivideAssign),
        [b'%', b'=', ..] => ("%=", Token::ModuloAssign),
        [b'&', b'=', ..] => ("&=", Token::BitAndAssign),
        [b'^', b'=', ..] => ("^=", Token::BitXorAssign),
        [b'|', b'=', ..] => ("|=", Token::BitOrAssign),
        [b'+', ..] => ("+", Token::Plus),
        [b'-', ..] => ("-", Token::Minus),
        [b'*', ..] => ("*", Token::Multiply),
        [b'/', ..] => ("/", Token::Divide),
        [b'%', ..] => ("%", Token::Modulo),
        [b'|', ..] => ("|", Token::BitOr),
        [b'&', ..] => ("&", Token::BitAnd),
        [b'^', ..] => ("^", Token::BitXor),
        [b'~', ..] => ("~", Token::Complement),
        [b'[', ..] => ("[", Token::LeftBracket),
        [b']', ..] => ("]", Token::RightBracket),
        [b'{', ..] => ("{", Token::LeftBrace),
        [b'}', ..] => ("}", Token::RightBrace),
        [b'<', ..] => ("<", Token::LeftAngle),
        [b'>', ..] => (">", Token::RightAngle),
        [b'(', ..] => ("(", Token::LeftParen),
        [b')', ..] => (")", Token::RightParen),
        [b'!', ..] => ("!", Token::Not),
        [b':', ..] => (":", Token::Colon),
        [b',', ..] => (",", Token::Comma),
        [b'?', ..] => ("?", Token::Question),
        [b'.', ..] => (".", Token::Dot),
        [b'=', ..] => ("=", Token::Assign),
        [b';', ..] => (";", Token::Semicolon),
        [b'@', ..] => ("@", Token::At),
        _ => return None,
    })
}
