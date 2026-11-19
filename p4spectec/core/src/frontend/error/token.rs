//! Token descriptions and parser expectation diagnostics
//!
//! Source spans retain actual token spellings.
//! LALRPOP terminal names become readable expected alternatives.
//! Token and end-of-input reports use these descriptions in their labels.

use crate::lang::common::source::{Position, Span};

use crate::diagnostic::Label;

use crate::frontend::lexer::Token;

use super::{FrontendError, diagnostic};

// = Helpers

/// Uses the lexeme's source spelling, including payloads and punctuation.
pub(crate) fn describe_token(source: &str, span: &Span, token: &Token) -> String {
    // Layout and parser-inserted tokens can have zero-width source spans
    match token {
        Token::Sequence => return "adjacent token".to_owned(),
        Token::Eof => return "end of input".to_owned(),
        Token::NewlineBar => return "newline followed by `|`".to_owned(),
        Token::Newline2 => return "blank line".to_owned(),
        Token::Newline3 => return "two blank lines".to_owned(),
        _ => {}
    }

    // Positions retain byte columns, so source spelling needs no decoding
    let offset = |pos: &Position| {
        source
            .split_inclusive('\n')
            .take((pos.line as usize).saturating_sub(1))
            .map(str::len)
            .sum::<usize>()
            + pos.column as usize
    };
    let text = &source[offset(&span.left)..offset(&span.right)];
    format!("token {text:?}")
}

/// Translates LALRPOP terminal names into the frontend's visible vocabulary.
fn terminal_presentation(terminal: &str) -> (&str, &str) {
    match terminal {
        "TAG_UPID" => ("an identifier", "a tagged identifier"),
        "OPERATOR" => ("an operator", "a quoted operator"),
        "TICK_LPAREN" => ("a delimiter", "``(`"),
        "TICK_RPAREN" => ("a delimiter", "``)`"),
        "TICK_LBRACK" => ("a delimiter", "``[`"),
        "TICK_RBRACK" => ("a delimiter", "``]`"),
        "TICK_LBRACE" => ("a delimiter", "``{`"),
        "TICK_RBRACE" => ("a delimiter", "``}`"),
        "TICK_LANGLE" => ("a delimiter", "``<`"),
        "TICK_RANGLE" => ("a delimiter", "``>`"),
        "NL_BAR" => ("a line break", "a newline followed by `|`"),
        "NL2" => ("a line break", "a blank line"),
        "NL3" => ("a line break", "two blank lines"),
        "SEQ" => ("an adjacent token", "an adjacent token"),
        "SUB" => ("an operator or separator", "`<:`"),
        "TURNSTILE" => ("an operator or separator", "`|-`"),
        "TILESTURN" => ("an operator or separator", "`-|`"),
        "ARROW" => ("an operator or separator", "`->`"),
        "ARROW_SUB" => ("an operator or separator", "`->_`"),
        "DOUBLE_ARROW" => ("an operator or separator", "`=>`"),
        "DOUBLE_ARROW_SUB" => ("an operator or separator", "`=>_`"),
        "DOUBLE_ARROW_BOTH" => ("an operator or separator", "`<=>`"),
        "DOUBLE_ARROW_LONG" => ("an operator or separator", "`==>`"),
        "SQARROW" => ("an operator or separator", "`~>`"),
        "SQARROW_STAR" => ("an operator or separator", "`~>*`"),
        "AND" => ("an operator or separator", "`/\\`"),
        "OR" => ("an operator or separator", "`\\/`"),
        "DOT" => ("an operator or separator", "`.`"),
        "DOT2" => ("an operator or separator", "`..`"),
        "DOT3" => ("an operator or separator", "`...`"),
        "COMMA" => ("an operator or separator", "`,`"),
        "COMMA_NL" => ("a separator", "a comma followed by a newline"),
        "SEMICOLON" => ("an operator or separator", "`;`"),
        "COLON" => ("an operator or separator", "`:`"),
        "COLON2" => ("an operator or separator", "`::`"),
        "COLON_SLASH" => ("an operator or separator", "`:/`"),
        "COLON_EQ" => ("an operator or separator", "`:=`"),
        "HASH" => ("an operator or separator", "`#`"),
        "HASH2" => ("an operator or separator", "`##`"),
        "DOLLAR" => ("an operator or separator", "`$`"),
        "QUEST" => ("an operator or separator", "`?`"),
        "TILDE" => ("an operator or separator", "`~`"),
        "TILDE2" => ("an operator or separator", "`~~`"),
        "LANGLE" => ("an operator or separator", "`<`"),
        "LANGLE_DASH" => ("an operator or separator", "`<-`"),
        "LANGLE_EQ" => ("an operator or separator", "`<=`"),
        "RANGLE" => ("an operator or separator", "`>`"),
        "RANGLE_EQ" => ("an operator or separator", "`>=`"),
        "RANGLE_LPAREN" => ("a delimiter", "`>(`"),
        "LPAREN" => ("a delimiter", "`(`"),
        "RPAREN" => ("a delimiter", "`)`"),
        "LBRACK" => ("a delimiter", "`[`"),
        "RBRACK" => ("a delimiter", "`]`"),
        "LBRACE" => ("a delimiter", "`{`"),
        "RBRACE" => ("a delimiter", "`}`"),
        "PLUS" => ("an operator or separator", "`+`"),
        "PLUS2" => ("an operator or separator", "`++`"),
        "MINUS" => ("an operator or separator", "`-`"),
        "DASH" => ("an operator or separator", "`--`"),
        "STAR" => ("an operator or separator", "`*`"),
        "ITER_STAR" => ("an operator or separator", "`*`"),
        "SLASH" => ("an operator or separator", "`/`"),
        "BACKSLASH" => ("an operator or separator", "`\\`"),
        "HOLE" => ("a hole", "`%`"),
        "HOLE_NUM" => ("a hole", "a numbered hole"),
        "HOLE_MULTI" => ("a hole", "`%%`"),
        "HOLE_NIL" => ("a hole", "`!%`"),
        "EQ" => ("an operator or separator", "`=`"),
        "NEQ" => ("an operator or separator", "`=/=`"),
        "UP" => ("an operator or separator", "`^`"),
        "BAR" => ("an operator or separator", "`|`"),
        "LATEX" => ("an operator or separator", "`%latex`"),
        "BOOL" => ("a built-in type", "`bool`"),
        "NAT" => ("a built-in type", "`nat`"),
        "INT" => ("a built-in type", "`int`"),
        "TEXT" => ("a built-in type", "`text`"),
        "SYNTAX" => ("a keyword", "`syntax`"),
        "EXTERN" => ("a keyword", "`extern`"),
        "TABLE" => ("a keyword", "`tbl`"),
        "RELATION" => ("a keyword", "`relation`"),
        "RULEGROUP" => ("a keyword", "`rulegroup`"),
        "RULE" => ("a keyword", "`rule`"),
        "VAR" => ("a keyword", "`var`"),
        "BUILTIN" => ("a keyword", "`builtin`"),
        "DEC" => ("a keyword", "`dec`"),
        "DEF" => ("a keyword", "`def`"),
        "IF" => ("a keyword", "`if`"),
        "OTHERWISE" => ("a keyword", "`otherwise`"),
        "DEBUG" => ("a keyword", "`debug`"),
        "HINT_LPAREN" => ("a keyword", "`hint(`"),
        "EPS" => ("a keyword", "`eps`"),
        "BOOLLIT" => ("a literal", "a boolean literal"),
        "NATLIT" => ("a literal", "a natural number"),
        "HEXLIT" => ("a literal", "a hexadecimal number"),
        "TEXTLIT" => ("a literal", "a text literal"),
        "UPID" => ("an identifier", "an uppercase identifier"),
        "LOID" => ("an identifier", "an identifier"),
        "DOTID" => ("an identifier", "a dot-prefixed identifier"),
        "UPID_LPAREN" => ("an identifier", "an uppercase identifier followed by `(`"),
        "LOID_LPAREN" => ("an identifier", "an identifier followed by `(`"),
        "UPID_LANGLE" => ("an identifier", "an uppercase identifier followed by `<`"),
        "LOID_LANGLE" => ("an identifier", "an identifier followed by `<`"),
        "EOF" => ("end of input", "end of input"),
        _ => ("a token", "a token"),
    }
}

/// Presents grammar terminals as source vocabulary, grouping long alternatives.
fn describe_expected(expected: &[String]) -> Option<String> {
    // Remove aliases that have the same visible spelling
    let mut alternatives = Vec::new();
    for terminal in expected {
        let (category, text) = terminal_presentation(terminal.trim_matches('"'));
        if !alternatives.contains(&(category, text)) {
            alternatives.push((category, text));
        }
    }
    if alternatives.is_empty() {
        return None;
    }

    // Small sets retain every spelling; large sets show vocabulary categories
    let texts = if alternatives.len() <= 8 {
        alternatives
            .into_iter()
            .map(|(_, text)| text.to_owned())
            .collect::<Vec<_>>()
    } else {
        let mut groups: Vec<(&str, Vec<&str>)> = Vec::new();
        for (category, text) in alternatives {
            if let Some((_, texts)) = groups.iter_mut().find(|(name, _)| *name == category) {
                texts.push(text);
            } else {
                groups.push((category, vec![text]));
            }
        }
        groups
            .into_iter()
            .map(|(category, texts)| {
                if texts.len() == 1 {
                    texts[0].to_owned()
                } else if texts.len() <= 4 {
                    format!("{category} ({})", texts.join(", "))
                } else {
                    format!("{category} (such as {})", texts[..3].join(", "))
                }
            })
            .collect()
    };
    Some(format!("expected {}", texts.join(" or ")))
}

// = Token expectations

const TOKEN_INVALID: &str = "parse/token-invalid";

/// Reports an unexpected token.
pub(crate) fn token_invalid(
    span: Span,
    actual: Option<&str>,
    expected: &[String],
) -> FrontendError {
    let diagnostic_error = diagnostic(
        TOKEN_INVALID,
        actual
            .map_or_else(|| "unexpected token".to_owned(), |actual| format!("unexpected {actual}")),
        vec![Label::primary(
            &span,
            describe_expected(expected).unwrap_or_else(|| "unexpected token".to_owned()),
        )],
    );
    Box::new(diagnostic_error.into())
}

const INPUT_INCOMPLETE: &str = "parse/input-incomplete";

/// Reports an unexpected end of input with the grammar's expected alternatives.
pub(crate) fn input_incomplete(span: Span, expected: &[String]) -> FrontendError {
    let diagnostic_error = diagnostic(
        INPUT_INCOMPLETE,
        "unexpected end of input".to_owned(),
        vec![Label::primary(
            &span,
            describe_expected(expected).unwrap_or_else(|| "expected more input".to_owned()),
        )],
    );
    Box::new(diagnostic_error.into())
}
