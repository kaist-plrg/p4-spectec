//! A recursive-descent parser for KORE text
//!
//! `parse_definition` reads a `definition.kore`,
//! `parse_pattern` one pattern such as krun's initial term.
//! Errors carry the line where they occur.
//!
//! KORE strings are byte strings, as in the LLVM backend:
//! `\xHH` is one byte, `\uHHHH` and `\UHHHHHHHH` the bytes of UTF-8.
//! A string is kept as the text with one character per byte,
//! the character whose code is that byte, so `"\u00e9"` has two characters.

use super::ast::{Attr, Definition, Module, Pattern, Sentence, Sort};

// == Errors

/// A KORE syntax error at a line.
#[derive(Debug)]
pub struct ParseError {
    pub line: usize,
    pub message: String,
}

type Result<T> = std::result::Result<T, ParseError>;

// == Lexer

#[derive(Clone, Debug, PartialEq, Eq)]
enum Token {
    /// An identifier, `\connective`, or `@setvar`.
    Id(String),
    Str(String),
    LBrace,
    RBrace,
    LParen,
    RParen,
    LBrack,
    RBrack,
    Comma,
    Colon,
    ColonEq,
    Eof,
}

struct Lexer<'a> {
    src: &'a [u8],
    pos: usize,
    line: usize,
}

fn is_id_start(c: u8) -> bool {
    c.is_ascii_alphabetic()
}

fn is_id_char(c: u8) -> bool {
    c.is_ascii_alphanumeric() || c == b'\'' || c == b'-'
}

impl<'a> Lexer<'a> {
    fn error<T>(&self, message: impl Into<String>) -> Result<T> {
        Err(ParseError { line: self.line, message: message.into() })
    }

    fn peek_at(&self, offset: usize) -> Option<u8> {
        self.src.get(self.pos + offset).copied()
    }

    /// Skips whitespace and comments.
    fn skip(&mut self) {
        while let Some(c) = self.peek_at(0) {
            match c {
                b'\n' => {
                    self.line += 1;
                    self.pos += 1;
                }
                b' ' | b'\t' | b'\r' => self.pos += 1,
                b'/' if self.peek_at(1) == Some(b'/') => {
                    while self.peek_at(0).is_some_and(|c| c != b'\n') {
                        self.pos += 1;
                    }
                }
                b'/' if self.peek_at(1) == Some(b'*') => {
                    self.pos += 2;
                    while self.pos + 1 < self.src.len()
                        && !(self.src[self.pos] == b'*' && self.src[self.pos + 1] == b'/')
                    {
                        if self.src[self.pos] == b'\n' {
                            self.line += 1;
                        }
                        self.pos += 1;
                    }
                    self.pos += 2;
                }
                _ => return,
            }
        }
    }

    /// Reads `k` hexadecimal digits of an escape.
    fn hex(&mut self, k: usize) -> Result<u32> {
        let digits = self
            .src
            .get(self.pos..self.pos + k)
            .and_then(|digits| std::str::from_utf8(digits).ok())
            .and_then(|digits| u32::from_str_radix(digits, 16).ok());
        let Some(code) = digits else {
            return self.error("bad escape");
        };
        self.pos += k;
        Ok(code)
    }

    /// A string literal; escapes are `\" \\ \n \t \r \f \xHH \uHHHH \UHHHHHHHH`.
    fn string(&mut self) -> Result<String> {
        let mut bytes = Vec::new();
        self.pos += 1;
        loop {
            let Some(c) = self.peek_at(0) else {
                return self.error("unterminated string");
            };
            self.pos += 1;
            match c {
                b'"' => break,
                b'\\' => {
                    let Some(c) = self.peek_at(0) else {
                        return self.error("unterminated string");
                    };
                    self.pos += 1;
                    match c {
                        b'"' => bytes.push(b'"'),
                        b'\\' => bytes.push(b'\\'),
                        b'n' => bytes.push(b'\n'),
                        b't' => bytes.push(b'\t'),
                        b'r' => bytes.push(b'\r'),
                        b'f' => bytes.push(0x0c),
                        b'x' => bytes.push(self.hex(2)? as u8),
                        b'u' => push_code(&mut bytes, self.hex(4)?),
                        b'U' => push_code(&mut bytes, self.hex(8)?),
                        c => return self.error(format!("unknown escape \\{}", c as char)),
                    }
                }
                c => bytes.push(c),
            }
        }
        Ok(text_of_bytes(&bytes))
    }

    fn next(&mut self) -> Result<Token> {
        self.skip();
        let Some(c) = self.peek_at(0) else {
            return Ok(Token::Eof);
        };
        let single = |lexer: &mut Self, token| {
            lexer.pos += 1;
            Ok(token)
        };
        match c {
            b'{' => single(self, Token::LBrace),
            b'}' => single(self, Token::RBrace),
            b'(' => single(self, Token::LParen),
            b')' => single(self, Token::RParen),
            b'[' => single(self, Token::LBrack),
            b']' => single(self, Token::RBrack),
            b',' => single(self, Token::Comma),
            b':' if self.peek_at(1) == Some(b'=') => {
                self.pos += 2;
                Ok(Token::ColonEq)
            }
            b':' => single(self, Token::Colon),
            b'"' => self.string().map(Token::Str),
            b'\\' | b'@' => {
                let start = self.pos;
                self.pos += 1;
                Ok(Token::Id(self.ident_from(start)))
            }
            c if is_id_start(c) => {
                let start = self.pos;
                Ok(Token::Id(self.ident_from(start)))
            }
            c => self.error(format!("unexpected character {:?}", c as char)),
        }
    }

    fn ident_from(&mut self, start: usize) -> String {
        while self.peek_at(0).is_some_and(is_id_char) {
            self.pos += 1;
        }
        // Identifier characters are ASCII
        String::from_utf8_lossy(&self.src[start..self.pos]).into_owned()
    }
}

/// Appends a code point in UTF-8.
fn push_code(bytes: &mut Vec<u8>, code: u32) {
    // Code points beyond Unicode or surrogates are kept as the replacement character
    let c = char::from_u32(code).unwrap_or(char::REPLACEMENT_CHARACTER);
    let mut buf = [0; 4];
    bytes.extend_from_slice(c.encode_utf8(&mut buf).as_bytes());
}

/// The text with one character per byte.
pub fn text_of_bytes(bytes: &[u8]) -> String {
    bytes.iter().map(|&byte| char::from(byte)).collect()
}

/// The bytes of a text with one character per byte, if it is one.
pub fn bytes_of_text(text: &str) -> Option<Vec<u8>> {
    text.chars().map(|c| u8::try_from(c).ok()).collect()
}

// == Parser

/// A parser with one token of lookahead.
struct Parser<'a> {
    lexer: Lexer<'a>,
    token: Token,
}

impl<'a> Parser<'a> {
    fn new(src: &'a [u8]) -> Result<Self> {
        let mut lexer = Lexer { src, pos: 0, line: 1 };
        let token = lexer.next()?;
        Ok(Self { lexer, token })
    }

    fn error<T>(&self, message: impl Into<String>) -> Result<T> {
        self.lexer.error(message)
    }

    fn advance(&mut self) -> Result<Token> {
        let token = self.lexer.next()?;
        Ok(std::mem::replace(&mut self.token, token))
    }

    fn expect(&mut self, token: Token, what: &str) -> Result<()> {
        if self.token == token {
            self.advance()?;
            Ok(())
        } else {
            self.error(format!("expected {what}"))
        }
    }

    fn ident(&mut self) -> Result<String> {
        match self.token {
            Token::Id(_) => match self.advance()? {
                Token::Id(id) => Ok(id),
                _ => unreachable!("the current token is an identifier"),
            },
            _ => self.error("expected an identifier"),
        }
    }

    fn keyword(&mut self, keyword: &str) -> Result<()> {
        match &self.token {
            Token::Id(id) if id == keyword => {
                self.advance()?;
                Ok(())
            }
            _ => self.error(format!("expected {keyword}")),
        }
    }

    /// A comma-separated list between `open` and `close`.
    fn seq<T>(
        &mut self,
        open: Token,
        close: Token,
        mut item: impl FnMut(&mut Self) -> Result<T>,
    ) -> Result<Vec<T>> {
        self.expect(open, "an opening bracket")?;
        let mut items = Vec::new();
        if self.token == close {
            self.advance()?;
            return Ok(items);
        }
        loop {
            items.push(item(self)?);
            if self.token == Token::Comma {
                self.advance()?;
            } else {
                self.expect(close, "a closing bracket")?;
                return Ok(items);
            }
        }
    }

    fn sort(&mut self) -> Result<Sort> {
        let name = self.ident()?;
        if self.token == Token::LBrace {
            let sorts = self.seq(Token::LBrace, Token::RBrace, Self::sort)?;
            Ok(Sort::Sort(name, sorts))
        } else {
            Ok(Sort::Var(name))
        }
    }

    fn sorts(&mut self, open: Token, close: Token) -> Result<Vec<Sort>> {
        self.seq(open, close, Self::sort)
    }

    fn pattern(&mut self) -> Result<Pattern> {
        // Terms nest deeply, e.g. long K sequences and maps
        stacker::maybe_grow(64 * 1024, 16 * 1024 * 1024, || match &self.token {
            Token::Str(_) => match self.advance()? {
                Token::Str(text) => Ok(Pattern::Str(text)),
                _ => unreachable!("the current token is a string"),
            },
            Token::Id(_) => {
                let name = self.ident()?;
                if self.token == Token::Colon && !name.starts_with('\\') {
                    self.advance()?;
                    let sort = self.sort()?;
                    if name.starts_with('@') {
                        Ok(Pattern::SetVar(name, sort))
                    } else {
                        Ok(Pattern::Var(name, sort))
                    }
                } else {
                    let sorts = self.sorts(Token::LBrace, Token::RBrace)?;
                    let args = self.seq(Token::LParen, Token::RParen, Self::pattern)?;
                    Ok(Pattern::App(name, sorts, args))
                }
            }
            _ => self.error("expected a pattern"),
        })
    }

    fn attrs(&mut self) -> Result<Vec<Attr>> {
        self.seq(Token::LBrack, Token::RBrack, Self::pattern)
    }

    fn sort_params(&mut self) -> Result<Vec<String>> {
        self.seq(Token::LBrace, Token::RBrace, Self::ident)
    }

    fn sentence(&mut self) -> Result<Sentence> {
        let keyword = self.ident()?;
        match keyword.as_str() {
            "import" => {
                let name = self.ident()?;
                self.attrs()?;
                Ok(Sentence::Import(name))
            }
            "sort" | "hooked-sort" => {
                let name = self.ident()?;
                let params = self.sort_params()?;
                let attrs = self.attrs()?;
                Ok(Sentence::SortDecl { name, params, attrs })
            }
            "symbol" | "hooked-symbol" => {
                let name = self.ident()?;
                let params = self.sort_params()?;
                let args = self.sorts(Token::LParen, Token::RParen)?;
                self.expect(Token::Colon, "':'")?;
                let result = self.sort()?;
                let attrs = self.attrs()?;
                Ok(Sentence::SymbolDecl { name, params, args, result, attrs })
            }
            "alias" => {
                let name = self.ident()?;
                self.sort_params()?;
                self.sorts(Token::LParen, Token::RParen)?;
                self.expect(Token::Colon, "':'")?;
                self.sort()?;
                self.keyword("where")?;
                let lhs = self.pattern()?;
                self.expect(Token::ColonEq, "':='")?;
                let body = self.pattern()?;
                self.attrs()?;
                Ok(Sentence::AliasDecl { name, lhs, body })
            }
            "axiom" => {
                self.sort_params()?;
                let pattern = self.pattern()?;
                let attrs = self.attrs()?;
                Ok(Sentence::Axiom { pattern, attrs })
            }
            "claim" => {
                self.sort_params()?;
                let pattern = self.pattern()?;
                let attrs = self.attrs()?;
                Ok(Sentence::Claim { pattern, attrs })
            }
            _ => self.error(format!("unknown sentence {keyword}")),
        }
    }

    fn module(&mut self) -> Result<Module> {
        self.keyword("module")?;
        let name = self.ident()?;
        let mut sentences = Vec::new();
        while self.token != Token::Id("endmodule".to_owned()) {
            sentences.push(self.sentence()?);
        }
        self.advance()?;
        self.attrs()?;
        Ok(Module { name, sentences })
    }
}

// == Entry points

/// Parses a definition: attributes, then modules.
pub fn parse_definition(src: &[u8]) -> Result<Definition> {
    let mut parser = Parser::new(src)?;
    parser.attrs()?;
    let mut modules = Vec::new();
    while parser.token != Token::Eof {
        modules.push(parser.module()?);
    }
    Ok(Definition { modules })
}

/// Parses one sort, with nothing after it.
pub fn parse_sort(src: &[u8]) -> Result<Sort> {
    let mut parser = Parser::new(src)?;
    let sort = parser.sort()?;
    if parser.token != Token::Eof {
        return parser.error("trailing input after sort");
    }
    Ok(sort)
}

/// Parses one pattern, with nothing after it.
pub fn parse_pattern(src: &[u8]) -> Result<Pattern> {
    let mut parser = Parser::new(src)?;
    let pattern = parser.pattern()?;
    if parser.token != Token::Eof {
        return parser.error("trailing input after pattern");
    }
    Ok(pattern)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn domain_value() {
        let pattern = parse_pattern(br#"\dv{SortInt{}}("1")"#).unwrap();
        let sort = Sort::Sort("SortInt".to_owned(), vec![]);
        assert_eq!(
            pattern,
            Pattern::App("\\dv".to_owned(), vec![sort], vec![Pattern::Str("1".to_owned())])
        );
    }

    #[test]
    fn variables() {
        let pattern = parse_pattern(b"f{}(VarX:SortInt{}, @Y:S)").unwrap();
        let Pattern::App(_, _, args) = pattern else { panic!() };
        assert!(matches!(&args[0], Pattern::Var(x, Sort::Sort(..)) if x == "VarX"));
        assert!(matches!(&args[1], Pattern::SetVar(y, Sort::Var(_)) if y == "@Y"));
    }

    #[test]
    fn escapes() {
        let pattern = parse_pattern(br#""a\"\\\n\x41\xe9\u00e9""#).unwrap();
        assert_eq!(pattern, Pattern::Str("a\"\\\nA\u{e9}\u{c3}\u{a9}".to_owned()));
    }

    #[test]
    fn raw_bytes() {
        // A raw UTF-8 character is its bytes, as an escape is
        let raw = parse_pattern("\"\u{ac00}\"".as_bytes()).unwrap();
        let escaped = parse_pattern(br#""\uac00""#).unwrap();
        assert_eq!(raw, escaped);
        let Pattern::Str(text) = raw else { panic!() };
        assert_eq!(bytes_of_text(&text).unwrap(), "\u{ac00}".as_bytes());
    }

    #[test]
    fn definition() {
        let src = r#"[topCellInitializer{}(LblinitGeneratedTopCell{}())]
module M
  sort SortInt{} [hook{}("INT.Int")]
  symbol f{}(SortInt{}) : SortInt{} [function{}()]
  alias a{}() : SortInt{} where a{}() := \dv{SortInt{}}("1") []
  // a comment
  axiom{R} \equals{SortInt{}, R}(f{}(VarX:SortInt{}), VarX:SortInt{}) [owise{}()]
endmodule [ ]"#;
        let definition = parse_definition(src.as_bytes()).unwrap();
        assert_eq!(definition.modules.len(), 1);
        assert_eq!(definition.sentences().count(), 4);
    }

    #[test]
    fn error_line() {
        let error = parse_pattern(b"f{}(\n\n)x").unwrap_err();
        assert_eq!(error.line, 3);
    }
}
