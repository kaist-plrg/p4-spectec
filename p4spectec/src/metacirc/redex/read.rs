//! Reading the s-expressions that spec-meta-redex writes
//!
//! spec-meta-redex prints its answers and requests with Racket's `write`
//! (`3-execution/3.04-extern-host.rkt`),
//! so a value or type arrives as the `val` or `typ` term it is, in the grammar of `sexp`.
//! `Reader` reads that text in one pass into the arena, without building a tree first;
//! `read_value`, `read_values`, `read_typs`, and `read_id` read the terms.
//!
//! It reads what `write` prints for these terms:
//! lists, the symbols of the grammar, `#t` and `#f`, decimal integers,
//! and string literals with the escapes `write` uses,
//! `\"`, `\\`, `\a`, `\b`, `\t`, `\n`, `\v`, `\f`, `\r`, `\e`, `\uXXXX`, and `\UXXXXXXXX`.
//! As `read` does, `\u` takes up to four hex digits and `\U` up to eight.

use super::cache::Cache;
use crate::{
    frontend::parse::parse_typ,
    lang::{
        common::{Id, Iter, notation::atom::Atom, prim::num::Natural, source::Span},
        data::{
            arena::Arena,
            notation::{
                AtomPhrase,
                flat::{Mixfix, Mixop, parse::ToMixop},
            },
            typ::{self, Typ, TypKind},
            value::flat::{Value, ValueField, ValueKind, make},
        },
        el::ast as el,
    },
    phrase,
    runner::ExternError,
    util::json::json,
};
use num_bigint::BigInt;
use serde::Deserialize;
use std::{borrow::Cow, rc::Rc};

/// A cursor over the text of one s-expression.
pub(super) struct Reader<'a> {
    text: &'a str,
    pos: usize,
}

impl<'a> Reader<'a> {
    /// Starts reading `text` at its beginning.
    pub(super) fn new(text: &'a str) -> Self {
        Self { text, pos: 0 }
    }

    /// Checks that only whitespace is left.
    pub(super) fn end(mut self) -> Result<(), ExternError> {
        self.skip_space();
        if self.pos < self.text.len() {
            return Err(self.unexpected("the end of the text"));
        }
        Ok(())
    }

    /// Reports text of another shape than `expected`, from the cursor on.
    pub(super) fn unexpected(&self, expected: &str) -> ExternError {
        let text_rest = &self.text[self.pos..];
        let mut text_found = text_rest.chars().take(200).collect::<String>();
        if text_found.len() < text_rest.len() {
            text_found.push_str("...");
        }
        if text_found.is_empty() {
            text_found.push_str("the end of the text");
        }
        ExternError::diagnostic_message(format!(
            "spec-meta-redex's s-expression: expected {expected} at offset {}, but got {text_found}",
            self.pos
        ))
    }

    fn rest(&self) -> &'a [u8] {
        &self.text.as_bytes()[self.pos..]
    }

    fn skip_space(&mut self) {
        while let Some(byte) = self.rest().first()
            && byte.is_ascii_whitespace()
        {
            self.pos += 1;
        }
    }

    /// Returns the next byte after whitespace, without taking it.
    fn peek(&mut self) -> Option<u8> {
        self.skip_space();
        self.rest().first().copied()
    }

    /// Takes `(`.
    pub(super) fn open(&mut self, expected: &str) -> Result<(), ExternError> {
        match self.peek() {
            Some(b'(') => {
                self.pos += 1;
                Ok(())
            }
            _ => Err(self.unexpected(expected)),
        }
    }

    /// Takes `)`.
    pub(super) fn close(&mut self) -> Result<(), ExternError> {
        match self.peek() {
            Some(b')') => {
                self.pos += 1;
                Ok(())
            }
            _ => Err(self.unexpected("`)`")),
        }
    }

    /// Takes `)` if it is next.
    pub(super) fn at_close(&mut self) -> bool {
        let close = self.peek() == Some(b')');
        if close {
            self.pos += 1;
        }
        close
    }

    /// Tells whether a list starts next, rather than an atom.
    pub(super) fn at_open(&mut self) -> bool {
        self.peek() == Some(b'(')
    }

    /// Takes the text of the next atom: a symbol, a boolean, or a number.
    fn token(&mut self) -> &'a str {
        self.skip_space();
        let pos_start = self.pos;
        while let Some(byte) = self.rest().first()
            && !byte.is_ascii_whitespace()
            && !matches!(byte, b'(' | b')' | b'"')
        {
            self.pos += 1;
        }
        &self.text[pos_start..self.pos]
    }

    /// Takes a symbol.
    pub(super) fn symbol(&mut self, expected: &str) -> Result<&'a str, ExternError> {
        let pos_start = self.pos;
        let text_symbol = self.token();
        if text_symbol.is_empty() || text_symbol.starts_with('#') {
            self.pos = pos_start;
            return Err(self.unexpected(expected));
        }
        Ok(text_symbol)
    }

    /// Takes `(KEYWORD`, returning the symbol heading the list.
    pub(super) fn head(&mut self, expected: &str) -> Result<&'a str, ExternError> {
        self.open(expected)?;
        self.symbol(expected)
    }

    fn boolean(&mut self) -> Result<bool, ExternError> {
        let pos_start = self.pos;
        match self.token() {
            "#t" => Ok(true),
            "#f" => Ok(false),
            _ => {
                self.pos = pos_start;
                Err(self.unexpected("a boolean"))
            }
        }
    }

    /// Takes a decimal integer: an optional minus sign, then digits.
    fn integer(&mut self) -> Result<BigInt, ExternError> {
        let pos_start = self.pos;
        let token = self.token();
        let text_digits = token.strip_prefix('-').unwrap_or(token);
        if !text_digits.is_empty()
            && text_digits.bytes().all(|byte| byte.is_ascii_digit())
            && let Ok(int) = token.parse()
        {
            return Ok(int);
        }
        self.pos = pos_start;
        Err(self.unexpected("an integer"))
    }

    /// Takes a string literal, borrowed from the text unless it has escapes.
    pub(super) fn string(&mut self) -> Result<Cow<'a, str>, ExternError> {
        if self.peek() != Some(b'"') {
            return Err(self.unexpected("a string"));
        }
        let pos_start = self.pos;
        self.pos += 1;
        let pos_body = self.pos;
        // Borrow a string without escapes whole
        loop {
            match self.rest().first() {
                Some(b'"') => {
                    let text = &self.text[pos_body..self.pos];
                    self.pos += 1;
                    return Ok(Cow::Borrowed(text));
                }
                Some(b'\\') => break,
                Some(_) => self.pos += 1,
                None => {
                    self.pos = pos_start;
                    return Err(self.unexpected("a closed string"));
                }
            }
        }
        // Copy the text before the first escape, then decode the rest
        let mut text = self.text[pos_body..self.pos].to_owned();
        loop {
            let Some(ch) = self.text[self.pos..].chars().next() else {
                self.pos = pos_start;
                return Err(self.unexpected("a closed string"));
            };
            self.pos += ch.len_utf8();
            match ch {
                // End the string
                '"' => return Ok(Cow::Owned(text)),
                // Decode an escape
                '\\' => {
                    let ch_escaped = self.escape().ok_or_else(|| {
                        let error = self.unexpected("an escape that Racket's write prints");
                        self.pos = pos_start;
                        error
                    })?;
                    text.push(ch_escaped);
                }
                // Keep any other character as it is
                ch => text.push(ch),
            }
        }
    }

    /// Takes the escape after `\`, returning the character it stands for.
    fn escape(&mut self) -> Option<char> {
        let ch = self.text[self.pos..].chars().next()?;
        self.pos += ch.len_utf8();
        Some(match ch {
            '"' => '"',
            '\\' => '\\',
            'a' => '\x07',
            'b' => '\x08',
            't' => '\t',
            'n' => '\n',
            'v' => '\x0b',
            'f' => '\x0c',
            'r' => '\r',
            'e' => '\x1b',
            'u' => self.hex(4)?,
            'U' => self.hex(8)?,
            _ => return None,
        })
    }

    /// Takes up to `len_max` hex digits, at least one, returning their character.
    fn hex(&mut self, len_max: usize) -> Option<char> {
        let len = self
            .rest()
            .iter()
            .take(len_max)
            .take_while(|byte| byte.is_ascii_hexdigit())
            .count();
        if len == 0 {
            return None;
        }
        let code = u32::from_str_radix(&self.text[self.pos..self.pos + len], 16).ok()?;
        self.pos += len;
        char::from_u32(code)
    }

    /// Skips one s-expression, returning its text.
    fn skip(&mut self) -> Result<&'a str, ExternError> {
        self.skip_space();
        let pos_start = self.pos;
        match self.peek() {
            // Skip a list and everything in it
            Some(b'(') => {
                self.pos += 1;
                while !self.at_close() {
                    self.skip()?;
                }
            }
            // Skip a string, escapes included
            Some(b'"') => {
                self.string()?;
            }
            // Skip an atom
            Some(_) if !self.token().is_empty() => {}
            _ => return Err(self.unexpected("an s-expression")),
        }
        Ok(&self.text[pos_start..self.pos])
    }
}

// == Identifiers and notation

fn id_of(name: &str) -> Id {
    phrase!(node: name.into(), span: Span::default())
}

/// Reads an identifier, a string.
pub(super) fn read_id(reader: &mut Reader) -> Result<Id, ExternError> {
    Ok(id_of(&reader.string()?))
}

/// Reads a notation from its atom rows, interned once per spelling.
///
/// The cache keys a notation by its s-expression as Racket's `write` prints it.
fn read_mixop(
    reader: &mut Reader,
    arena: &mut Arena,
    cache: &mut Cache,
) -> Result<Mixop, ExternError> {
    let pos_start = reader.pos;
    let text = reader.skip()?;
    if let Some(mixop) = cache.mixops_in.get(text) {
        return Ok(*mixop);
    }
    // Read the atom rows
    let mut reader_rows = Reader::new(text);
    let mut rows = vec![];
    reader_rows.open("a mixop")?;
    while !reader_rows.at_close() {
        reader_rows.open("a row of atoms")?;
        let mut row = vec![];
        while !reader_rows.at_close() {
            row.push(reader_rows.string()?);
        }
        rows.push(row);
    }
    let failed = |error: String| {
        ExternError::diagnostic_message(format!(
            "spec-meta-redex's s-expression: invalid mixop {text} at offset {pos_start}: {error}"
        ))
    };
    // Intern the notation the rows spell
    let mixop = match &rows[..] {
        [] => return Err(failed("it has no rows".into())),
        // (()) is the empty notation, which has no text to parse
        [row] if row.is_empty() => [Vec::<AtomPhrase>::new()][..]
            .to_mixop(&mut arena.mixop)
            .map_err(|error| failed(error.to_string()))?,
        _ => {
            // Print the rows as `[Vec<AtomPhrase>]::to_mixop` does, a placeholder at each hole
            let text_mixop = rows
                .iter()
                .map(|row| row.join(" "))
                .collect::<Vec<_>>()
                .join(" nat ");
            let mixop = text_mixop
                .as_str()
                .to_mixop(&mut arena.mixop)
                .map_err(|error| failed(error.to_string()))?;
            if arena.mixop.arity(mixop) + 1 != rows.len() {
                return Err(failed("the rows do not match its holes".into()));
            }
            mixop
        }
    };
    if cache.enabled {
        cache.mixops_in.insert(text.to_owned(), mixop);
    }
    Ok(mixop)
}

/// Reads an atom from its printed spelling, as a struct field names it,
/// parsed once per spelling.
fn read_atom(cache: &mut Cache, text: &str) -> Result<AtomPhrase, ExternError> {
    if let Some(atom) = cache.atoms_in.get(text) {
        return Ok(atom.clone());
    }
    let not_atom = || {
        ExternError::diagnostic_message(format!(
            "spec-meta-redex's s-expression: expected an atom, but got {text:?}"
        ))
    };
    // Delimiter atoms do not parse outside their matching pair
    let atom_kind = match text {
        "`<" => Atom::LAngle,
        "`>" => Atom::RAngle,
        "`(" => Atom::LParen,
        "`)" => Atom::RParen,
        "`[" => Atom::LBrack,
        "`]" => Atom::RBrack,
        "`{" => Atom::LBrace,
        "`}" => Atom::RBrace,
        _ => match parse_typ(text)? {
            el::Typ::Notation(not_typ) => match not_typ.node {
                el::NotTypKind::Atom(atom) => atom.node,
                _ => return Err(not_atom()),
            },
            el::Typ::Plain(_) => return Err(not_atom()),
        },
    };
    let atom = phrase!(node: atom_kind, span: Span::default());
    if cache.enabled {
        cache.atoms_in.insert(text.into(), atom.clone());
    }
    Ok(atom)
}

// == Values

/// Returns the type of values the wire does not type.
fn placeholder_typ() -> Typ {
    typ::make::var(id_of("_"), vec![])
}

fn placeholder() -> Rc<TypKind> {
    Rc::new(placeholder_typ().node)
}

fn typ_of(arena: &Arena, value: Value) -> Typ {
    phrase!(node: (**arena.value.typ(value.note)).clone(), span: Span::default())
}

/// Parses an extern value's JSON payload, of any depth.
fn read_payload(text: &str) -> Result<json, ExternError> {
    let mut deserializer = serde_json::Deserializer::from_str(text);
    deserializer.disable_recursion_limit();
    let json = json::deserialize(&mut deserializer)?;
    deserializer.end()?;
    Ok(json)
}

/// Reads a value into the arena.
///
/// A value has the placeholder type `_` where the wire has none;
/// tuples, options, and lists take theirs from their elements.
pub(super) fn read_value(
    reader: &mut Reader,
    arena: &mut Arena,
    cache: &mut Cache,
) -> Result<Value, ExternError> {
    let span = Span::default();
    let pos_start = reader.pos;
    let value = match reader.head("a value")? {
        "BOOL" => make::bool(arena, reader.boolean()?, span)?,
        "NAT" => {
            let int = reader.integer()?;
            let nat = Natural::try_from(int).map_err(|_| {
                reader.pos = pos_start;
                reader.unexpected("a natural number")
            })?;
            make::nat(arena, nat, span)?
        }
        "INT" => make::int(arena, reader.integer()?, span)?,
        "TEXT" => make::text(arena, reader.string()?.into_owned(), span)?,
        "STR" => {
            reader.open("struct fields")?;
            let mut fields = vec![];
            while !reader.at_close() {
                reader.open("a struct field")?;
                let atom = read_atom(cache, &reader.string()?)?;
                let value = read_value(reader, arena, cache)?;
                reader.close()?;
                fields.push(ValueField { atom, value });
            }
            make::structure(arena, placeholder(), fields, span)?
        }
        "INJ" => {
            reader.open("a case")?;
            let mixop = read_mixop(reader, arena, cache)?;
            let values = read_values(reader, arena, cache)?;
            reader.close()?;
            let mixfix = Mixfix::new(&arena.mixop, mixop, values)
                .map_err(|error| ExternError::diagnostic_message(error.to_string()))?;
            make::case_flat(arena, placeholder(), mixfix, span)?
        }
        "TUP" => {
            let values = read_values(reader, arena, cache)?;
            let typ = typ::make::tuple(values.iter().map(|value| typ_of(arena, *value)).collect());
            make::tuple(arena, Rc::new(typ.node), values, span)?
        }
        "OPT" => {
            reader.open("an option")?;
            if reader.at_close() {
                let typ = typ::make::opt(placeholder_typ());
                make::opt(arena, Rc::new(typ.node), None, span)?
            } else {
                let value = read_value(reader, arena, cache)?;
                reader.close()?;
                let typ = typ::make::opt(typ_of(arena, value));
                make::opt(arena, Rc::new(typ.node), Some(value), span)?
            }
        }
        "LIST" => {
            let values = read_values(reader, arena, cache)?;
            let typ_elem = values
                .first()
                .map_or_else(placeholder_typ, |value| typ_of(arena, *value));
            let typ = typ::make::list(typ_elem);
            make::list(arena, Rc::new(typ.node), values, span)?
        }
        "FUNC" => Value::new(arena, ValueKind::Func(read_id(reader)?), placeholder(), span)?,
        "EXT" => {
            let json = read_payload(&reader.string()?)?;
            make::external(arena, placeholder(), Rc::new(json), span)?
        }
        _ => {
            reader.pos = pos_start;
            return Err(reader.unexpected("a value"));
        }
    };
    reader.close()?;
    Ok(value)
}

/// Reads `(val ...)` into the arena.
pub(super) fn read_values(
    reader: &mut Reader,
    arena: &mut Arena,
    cache: &mut Cache,
) -> Result<Vec<Value>, ExternError> {
    reader.open("a list of values")?;
    let mut values = vec![];
    while !reader.at_close() {
        values.push(read_value(reader, arena, cache)?);
    }
    Ok(values)
}

// == Types

/// Reads a type.
fn read_typ(reader: &mut Reader) -> Result<Typ, ExternError> {
    let pos_start = reader.pos;
    // Read a type without arguments, a symbol
    if !reader.at_open() {
        return Ok(match reader.symbol("a type")? {
            "BOOL" => typ::make::bool(),
            "NAT" => typ::make::nat(),
            "INT" => typ::make::int(),
            "TEXT" => typ::make::text(),
            _ => {
                reader.pos = pos_start;
                return Err(reader.unexpected("a type"));
            }
        });
    }
    // Read a type with arguments, a list
    let typ = match reader.head("a type")? {
        "VAR" => typ::make::var(read_id(reader)?, read_typs(reader)?),
        "TUP" => typ::make::tuple(read_typs(reader)?),
        "ITER" => {
            let typ = read_typ(reader)?;
            let iter = match reader.symbol("an iteration")? {
                "QUEST" => Iter::Opt,
                "STAR" => Iter::List,
                _ => {
                    reader.pos = pos_start;
                    return Err(reader.unexpected("a type"));
                }
            };
            typ::make::iter(typ, iter)
        }
        "FUNC" => {
            reader.open("type parameters")?;
            let mut tparams = vec![];
            while !reader.at_close() {
                tparams.push(read_id(reader)?);
            }
            typ::make::func(tparams, read_typs(reader)?, read_typ(reader)?)
        }
        _ => {
            reader.pos = pos_start;
            return Err(reader.unexpected("a type"));
        }
    };
    reader.close()?;
    Ok(typ)
}

/// Reads `(typ ...)`.
pub(super) fn read_typs(reader: &mut Reader) -> Result<Vec<Typ>, ExternError> {
    reader.open("a list of types")?;
    let mut typs = vec![];
    while !reader.at_close() {
        typs.push(read_typ(reader)?);
    }
    Ok(typs)
}

/// Reads the text of a string literal that is all of `text`,
/// as a `raise` line carries it.
pub(super) fn read_message(text: &str) -> Option<String> {
    let mut reader = Reader::new(text);
    let message = reader.string().ok()?.into_owned();
    reader.end().ok()?;
    Some(message)
}
