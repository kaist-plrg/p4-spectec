//! Parsers for splice marker names and keys
//!
//! Ordinary markers accept whitespace-separated identifiers.
//! Rule groups accept one relation with an optional slash and group identifier;
//! their closing brace remains optional, matching the OCaml parser.

use super::{error::Error, source::Source};

// == Parsing strings with expects

/// Consumes an exact prefix and leaves mismatches untouched.
pub fn parse_string(source: &mut Source<'_>, text: &str) -> bool {
    if source.remaining().starts_with(text) {
        source.advn(text.len());
        true
    } else {
        false
    }
}

// == Whitespace parsing

/// Consumes spaces, tabs, and newlines accepted by the marker grammar.
pub fn parse_space(source: &mut Source<'_>) {
    while matches!(source.get(), Some(b' ' | b'\t' | b'\n')) {
        source.advn(1);
    }
}

// == Splice anchor parsing

/// Consumes the opening of a marker with the supplied name.
pub fn parse_splice_start(source: &mut Source<'_>, name: &str) -> bool {
    parse_string(source, &format!("${{{name}:"))
}

// == Identifier parsing

fn parse_id(source: &mut Source<'_>) -> Result<String, Error> {
    // Accept the same ASCII identifier alphabet as the OCaml parser
    let text = source.remaining();
    let len = text.bytes().take_while(|ch| matches!(ch, b'A'..=b'Z' | b'a'..=b'z' | b'0'..=b'9' | b'_' | b'\'' | b'`' | b'-' | b'*' | b'.')).count();
    // Empty identifiers fail at the first unexpected byte
    if len == 0 {
        return Err(Error::Identifier(source.span()));
    }
    source.advn(len);
    Ok(text[..len].to_owned())
}

// == Entry points

/// Parses identifiers through the required closing brace.
pub fn parse_ids(source: &mut Source<'_>) -> Result<Vec<String>, Error> {
    let mut ids = Vec::new();
    // Preserve identifier order and repetitions
    loop {
        parse_space(source);
        if parse_string(source, "}") {
            return Ok(ids);
        }
        ids.push(parse_id(source)?);
    }
}

/// Parses one relation and optional group, accepting an absent closing brace.
pub fn parse_id_with_sub(source: &mut Source<'_>) -> Result<(String, String), Error> {
    parse_space(source);
    let id = parse_id(source)?;
    // A slash requires a nonempty group identifier
    let id_sub = if parse_string(source, "/") { parse_id(source)? } else { String::new() };
    // Preserve the OCaml parser's optional closing delimiter
    parse_space(source);
    parse_string(source, "}");
    Ok((id, id_sub))
}
