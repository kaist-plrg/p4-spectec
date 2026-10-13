//! Decodes AL meta data with constructor and payload checks
//!
//! `spec`, `exp`, and `typ` reconstruct source syntax while borrowing the arena
//! `value` allocates runtime payloads in that arena,
//! copying child handles before recursive allocation

mod def;
mod exp;
mod typ;
mod value;

use super::mixops as m;
use crate::{
    frontend::parse::parse_mixop,
    lang::{
        common::{Id, Iter, notation::atom::Atom, source::Span},
        data::{
            arena::Arena,
            notation::tree::Mixop,
            value::flat::{Value, get},
        },
        il::ast,
        traits::print::Print,
    },
    metacirc::Error,
    phrase,
};
use std::rc::Rc;

pub use def::{def, spec};
pub use exp::{exp, exps};

pub use typ::{deftyp, typ, typs};
pub use value::{value, values};

/// Decodes an owned identifier from a meta text value.
pub fn id(arena: &Arena, value_meta: Value) -> Result<Id, Error> {
    Ok(
        phrase!(node: get::text(arena, &value_meta)?.into(), span: *arena.value.span(value_meta.span)),
    )
}

/// Maps a checked meta list without allocating runtime values.
pub(super) fn list<T>(
    arena: &Arena,
    value_meta: Value,
    func_decode: impl Fn(&Arena, Value) -> Result<T, Error>,
) -> Result<Vec<T>, Error> {
    get::list(arena, &value_meta)?
        .iter()
        .map(|value_elem| func_decode(arena, *value_elem))
        .collect()
}

/// Copies a case's children before recursive decoding or allocation.
pub(super) fn case(
    arena: &Arena,
    value_meta: Value,
) -> Result<(Rc<Mixop>, Vec<Value>, Span), Error> {
    let value_case = get::case(arena, &value_meta)?;
    Ok((
        Rc::new(value_case.mixop().into_tree(&arena.mixop)),
        value_case.args().to_vec(),
        *arena.value.span(value_meta.span),
    ))
}

/// Decodes an atom, rejecting strings containing several notation tokens.
pub(super) fn atom(arena: &Arena, value_meta: Value) -> Result<ast::Atom, Error> {
    let text = get::text(arena, &value_meta)?;
    let span = *arena.value.span(value_meta.span);
    // Delimiter atoms cannot be parsed outside their matching pair
    let atom_kind = match text {
        "`<" => Atom::LAngle,
        "`>" => Atom::RAngle,
        "`(" => Atom::LParen,
        "`)" => Atom::RParen,
        "`[" => Atom::LBrack,
        "`]" => Atom::RBrack,
        "`{" => Atom::LBrace,
        "`}" => Atom::RBrace,
        _ => decode_atom(arena, value_meta, text, span)?,
    };
    Ok(phrase!(node: atom_kind, span: span))
}

fn decode_atom(arena: &Arena, value_meta: Value, text: &str, span: Span) -> Result<Atom, Error> {
    match parse_mixop(text).map_err(|report| Error::from(report).at(span))? {
        Mixop::Atom(atom_parsed) => Ok(atom_parsed.node),
        _ => Err(Error::unexpected(arena, value_meta, "one notation atom")),
    }
}

/// Reconstructs notation from atoms between argument positions.
pub(super) fn mixop(arena: &Arena, value_meta: Value) -> Result<Rc<Mixop>, Error> {
    let atoms_rows = list(arena, value_meta, |arena, value_row| list(arena, value_row, atom))?;
    let span = *arena.value.span(value_meta.span);
    // A matrix with n holes has n + 1 rows, including empty boundary rows
    if atoms_rows.is_empty() {
        return Err(Error::unexpected(arena, value_meta, "a nonempty atom matrix"));
    }
    // Preserve empty notation as the single empty boundary row
    if atoms_rows.len() == 1 && atoms_rows[0].is_empty() {
        return Ok(Rc::new(Mixop::Seq(vec![])));
    }
    // Insert one parser argument between each pair of boundary rows
    let text = atoms_rows
        .iter()
        .map(|atoms| {
            atoms
                .iter()
                .map(Print::to_string)
                .collect::<Vec<_>>()
                .join(" ")
        })
        .collect::<Vec<_>>()
        .join(" nat ");
    let mixop_parsed = parse_mixop(&text).map_err(|report| Error::from(report).at(span))?;
    // Atom text must not inject additional argument positions
    if mixop_parsed.arity() + 1 != atoms_rows.len() {
        return Err(Error::unexpected(
            arena,
            value_meta,
            "an atom matrix with one row per argument boundary",
        ));
    }
    Ok(Rc::new(mixop_parsed))
}

/// Decodes an iterator with its nullary constructor checked.
pub(super) fn iter(arena: &Arena, value_meta: Value) -> Result<Iter, Error> {
    let (mixop_meta, values_meta, _) = case(arena, value_meta)?;
    match values_meta.as_slice() {
        [] if m::is(&mixop_meta, m::QUEST) => Ok(Iter::Opt),
        [] if m::is(&mixop_meta, m::STAR) => Ok(Iter::List),
        _ => Err(Error::unexpected(arena, value_meta, "QUEST or STAR")),
    }
}

/// Decodes a variable's underlying type and dimensions.
pub fn var(arena: &Arena, value_meta: Value) -> Result<ast::Var, Error> {
    let (mixop_meta, values_meta, _) = case(arena, value_meta)?;
    match values_meta.as_slice() {
        [value_id, value_typ, value_iters] if m::is(&mixop_meta, m::VARI) => {
            decode_var(arena, *value_id, *value_typ, *value_iters)
        }
        _ => Err(Error::unexpected(arena, value_meta, "id typ iter*")),
    }
}

/// Reconstructs a variable's type and iteration dimensions.
fn decode_var(
    arena: &Arena,
    value_id: Value,
    value_typ: Value,
    value_iters: Value,
) -> Result<ast::Var, Error> {
    Ok(ast::Var {
        id: id(arena, value_id)?,
        typ: typ(arena, value_typ)?,
        iters: list(arena, value_iters, iter)?,
    })
}

/// Maps a checked optional meta value.
pub(super) fn opt<T>(
    arena: &Arena,
    value_meta: Value,
    func_decode: impl FnOnce(&Arena, Value) -> Result<T, Error>,
) -> Result<Option<T>, Error> {
    get::opt(arena, &value_meta)?
        .map(|value_inner| func_decode(arena, value_inner))
        .transpose()
}
