//! Decodes AL meta data with constructor and payload checks
//!
//! Source syntax only borrows the arena. Runtime-value decoding allocates in
//! that same arena and copies child handles before recursive allocation.

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
    decode: impl Fn(&Arena, Value) -> Result<T, Error>,
) -> Result<Vec<T>, Error> {
    get::list(arena, &value_meta)?
        .iter()
        .map(|value| decode(arena, *value))
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
        _ => match parse_mixop(text).map_err(|report| Error::from(report).at(span))? {
            Mixop::Atom(atom) => atom.node,
            _ => return Err(Error::unexpected(arena, value_meta, "one notation atom")),
        },
    };
    Ok(phrase!(node: atom_kind, span: span))
}

/// Reconstructs notation from atoms between argument positions.
pub(super) fn mixop(arena: &Arena, value_meta: Value) -> Result<Rc<Mixop>, Error> {
    let rows = list(arena, value_meta, |arena, value| list(arena, value, atom))?;
    let span = *arena.value.span(value_meta.span);
    // A matrix with n holes has n + 1 rows, including empty boundary rows
    if rows.is_empty() {
        return Err(Error::unexpected(arena, value_meta, "a nonempty atom matrix"));
    }
    if rows.len() == 1 && rows[0].is_empty() {
        return Ok(Rc::new(Mixop::Seq(vec![])));
    }
    let text = rows
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
    let mixop = parse_mixop(&text).map_err(|report| Error::from(report).at(span))?;
    // Atom text must not inject additional argument positions
    if mixop.arity() + 1 != rows.len() {
        return Err(Error::unexpected(
            arena,
            value_meta,
            "an atom matrix with one row per argument boundary",
        ));
    }
    Ok(Rc::new(mixop))
}

/// Decodes an iterator with its nullary constructor checked.
pub(super) fn iter(arena: &Arena, value_meta: Value) -> Result<Iter, Error> {
    let (mixop, values, _) = case(arena, value_meta)?;
    match values.as_slice() {
        [] if m::is(&mixop, m::QUEST) => Ok(Iter::Opt),
        [] if m::is(&mixop, m::STAR) => Ok(Iter::List),
        _ => Err(Error::unexpected(arena, value_meta, "QUEST or STAR")),
    }
}

/// Decodes a variable's underlying type and dimensions.
pub fn var(arena: &Arena, value_meta: Value) -> Result<ast::Var, Error> {
    let (mixop, values, _) = case(arena, value_meta)?;
    match values.as_slice() {
        [value_id, value_typ, value_iters] if m::is(&mixop, m::VARI) => Ok(ast::Var {
            id: id(arena, *value_id)?,
            typ: typ(arena, *value_typ)?,
            iters: list(arena, *value_iters, iter)?,
        }),
        _ => Err(Error::unexpected(arena, value_meta, "id typ iter*")),
    }
}

/// Maps a checked optional meta value.
pub(super) fn opt<T>(
    arena: &Arena,
    value_meta: Value,
    decode: impl FnOnce(&Arena, Value) -> Result<T, Error>,
) -> Result<Option<T>, Error> {
    get::opt(arena, &value_meta)?
        .map(|value| decode(arena, value))
        .transpose()
}
