//! Encodes source AL data in a borrowed runtime arena
//!
//! Helpers construct typed lists, options, and meta cases after recursively
//! encoding their children. No conversion resets or owns the arena.

mod typ;
mod value;

use super::{mixops, typs};
use crate::{
    lang::{
        common::{Id, Iter, source::Span},
        data::{
            arena::Arena,
            notation::tree::{Mixfix, Mixop, parse},
            value::flat::{Value, make},
        },
        il::ast,
        traits::print::Print,
    },
    metacirc::Error,
};

pub use typ::{deftyp, typ, typs};
pub use value::{value, values};

/// Encodes an identifier as text, retaining its span.
pub fn id(arena: &mut Arena, id_al: &Id) -> Result<Value, Error> {
    Ok(make::text(arena, id_al.node.to_string(), id_al.span)?)
}

/// Encodes a notation atom by its source spelling.
pub(super) fn atom(arena: &mut Arena, atom_al: &ast::Atom) -> Result<Value, Error> {
    Ok(make::text(arena, atom_al.to_string(), atom_al.span)?)
}

/// Constructs a typed meta case after its children have been encoded.
pub(super) fn case(
    arena: &mut Arena,
    shape: &str,
    name: &str,
    values: Vec<Value>,
    span: Span,
) -> Result<Value, Error> {
    let mixfix = Mixfix::new(parse::mixop(shape), values)?;
    Ok(make::case(arena, typs::named(name), mixfix, span)?)
}

/// Maps a source list and annotates its meta element type.
pub(super) fn list<T>(
    arena: &mut Arena,
    name: &str,
    source: &[T],
    mut encode: impl FnMut(&mut Arena, &T) -> Result<Value, Error>,
) -> Result<Value, Error> {
    let values = source
        .iter()
        .map(|value| encode(arena, value))
        .collect::<Result<_, _>>()?;
    Ok(make::list(arena, typs::iter(name, Iter::List), values, Span::default())?)
}

/// Maps an optional source value and retains its meta element type.
pub(super) fn opt<T>(
    arena: &mut Arena,
    name: &str,
    source: Option<&T>,
    encode: impl FnOnce(&mut Arena, &T) -> Result<Value, Error>,
) -> Result<Value, Error> {
    let value = source.map(|value| encode(arena, value)).transpose()?;
    Ok(make::opt(arena, typs::iter(name, Iter::Opt), value, Span::default())?)
}

/// Encodes a source iterator.
pub(super) fn iter(arena: &mut Arena, iter_al: &Iter) -> Result<Value, Error> {
    let shape = match iter_al {
        Iter::Opt => mixops::QUEST,
        Iter::List => mixops::STAR,
    };
    case(arena, shape, "iter", vec![], Span::default())
}

/// Encodes a source variable with its underlying type and dimensions.
pub fn var(arena: &mut Arena, var_al: &ast::Var) -> Result<Value, Error> {
    let value_id = id(arena, &var_al.id)?;
    let value_typ = typ(arena, &var_al.typ)?;
    let value_iters = list(arena, "iter", &var_al.iters, iter)?;
    case(arena, mixops::VARI, "vari", vec![value_id, value_typ, value_iters], var_al.id.span)
}

/// Encodes atoms between argument positions as a nonempty matrix.
pub(super) fn mixop(arena: &mut Arena, mixop_al: &Mixop) -> Result<Value, Error> {
    fn split<'a>(
        mixop: &'a Mixop,
        atoms: &mut Vec<&'a ast::Atom>,
        rows: &mut Vec<Vec<&'a ast::Atom>>,
    ) {
        match mixop {
            Mixop::Arg => rows.push(std::mem::take(atoms)),
            Mixop::Atom(atom) => atoms.push(atom),
            Mixop::Brack(atom_l, mixop, atom_r) => {
                atoms.push(atom_l);
                split(mixop, atoms, rows);
                atoms.push(atom_r);
            }
            Mixop::Infix(mixop_l, atom, mixop_r) => {
                split(mixop_l, atoms, rows);
                atoms.push(atom);
                split(mixop_r, atoms, rows);
            }
            Mixop::Seq(mixops) => {
                for mixop in mixops {
                    split(mixop, atoms, rows);
                }
            }
        }
    }
    // Keep empty rows: each boundary represents one argument hole
    let mut rows = Vec::new();
    let mut atoms = Vec::new();
    split(mixop_al, &mut atoms, &mut rows);
    rows.push(atoms);
    let mut values = Vec::with_capacity(rows.len());
    for atoms in rows {
        values.push(list(arena, "atom", &atoms, |arena, atom_al| atom(arena, atom_al))?);
    }
    let typ = crate::lang::data::typ::make::list(crate::lang::data::typ::make::list(
        crate::lang::data::typ::make::text(),
    ));
    Ok(make::list(arena, std::rc::Rc::new(typ.node), values, Span::default())?)
}
