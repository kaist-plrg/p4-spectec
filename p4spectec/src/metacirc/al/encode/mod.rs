//! Encodes source AL data in a borrowed runtime arena
//!
//! `spec`, `exp`, `typ`, and `value` recursively encode their children
//! Helpers then construct typed lists, options, and meta cases
//! Every conversion borrows the supplied arena without resetting it

mod def;
mod exp;
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

pub use def::{def, spec};
pub use exp::{exp, exps};

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
    values_meta: Vec<Value>,
    span: Span,
) -> Result<Value, Error> {
    let mixfix = Mixfix::new(parse::mixop(shape), values_meta)?;
    Ok(make::case(arena, typs::named(name), mixfix, span)?)
}

/// Maps a source list and annotates its meta element type.
pub(super) fn list<T>(
    arena: &mut Arena,
    name: &str,
    source: &[T],
    mut func_encode: impl FnMut(&mut Arena, &T) -> Result<Value, Error>,
) -> Result<Value, Error> {
    let values_meta = source
        .iter()
        .map(|value_source| func_encode(arena, value_source))
        .collect::<Result<_, _>>()?;
    Ok(make::list(arena, typs::iter(name, Iter::List), values_meta, Span::default())?)
}

/// Maps an optional source value and retains its meta element type.
pub(super) fn opt<T>(
    arena: &mut Arena,
    name: &str,
    source: Option<&T>,
    func_encode: impl FnOnce(&mut Arena, &T) -> Result<Value, Error>,
) -> Result<Value, Error> {
    let value_meta = source
        .map(|value_source| func_encode(arena, value_source))
        .transpose()?;
    Ok(make::opt(arena, typs::iter(name, Iter::Opt), value_meta, Span::default())?)
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
    // Split atoms at argument holes, including empty boundary rows
    let mut atoms_rows_al = Vec::new();
    let mut atoms_tail_al = Vec::new();
    split_mixop(mixop_al, &mut atoms_tail_al, &mut atoms_rows_al);
    atoms_rows_al.push(atoms_tail_al);
    // Encode every row before allocating the outer matrix
    let mut values_meta = Vec::with_capacity(atoms_rows_al.len());
    for atoms_row_al in atoms_rows_al {
        values_meta
            .push(list(arena, "atom", &atoms_row_al, |arena, atom_al| atom(arena, atom_al))?);
    }
    // Retain both list dimensions even when a row is empty
    let typ_matrix = crate::lang::data::typ::make::list(crate::lang::data::typ::make::list(
        crate::lang::data::typ::make::text(),
    ));
    Ok(make::list(arena, std::rc::Rc::new(typ_matrix.node), values_meta, Span::default())?)
}

fn split_mixop<'a>(
    mixop_al: &'a Mixop,
    atoms_tail_al: &mut Vec<&'a ast::Atom>,
    atoms_rows_al: &mut Vec<Vec<&'a ast::Atom>>,
) {
    match mixop_al {
        Mixop::Arg => atoms_rows_al.push(std::mem::take(atoms_tail_al)),
        Mixop::Atom(atom_al) => atoms_tail_al.push(atom_al),
        Mixop::Brack(atom_l_al, mixop_inner_al, atom_r_al) => {
            split_brack_mixop(atom_l_al, mixop_inner_al, atom_r_al, atoms_tail_al, atoms_rows_al)
        }
        Mixop::Infix(mixop_l_al, atom_al, mixop_r_al) => {
            split_infix_mixop(mixop_l_al, atom_al, mixop_r_al, atoms_tail_al, atoms_rows_al)
        }
        Mixop::Seq(mixops_al) => split_seq_mixop(mixops_al, atoms_tail_al, atoms_rows_al),
    }
}

/// Collects a bracketed notation's atoms around its inner arguments.
fn split_brack_mixop<'a>(
    atom_l_al: &'a ast::Atom,
    mixop_inner_al: &'a Mixop,
    atom_r_al: &'a ast::Atom,
    atoms_tail_al: &mut Vec<&'a ast::Atom>,
    atoms_rows_al: &mut Vec<Vec<&'a ast::Atom>>,
) {
    atoms_tail_al.push(atom_l_al);
    split_mixop(mixop_inner_al, atoms_tail_al, atoms_rows_al);
    atoms_tail_al.push(atom_r_al);
}

/// Collects infix notation in source order around its argument holes.
fn split_infix_mixop<'a>(
    mixop_l_al: &'a Mixop,
    atom_al: &'a ast::Atom,
    mixop_r_al: &'a Mixop,
    atoms_tail_al: &mut Vec<&'a ast::Atom>,
    atoms_rows_al: &mut Vec<Vec<&'a ast::Atom>>,
) {
    split_mixop(mixop_l_al, atoms_tail_al, atoms_rows_al);
    atoms_tail_al.push(atom_al);
    split_mixop(mixop_r_al, atoms_tail_al, atoms_rows_al);
}

fn split_seq_mixop<'a>(
    mixops_al: &'a [Mixop],
    atoms_tail_al: &mut Vec<&'a ast::Atom>,
    atoms_rows_al: &mut Vec<Vec<&'a ast::Atom>>,
) {
    for mixop_al in mixops_al {
        split_mixop(mixop_al, atoms_tail_al, atoms_rows_al);
    }
}

/// Encodes a successful function result as `OK val : valres`.
pub fn ok_value(arena: &mut Arena, value_target: Value) -> Result<Value, Error> {
    let span = *arena.value.span(value_target.span);
    let value_meta = value(arena, value_target)?;
    case(arena, mixops::OK, "valres", vec![value_meta], span)
}

/// Encodes successful relation outputs as `OK val* : valsres`.
pub fn ok_values(arena: &mut Arena, values_target: &[Value]) -> Result<Value, Error> {
    let values_meta = values(arena, values_target)?;
    case(arena, mixops::OK, "valsres", vec![values_meta], Span::default())
}
