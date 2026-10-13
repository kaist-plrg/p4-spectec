//! Decodes AL meta data into source syntax and runtime values
//!
//! `Decode` implementations check constructor handles in the supplied arena
//! Syntax is reconstructed canonically for editing and re-encoding
//! Runtime payload decoding allocates only in that same arena

use super::{Error, mixops as m, stub};
use crate::lang::common::prim::num::Number;
use crate::{
    frontend::parse::parse_mixop,
    lang::{
        al::ast as al,
        common::{
            Id, Iter,
            notation::atom::Atom,
            prim::{bool as boolean, num},
            source::Span,
        },
        data::{
            arena::Arena,
            notation::{
                flat::Mixop as FlatMixop,
                tree::{Mixfix as TreeMixfix, Mixop},
            },
            typ::make as make_typ,
            value::{
                ValueError,
                flat::{Value, ValueField, get, make},
            },
        },
        il::ast as il,
        traits::print::Print,
    },
    note_phrase, phrase,
};
use num_traits::ToPrimitive;
use std::rc::Rc;

/// Reconstructs source AL data from a meta value.
pub trait Decode: Sized {
    /// Decodes a value using the arena that owns its handles.
    fn decode(arena: &mut Arena, value_meta: Value) -> Result<Self, Error>;
}

// == Helpers

/// Copies a case's children before recursive decoding or allocation.
fn case(arena: &Arena, value_meta: Value) -> Result<(Vec<Value>, Span), Error> {
    let value_case = get::case(arena, &value_meta)?;
    Ok((value_case.args().to_vec(), *arena.value.span(value_meta.span)))
}

// == Decoding

// - Containers

/// Decodes a checked meta list element by element.
impl<T: Decode> Decode for Vec<T> {
    fn decode(arena: &mut Arena, value_meta: Value) -> Result<Self, Error> {
        get::list(arena, &value_meta)?
            .to_vec()
            .into_iter()
            .map(|value_elem| T::decode(arena, value_elem))
            .collect()
    }
}

/// Decodes a checked optional meta value.
impl<T: Decode> Decode for Option<T> {
    fn decode(arena: &mut Arena, value_meta: Value) -> Result<Self, Error> {
        get::opt(arena, &value_meta)?
            .map(|value_inner| T::decode(arena, value_inner))
            .transpose()
    }
}

// - Identifiers

/// Decodes an owned identifier from a meta text value.
impl Decode for Id {
    fn decode(arena: &mut Arena, value_meta: Value) -> Result<Self, Error> {
        Ok(
            phrase!(node: get::text(arena, &value_meta)?.into(), span: *arena.value.span(value_meta.span)),
        )
    }
}

// - Atoms

/// Decodes an atom, rejecting strings containing several notation tokens.
impl Decode for il::Atom {
    fn decode(arena: &mut Arena, value_meta: Value) -> Result<Self, Error> {
        let text_meta = get::text(arena, &value_meta)?;
        let span = *arena.value.span(value_meta.span);
        // Delimiter atoms cannot be parsed outside their matching pair
        let atom_kind = match text_meta {
            "`<" => Atom::LAngle,
            "`>" => Atom::RAngle,
            "`(" => Atom::LParen,
            "`)" => Atom::RParen,
            "`[" => Atom::LBrack,
            "`]" => Atom::RBrack,
            "`{" => Atom::LBrace,
            "`}" => Atom::RBrace,
            _ => match parse_mixop(text_meta).map_err(|report| Error::from(report).at(span))? {
                Mixop::Atom(atom_parsed) => atom_parsed.node,
                _ => return Err(Error::unexpected(arena, value_meta, "one notation atom")),
            },
        };
        Ok(phrase!(node: atom_kind, span: span))
    }
}

// - Mixops

/// Reconstructs notation from atoms between argument positions.
impl Decode for Rc<Mixop> {
    fn decode(arena: &mut Arena, value_meta: Value) -> Result<Self, Error> {
        let atoms_rows = Vec::<Vec<il::Atom>>::decode(arena, value_meta)?;
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
        let text_mixop = atoms_rows
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
        let mixop_al = parse_mixop(&text_mixop).map_err(|report| Error::from(report).at(span))?;
        // Atom text must not inject additional argument positions
        if mixop_al.arity() + 1 != atoms_rows.len() {
            return Err(Error::unexpected(
                arena,
                value_meta,
                "an atom matrix with one row per argument boundary",
            ));
        }
        Ok(Rc::new(mixop_al))
    }
}

/// Interns decoded notation in the caller's arena.
impl Decode for FlatMixop {
    fn decode(arena: &mut Arena, value_meta: Value) -> Result<Self, Error> {
        let mixop_al = Rc::<Mixop>::decode(arena, value_meta)?;
        Ok(arena
            .mixop
            .intern(mixop_al.as_ref())
            .map_err(ValueError::from)?)
    }
}

// - Mixfixes

/// Decodes a type notation case.
impl Decode for TreeMixfix<il::Typ> {
    fn decode(arena: &mut Arena, value_meta: Value) -> Result<Self, Error> {
        let (values_meta, _) = case(arena, value_meta)?;
        match values_meta.as_slice() {
            [value_mixop, values_arg] if get::matches!(arena, &value_meta, m::TYPCASE) => {
                Ok(TreeMixfix::new(
                    Rc::<Mixop>::decode(arena, *value_mixop)?,
                    Vec::<il::Typ>::decode(arena, *values_arg)?,
                )?)
            }
            _ => Err(Error::unexpected(arena, value_meta, "mixop elem*")),
        }
    }
}

/// Decodes a runtime value notation case.
impl Decode for TreeMixfix<Value> {
    fn decode(arena: &mut Arena, value_meta: Value) -> Result<Self, Error> {
        let (values_meta, _) = case(arena, value_meta)?;
        match values_meta.as_slice() {
            [value_mixop, values_arg] if get::matches!(arena, &value_meta, m::VALCASE) => {
                Ok(TreeMixfix::new(
                    Rc::<Mixop>::decode(arena, *value_mixop)?,
                    Vec::<Value>::decode(arena, *values_arg)?,
                )?)
            }
            _ => Err(Error::unexpected(arena, value_meta, "mixop elem*")),
        }
    }
}

/// Decodes an expression notation case.
impl Decode for TreeMixfix<il::Exp> {
    fn decode(arena: &mut Arena, value_meta: Value) -> Result<Self, Error> {
        let (values_meta, _) = case(arena, value_meta)?;
        match values_meta.as_slice() {
            [value_mixop, values_arg] if get::matches!(arena, &value_meta, m::EXPCASE) => {
                Ok(TreeMixfix::new(
                    Rc::<Mixop>::decode(arena, *value_mixop)?,
                    Vec::<il::Exp>::decode(arena, *values_arg)?,
                )?)
            }
            _ => Err(Error::unexpected(arena, value_meta, "mixop elem*")),
        }
    }
}

// - Iterators

/// Decodes an iterator with its nullary constructor checked.
impl Decode for Iter {
    fn decode(arena: &mut Arena, value_meta: Value) -> Result<Self, Error> {
        let (values_meta, _) = case(arena, value_meta)?;
        match values_meta.as_slice() {
            [] if get::matches!(arena, &value_meta, m::QUEST) => Ok(Iter::Opt),
            [] if get::matches!(arena, &value_meta, m::STAR) => Ok(Iter::List),
            _ => Err(Error::unexpected(arena, value_meta, "QUEST or STAR")),
        }
    }
}

// - Variables

/// Decodes a variable's underlying type and dimensions.
impl Decode for il::Var {
    fn decode(arena: &mut Arena, value_meta: Value) -> Result<Self, Error> {
        let (values_meta, _) = case(arena, value_meta)?;
        match values_meta.as_slice() {
            [value_id, value_typ, value_iters] if get::matches!(arena, &value_meta, m::VARI) => {
                Ok(il::Var {
                    id: Id::decode(arena, *value_id)?,
                    typ: il::Typ::decode(arena, *value_typ)?,
                    iters: Vec::<Iter>::decode(arena, *value_iters)?,
                })
            }
            _ => Err(Error::unexpected(arena, value_meta, "id typ iter*")),
        }
    }
}

// - Types

/// Decodes a source type, checking every constructor's shape.
impl Decode for il::Typ {
    fn decode(arena: &mut Arena, value_meta: Value) -> Result<Self, Error> {
        let (values_meta, span) = case(arena, value_meta)?;
        let typ_kind_al = match values_meta.as_slice() {
            [] if get::matches!(arena, &value_meta, m::BOOL_TYP) => il::TypKind::Bool,
            [] if get::matches!(arena, &value_meta, m::NAT_TYP) => il::TypKind::Num(num::Typ::Nat),
            [] if get::matches!(arena, &value_meta, m::INT_TYP) => il::TypKind::Num(num::Typ::Int),
            [] if get::matches!(arena, &value_meta, m::TEXT_TYP) => il::TypKind::Text,
            [value_id, value_typs] if get::matches!(arena, &value_meta, m::VAR_TYP) => {
                il::TypKind::Var(
                    Id::decode(arena, *value_id)?,
                    Vec::<il::Typ>::decode(arena, *value_typs)?,
                )
            }
            [value_typs] if get::matches!(arena, &value_meta, m::TUP_TYP) => {
                il::TypKind::Tuple(Vec::<il::Typ>::decode(arena, *value_typs)?)
            }
            [value_typ, value_iter] if get::matches!(arena, &value_meta, m::ITER_TYP) => {
                il::TypKind::Iter(
                    Box::new(il::Typ::decode(arena, *value_typ)?),
                    Iter::decode(arena, *value_iter)?,
                )
            }
            [value_ids, value_typs, value_ret]
                if get::matches!(arena, &value_meta, m::FUNC_TYP) =>
            {
                il::TypKind::Func(il::FuncTyp {
                    tparams: Vec::<Id>::decode(arena, *value_ids)?,
                    typs_params: Vec::<il::Typ>::decode(arena, *value_typs)?,
                    typ_ret: Box::new(il::Typ::decode(arena, *value_ret)?),
                })
            }
            _ => {
                return Err(Error::unexpected(
                    arena,
                    value_meta,
                    "an AL type constructor with its declared arguments",
                ));
            }
        };
        Ok(phrase!(node: typ_kind_al, span: span))
    }
}

// - Defined types

/// Decodes an alias, struct, or variant without general hints.
impl Decode for il::DefTyp {
    fn decode(arena: &mut Arena, value_meta: Value) -> Result<Self, Error> {
        let (values_meta, span) = case(arena, value_meta)?;
        let deftyp_kind_al = match values_meta.as_slice() {
            [value_typ] if get::matches!(arena, &value_meta, m::ALIAS) => {
                il::DefTypKind::Plain(il::Typ::decode(arena, *value_typ)?)
            }
            [value_fields] if get::matches!(arena, &value_meta, m::STRUCT) => {
                il::DefTypKind::Struct(Vec::<il::TypField>::decode(arena, *value_fields)?)
            }
            [value_cases] if get::matches!(arena, &value_meta, m::VARIANT) => {
                il::DefTypKind::Variant(Vec::<il::TypCase>::decode(arena, *value_cases)?)
            }
            _ => return Err(Error::unexpected(arena, value_meta, "ALIAS, STRUCT, or VARIANT")),
        };
        Ok(phrase!(node: deftyp_kind_al, span: span))
    }
}

/// Decodes one struct type field.
impl Decode for il::TypField {
    fn decode(arena: &mut Arena, value_meta: Value) -> Result<Self, Error> {
        let (values_meta, _) = case(arena, value_meta)?;
        match values_meta.as_slice() {
            [value_atom, value_typ] if get::matches!(arena, &value_meta, m::TYPFIELD) => {
                Ok(il::TypField {
                    atom: il::Atom::decode(arena, *value_atom)?,
                    typ: il::Typ::decode(arena, *value_typ)?,
                })
            }
            _ => Err(Error::unexpected(arena, value_meta, "atom typ")),
        }
    }
}

/// Decodes one variant case without its origin or hints.
impl Decode for il::TypCase {
    fn decode(arena: &mut Arena, value_meta: Value) -> Result<Self, Error> {
        let span = *arena.value.span(value_meta.span);
        let mixfix_typ_al = TreeMixfix::<il::Typ>::decode(arena, value_meta)?;
        Ok(il::TypCase {
            not_typ: phrase!(node: mixfix_typ_al, span: span),
            typ_origin: stub::typ_origin(),
            hints: stub::hints(),
        })
    }
}

// - Values

/// Decodes a runtime payload, allocating only in the supplied arena.
impl Decode for Value {
    fn decode(arena: &mut Arena, value_meta: Value) -> Result<Self, Error> {
        let (values_meta, span) = case(arena, value_meta)?;
        let value_target = match values_meta.as_slice() {
            [value_bool] if get::matches!(arena, &value_meta, m::BOOL) => {
                let value_bool = get::bool(arena, value_bool)?;
                make::bool(arena, value_bool, span)?
            }
            [value_num] if get::matches!(arena, &value_meta, m::NAT) => {
                let Number::Nat(nat) = get::num(arena, value_num)? else {
                    return Err(Error::unexpected(arena, *value_num, "a natural-number payload"));
                };
                let nat_target = nat.clone();
                make::nat(arena, nat_target, span)?
            }
            [value_num] if get::matches!(arena, &value_meta, m::INT) => {
                let num_target = get::num(arena, value_num)?.clone();
                make::int(arena, num::to_int(&num_target).clone(), span)?
            }
            [value_text] if get::matches!(arena, &value_meta, m::TEXT) => {
                let text_target = get::text(arena, value_text)?.to_owned();
                make::text(arena, text_target, span)?
            }
            [value_fields] if get::matches!(arena, &value_meta, m::STR) => {
                let fields_target = Vec::<ValueField>::decode(arena, *value_fields)?;
                make::structure(arena, stub::value_typ().node.into(), fields_target, span)?
            }
            [value_case] if get::matches!(arena, &value_meta, m::INJ) => {
                let mixfix_target = TreeMixfix::<Value>::decode(arena, *value_case)?;
                make::case(arena, stub::value_typ().node.into(), mixfix_target, span)?
            }
            [value_values] if get::matches!(arena, &value_meta, m::TUP) => {
                let values_target = Vec::<Value>::decode(arena, *value_values)?;
                make::tuple(arena, stub::value_typ().node.into(), values_target, span)?
            }
            [value_opt] if get::matches!(arena, &value_meta, m::OPT) => {
                let value_target = Option::<Value>::decode(arena, *value_opt)?;
                make::opt(arena, make_typ::opt(stub::value_typ()).node.into(), value_target, span)?
            }
            [value_values] if get::matches!(arena, &value_meta, m::LIST) => {
                let values_target = Vec::<Value>::decode(arena, *value_values)?;
                make::list(
                    arena,
                    make_typ::list(stub::value_typ()).node.into(),
                    values_target,
                    span,
                )?
            }
            [value_id] if get::matches!(arena, &value_meta, m::FUNC) => {
                let id_target = Id::decode(arena, *value_id)?;
                let (tparams_target, typs_target, typ_target) = stub::func_typ_params();
                make::func(arena, id_target, tparams_target, typs_target, typ_target, span)?
            }
            [value_json] if get::matches!(arena, &value_meta, m::EXT) => {
                let json_target = get::external(arena, value_json)?.clone();
                make::external(arena, stub::json_typ().node.into(), json_target, span)?
            }
            _ => {
                return Err(Error::unexpected(
                    arena,
                    value_meta,
                    "an AL value constructor with its declared arguments",
                ));
            }
        };
        Ok(value_target)
    }
}

/// Decodes one runtime struct field.
impl Decode for ValueField {
    fn decode(arena: &mut Arena, value_meta: Value) -> Result<Self, Error> {
        let (values_meta, _) = case(arena, value_meta)?;
        match values_meta.as_slice() {
            [value_atom, value_inner] if get::matches!(arena, &value_meta, m::VALFIELD) => {
                Ok(ValueField {
                    atom: il::Atom::decode(arena, *value_atom)?,
                    value: Value::decode(arena, *value_inner)?,
                })
            }
            _ => Err(Error::unexpected(arena, value_meta, "atom val")),
        }
    }
}

// - Expressions

/// Reconstructs an expression suitable for editing and re-encoding.
impl Decode for il::Exp {
    fn decode(arena: &mut Arena, value_meta: Value) -> Result<Self, Error> {
        let (values_meta, span) = case(arena, value_meta)?;
        let exp_kind_al = match values_meta.as_slice() {
            [value_bool] if get::matches!(arena, &value_meta, m::BOOL) => {
                il::ExpKind::Bool(get::bool(arena, value_bool)?)
            }
            [value_num] if get::matches!(arena, &value_meta, m::NAT) => {
                let Number::Nat(nat) = get::num(arena, value_num)? else {
                    return Err(Error::unexpected(arena, *value_num, "a natural-number payload"));
                };
                il::ExpKind::Num(Number::Nat(nat.clone()))
            }
            [value_num] if get::matches!(arena, &value_meta, m::INT) => {
                il::ExpKind::Num(Number::Int(num::to_int(get::num(arena, value_num)?).clone()))
            }
            [value_text] if get::matches!(arena, &value_meta, m::TEXT) => {
                il::ExpKind::Text(get::text(arena, value_text)?.into())
            }
            [value_id] if get::matches!(arena, &value_meta, m::VAR_EXP) => {
                il::ExpKind::Id(Id::decode(arena, *value_id)?)
            }
            [value_op, value_exp] if get::matches!(arena, &value_meta, m::UN) => {
                let (op_al, typ_op_al) = <(il::UnOp, il::OpTyp)>::decode(arena, *value_op)?;
                il::ExpKind::Un(op_al, typ_op_al, Box::new(il::Exp::decode(arena, *value_exp)?))
            }
            [value_op, value_l, value_r] if get::matches!(arena, &value_meta, m::BIN) => {
                let (op_al, typ_op_al) = <(il::BinOp, il::OpTyp)>::decode(arena, *value_op)?;
                il::ExpKind::Bin(
                    op_al,
                    typ_op_al,
                    Box::new(il::Exp::decode(arena, *value_l)?),
                    Box::new(il::Exp::decode(arena, *value_r)?),
                )
            }
            [value_op, value_l, value_r] if get::matches!(arena, &value_meta, m::CMP) => {
                let (op_al, typ_op_al) = <(il::CmpOp, il::OpTyp)>::decode(arena, *value_op)?;
                il::ExpKind::Cmp(
                    op_al,
                    typ_op_al,
                    Box::new(il::Exp::decode(arena, *value_l)?),
                    Box::new(il::Exp::decode(arena, *value_r)?),
                )
            }
            [value_typ, value_exp] if get::matches!(arena, &value_meta, m::UPCAST) => {
                il::ExpKind::UpCast(
                    Box::new(il::Typ::decode(arena, *value_typ)?),
                    Box::new(il::Exp::decode(arena, *value_exp)?),
                )
            }
            [value_typ, value_exp] if get::matches!(arena, &value_meta, m::DOWNCAST) => {
                il::ExpKind::DownCast(
                    Box::new(il::Typ::decode(arena, *value_typ)?),
                    Box::new(il::Exp::decode(arena, *value_exp)?),
                )
            }
            [value_exp, value_typ] if get::matches!(arena, &value_meta, m::SUB_EXP) => {
                let typ_al = il::Typ::decode(arena, *value_typ)?;
                let subcheck_al = stub::subcheck(typ_al.clone());
                il::ExpKind::Sub(
                    Box::new(il::Exp::decode(arena, *value_exp)?),
                    Box::new(typ_al),
                    Box::new(subcheck_al),
                )
            }
            [value_exp, value_pattern] if get::matches!(arena, &value_meta, m::MATCH) => {
                il::ExpKind::Match(
                    Box::new(il::Exp::decode(arena, *value_exp)?),
                    il::Pattern::decode(arena, *value_pattern)?,
                )
            }
            [value_exps] if get::matches!(arena, &value_meta, m::TUP) => {
                il::ExpKind::Tuple(Vec::<il::Exp>::decode(arena, *value_exps)?)
            }
            [value_case] if get::matches!(arena, &value_meta, m::INJ) => {
                il::ExpKind::Case(Box::new(TreeMixfix::<il::Exp>::decode(arena, *value_case)?))
            }
            [value_fields] if get::matches!(arena, &value_meta, m::STR) => {
                il::ExpKind::Str(Vec::<il::ExpField>::decode(arena, *value_fields)?)
            }
            [value_opt] if get::matches!(arena, &value_meta, m::OPT) => {
                il::ExpKind::Opt(Option::<il::Exp>::decode(arena, *value_opt)?.map(Box::new))
            }
            [value_exps] if get::matches!(arena, &value_meta, m::LIST) => {
                il::ExpKind::List(Vec::<il::Exp>::decode(arena, *value_exps)?)
            }
            [value_head, value_tail] if get::matches!(arena, &value_meta, m::CONS) => {
                il::ExpKind::Cons(
                    Box::new(il::Exp::decode(arena, *value_head)?),
                    Box::new(il::Exp::decode(arena, *value_tail)?),
                )
            }
            [value_l, value_r] if get::matches!(arena, &value_meta, m::CAT) => il::ExpKind::Cat(
                Box::new(il::Exp::decode(arena, *value_l)?),
                Box::new(il::Exp::decode(arena, *value_r)?),
            ),
            [value_elem, value_list] if get::matches!(arena, &value_meta, m::MEM) => {
                il::ExpKind::Mem(
                    Box::new(il::Exp::decode(arena, *value_elem)?),
                    Box::new(il::Exp::decode(arena, *value_list)?),
                )
            }
            [value_exp] if get::matches!(arena, &value_meta, m::LEN) => {
                il::ExpKind::Len(Box::new(il::Exp::decode(arena, *value_exp)?))
            }
            [value_base, value_atom] if get::matches!(arena, &value_meta, m::DOT) => {
                il::ExpKind::Dot(
                    Box::new(il::Exp::decode(arena, *value_base)?),
                    il::Atom::decode(arena, *value_atom)?,
                )
            }
            [value_base, value_idx] if get::matches!(arena, &value_meta, m::IDX) => {
                il::ExpKind::Idx(
                    Box::new(il::Exp::decode(arena, *value_base)?),
                    Box::new(il::Exp::decode(arena, *value_idx)?),
                )
            }
            [value_base, value_idx, value_len] if get::matches!(arena, &value_meta, m::SLICE) => {
                il::ExpKind::Slice(
                    Box::new(il::Exp::decode(arena, *value_base)?),
                    Box::new(il::Exp::decode(arena, *value_idx)?),
                    Box::new(il::Exp::decode(arena, *value_len)?),
                )
            }
            [value_base, value_path, value_upd] if get::matches!(arena, &value_meta, m::UPD) => {
                il::ExpKind::Upd(
                    Box::new(il::Exp::decode(arena, *value_base)?),
                    Box::new(il::Path::decode(arena, *value_path)?),
                    Box::new(il::Exp::decode(arena, *value_upd)?),
                )
            }
            [value_id, value_typs, value_args] if get::matches!(arena, &value_meta, m::CALL) => {
                il::ExpKind::Call(
                    Id::decode(arena, *value_id)?,
                    Vec::<il::Typ>::decode(arena, *value_typs)?,
                    Vec::<il::Arg>::decode(arena, *value_args)?,
                )
            }
            [value_exp, value_iter] if get::matches!(arena, &value_meta, m::ITER_EXP) => {
                il::ExpKind::Iter(
                    Box::new(il::Exp::decode(arena, *value_exp)?),
                    il::ExpIter::decode(arena, *value_iter)?,
                )
            }
            _ => {
                return Err(Error::unexpected(
                    arena,
                    value_meta,
                    "an AL expression constructor with its declared arguments",
                ));
            }
        };
        Ok(note_phrase!(node: exp_kind_al, note: stub::exp_note(), span: span))
    }
}

/// Decodes one source struct-expression field.
impl Decode for il::ExpField {
    fn decode(arena: &mut Arena, value_meta: Value) -> Result<Self, Error> {
        let (values_meta, _) = case(arena, value_meta)?;
        match values_meta.as_slice() {
            [value_atom, value_exp] if get::matches!(arena, &value_meta, m::EXPFIELD) => {
                Ok(il::ExpField {
                    atom: il::Atom::decode(arena, *value_atom)?,
                    exp: il::Exp::decode(arena, *value_exp)?,
                })
            }
            _ => Err(Error::unexpected(arena, value_meta, "atom exp")),
        }
    }
}

/// Decodes an expression iterator and its variables.
impl Decode for il::ExpIter {
    fn decode(arena: &mut Arena, value_meta: Value) -> Result<Self, Error> {
        let (values_meta, _) = case(arena, value_meta)?;
        match values_meta.as_slice() {
            [value_iter, value_vars] if get::matches!(arena, &value_meta, m::ITEREXP) => {
                Ok(il::ExpIter {
                    iter: Iter::decode(arena, *value_iter)?,
                    vars: Vec::<il::Var>::decode(arena, *value_vars)?,
                })
            }
            _ => Err(Error::unexpected(arena, value_meta, "iter vari*")),
        }
    }
}

/// Decodes a value or function argument.
impl Decode for il::Arg {
    fn decode(arena: &mut Arena, value_meta: Value) -> Result<Self, Error> {
        let (values_meta, span) = case(arena, value_meta)?;
        let arg_kind_al = match values_meta.as_slice() {
            [value_exp] if get::matches!(arena, &value_meta, m::EXP_ARG) => {
                il::ArgKind::Exp(Box::new(il::Exp::decode(arena, *value_exp)?))
            }
            [value_id] if get::matches!(arena, &value_meta, m::FUN_ARG) => {
                il::ArgKind::Def(Id::decode(arena, *value_id)?)
            }
            _ => return Err(Error::unexpected(arena, value_meta, "EXP exp or FUN id")),
        };
        Ok(phrase!(node: arg_kind_al, span: span))
    }
}

// - Patterns

/// Decodes a case, list, or option shape pattern.
impl Decode for il::Pattern {
    fn decode(arena: &mut Arena, value_meta: Value) -> Result<Self, Error> {
        let (values_meta, _) = case(arena, value_meta)?;
        Ok(match values_meta.as_slice() {
            [value_mixop] if get::matches!(arena, &value_meta, m::CASE_PATTERN) => {
                il::Pattern::Case(Rc::<Mixop>::decode(arena, *value_mixop)?)
            }
            [] if get::matches!(arena, &value_meta, m::CONS_PATTERN) => {
                il::Pattern::List(il::ListPattern::Cons)
            }
            [value_nat] if get::matches!(arena, &value_meta, m::FIXED) => {
                let Number::Nat(nat) = get::num(arena, value_nat)? else {
                    return Err(Error::unexpected(arena, *value_nat, "a natural list length"));
                };
                let num_len = nat.as_bigint().to_usize().ok_or_else(|| {
                    Error::unexpected(arena, *value_nat, "a list length fitting usize")
                })?;
                il::Pattern::List(il::ListPattern::Fixed(num_len))
            }
            [] if get::matches!(arena, &value_meta, m::NIL) => {
                il::Pattern::List(il::ListPattern::Nil)
            }
            [] if get::matches!(arena, &value_meta, m::SOME) => {
                il::Pattern::Opt(il::OptPattern::Some)
            }
            [] if get::matches!(arena, &value_meta, m::NONE) => {
                il::Pattern::Opt(il::OptPattern::None)
            }
            _ => return Err(Error::unexpected(arena, value_meta, "an AL shape pattern")),
        })
    }
}

// - Paths

/// Decodes a source update path.
impl Decode for il::Path {
    fn decode(arena: &mut Arena, value_meta: Value) -> Result<Self, Error> {
        let (values_meta, span) = case(arena, value_meta)?;
        let path_kind_al = match values_meta.as_slice() {
            [] if get::matches!(arena, &value_meta, m::ROOT) => il::PathKind::Root,
            [value_path, value_idx] if get::matches!(arena, &value_meta, m::IDX) => {
                il::PathKind::Idx(
                    Box::new(il::Path::decode(arena, *value_path)?),
                    Box::new(il::Exp::decode(arena, *value_idx)?),
                )
            }
            [value_path, value_idx, value_len] if get::matches!(arena, &value_meta, m::SLICE) => {
                il::PathKind::Slice(
                    Box::new(il::Path::decode(arena, *value_path)?),
                    Box::new(il::Exp::decode(arena, *value_idx)?),
                    Box::new(il::Exp::decode(arena, *value_len)?),
                )
            }
            [value_path, value_atom] if get::matches!(arena, &value_meta, m::DOT) => {
                il::PathKind::Dot(
                    Box::new(il::Path::decode(arena, *value_path)?),
                    il::Atom::decode(arena, *value_atom)?,
                )
            }
            _ => return Err(Error::unexpected(arena, value_meta, "an AL update path")),
        };
        Ok(note_phrase!(node: path_kind_al, note: stub::exp_note(), span: span))
    }
}

// - Operators

/// Decodes a unary operator.
impl Decode for (il::UnOp, il::OpTyp) {
    fn decode(arena: &mut Arena, value_meta: Value) -> Result<Self, Error> {
        let (values_meta, _) = case(arena, value_meta)?;
        match values_meta.as_slice() {
            [] if get::matches!(arena, &value_meta, m::NOT) => {
                Ok((il::UnOp::Bool(boolean::UnOp::Not), il::OpTyp::Bool))
            }
            [] if get::matches!(arena, &value_meta, m::PLUS) => {
                Ok((il::UnOp::Num(num::UnOp::Plus), il::OpTyp::Nat))
            }
            [] if get::matches!(arena, &value_meta, m::MINUS) => {
                Ok((il::UnOp::Num(num::UnOp::Minus), il::OpTyp::Nat))
            }
            _ => Err(Error::unexpected(arena, value_meta, "a nullary unop constructor")),
        }
    }
}

/// Decodes a binary operator.
impl Decode for (il::BinOp, il::OpTyp) {
    fn decode(arena: &mut Arena, value_meta: Value) -> Result<Self, Error> {
        let (values_meta, _) = case(arena, value_meta)?;
        match values_meta.as_slice() {
            [] if get::matches!(arena, &value_meta, m::AND) => {
                Ok((il::BinOp::Bool(boolean::BinOp::And), il::OpTyp::Bool))
            }
            [] if get::matches!(arena, &value_meta, m::OR) => {
                Ok((il::BinOp::Bool(boolean::BinOp::Or), il::OpTyp::Bool))
            }
            [] if get::matches!(arena, &value_meta, m::IMPL) => {
                Ok((il::BinOp::Bool(boolean::BinOp::Impl), il::OpTyp::Bool))
            }
            [] if get::matches!(arena, &value_meta, m::EQUIV) => {
                Ok((il::BinOp::Bool(boolean::BinOp::Equiv), il::OpTyp::Bool))
            }
            [] if get::matches!(arena, &value_meta, m::ADD) => {
                Ok((il::BinOp::Num(num::BinOp::Add), il::OpTyp::Nat))
            }
            [] if get::matches!(arena, &value_meta, m::SUB) => {
                Ok((il::BinOp::Num(num::BinOp::Sub), il::OpTyp::Nat))
            }
            [] if get::matches!(arena, &value_meta, m::MUL) => {
                Ok((il::BinOp::Num(num::BinOp::Mul), il::OpTyp::Nat))
            }
            [] if get::matches!(arena, &value_meta, m::DIV) => {
                Ok((il::BinOp::Num(num::BinOp::Div), il::OpTyp::Nat))
            }
            [] if get::matches!(arena, &value_meta, m::MOD) => {
                Ok((il::BinOp::Num(num::BinOp::Mod), il::OpTyp::Nat))
            }
            [] if get::matches!(arena, &value_meta, m::POW) => {
                Ok((il::BinOp::Num(num::BinOp::Pow), il::OpTyp::Nat))
            }
            _ => Err(Error::unexpected(arena, value_meta, "a nullary binop constructor")),
        }
    }
}

/// Decodes a comparison operator.
impl Decode for (il::CmpOp, il::OpTyp) {
    fn decode(arena: &mut Arena, value_meta: Value) -> Result<Self, Error> {
        let (values_meta, _) = case(arena, value_meta)?;
        match values_meta.as_slice() {
            [] if get::matches!(arena, &value_meta, m::EQ) => {
                Ok((il::CmpOp::Bool(boolean::CmpOp::Eq), il::OpTyp::Bool))
            }
            [] if get::matches!(arena, &value_meta, m::NE) => {
                Ok((il::CmpOp::Bool(boolean::CmpOp::Ne), il::OpTyp::Bool))
            }
            [] if get::matches!(arena, &value_meta, m::LT) => {
                Ok((il::CmpOp::Num(num::CmpOp::Lt), il::OpTyp::Nat))
            }
            [] if get::matches!(arena, &value_meta, m::LE) => {
                Ok((il::CmpOp::Num(num::CmpOp::Le), il::OpTyp::Nat))
            }
            [] if get::matches!(arena, &value_meta, m::GT) => {
                Ok((il::CmpOp::Num(num::CmpOp::Gt), il::OpTyp::Nat))
            }
            [] if get::matches!(arena, &value_meta, m::GE) => {
                Ok((il::CmpOp::Num(num::CmpOp::Ge), il::OpTyp::Nat))
            }
            _ => Err(Error::unexpected(arena, value_meta, "a nullary cmpop constructor")),
        }
    }
}

// - Definitions

/// Decodes one definition without inventing omitted general hints.
impl Decode for al::Def {
    fn decode(arena: &mut Arena, value_meta: Value) -> Result<Self, Error> {
        let (values_meta, span) = case(arena, value_meta)?;
        let def_kind_al = match values_meta.as_slice() {
            [value_id] if get::matches!(arena, &value_meta, m::EXTTYP) => {
                al::DefKind::Typ(al::TypDef::Extern(al::ExternTyp {
                    id: Id::decode(arena, *value_id)?,
                    hints: stub::hints(),
                }))
            }
            [value_id, value_ids, value_typ] if get::matches!(arena, &value_meta, m::TYP_DEF) => {
                al::DefKind::Typ(al::TypDef::Defined(Box::new(al::DefinedTyp {
                    id: Id::decode(arena, *value_id)?,
                    tparams: Vec::<Id>::decode(arena, *value_ids)?,
                    def_typ: il::DefTyp::decode(arena, *value_typ)?,
                    hints: stub::hints(),
                })))
            }
            [value_id, value_input, value_output]
                if get::matches!(arena, &value_meta, m::EXTREL) =>
            {
                let (not_typ, input_hint) = stub::not_typ(
                    Vec::<il::Typ>::decode(arena, *value_input)?,
                    Vec::<il::Typ>::decode(arena, *value_output)?,
                )?;
                al::DefKind::Rel(al::RelDef::Extern(Box::new(al::ExternRel {
                    id: Id::decode(arena, *value_id)?,
                    not_typ,
                    input_hint,
                    hints: stub::hints(),
                })))
            }
            [value_id, value_input, value_output, value_groups, value_else]
                if get::matches!(arena, &value_meta, m::REL_DEF) =>
            {
                let (not_typ, input_hint) = stub::not_typ(
                    Vec::<il::Typ>::decode(arena, *value_input)?,
                    Vec::<il::Typ>::decode(arena, *value_output)?,
                )?;
                al::DefKind::Rel(al::RelDef::Defined(Box::new(al::DefinedRel {
                    id: Id::decode(arena, *value_id)?,
                    not_typ,
                    input_hint,
                    rule_groups: Vec::<al::RuleGroup>::decode(arena, *value_groups)?,
                    else_group: Option::<al::ElseGroup>::decode(arena, *value_else)?,
                    hints: stub::hints(),
                })))
            }
            [value_id, value_ids, value_params, value_typ]
                if get::matches!(arena, &value_meta, m::EXTFUNC) =>
            {
                al::DefKind::MetaFunc(al::MetaFuncDef::Extern(al::ExternFunc {
                    id: Id::decode(arena, *value_id)?,
                    tparams: Vec::<Id>::decode(arena, *value_ids)?,
                    params: Vec::<al::Param>::decode(arena, *value_params)?,
                    typ: il::Typ::decode(arena, *value_typ)?,
                    hints: stub::hints(),
                }))
            }
            [value_id, value_ids, value_params, value_typ]
                if get::matches!(arena, &value_meta, m::BUILTINFUNC) =>
            {
                al::DefKind::MetaFunc(al::MetaFuncDef::Builtin(al::BuiltinFunc {
                    id: Id::decode(arena, *value_id)?,
                    tparams: Vec::<Id>::decode(arena, *value_ids)?,
                    params: Vec::<al::Param>::decode(arena, *value_params)?,
                    typ: il::Typ::decode(arena, *value_typ)?,
                    hints: stub::hints(),
                }))
            }
            [value_id, value_params, value_typ, value_rows]
                if get::matches!(arena, &value_meta, m::TABLEFUNC) =>
            {
                al::DefKind::MetaFunc(al::MetaFuncDef::Table(al::TableFunc {
                    id: Id::decode(arena, *value_id)?,
                    params: Vec::<al::Param>::decode(arena, *value_params)?,
                    typ: il::Typ::decode(arena, *value_typ)?,
                    table_rows: Vec::<al::TableRow>::decode(arena, *value_rows)?,
                    hints: stub::hints(),
                }))
            }
            [value_id, value_ids, value_params, value_typ, value_clauses, value_else]
                if get::matches!(arena, &value_meta, m::FUNC_DEF) =>
            {
                al::DefKind::MetaFunc(al::MetaFuncDef::Defined(Box::new(al::DefinedFunc {
                    id: Id::decode(arena, *value_id)?,
                    tparams: Vec::<Id>::decode(arena, *value_ids)?,
                    params: Vec::<al::Param>::decode(arena, *value_params)?,
                    typ: il::Typ::decode(arena, *value_typ)?,
                    clauses: Vec::<al::Clause>::decode(arena, *value_clauses)?,
                    else_clause: Option::<al::Clause>::decode(arena, *value_else)?,
                    hints: stub::hints(),
                })))
            }
            _ => {
                return Err(Error::unexpected(
                    arena,
                    value_meta,
                    "an AL definition constructor with its declared arguments",
                ));
            }
        };
        Ok(phrase!(node: def_kind_al, span: span))
    }
}

// - Parameters

/// Decodes a value or higher-order function parameter.
impl Decode for al::Param {
    fn decode(arena: &mut Arena, value_meta: Value) -> Result<Self, Error> {
        let (values_meta, span) = case(arena, value_meta)?;
        let param_kind_al = match values_meta.as_slice() {
            [value_typ] if get::matches!(arena, &value_meta, m::EXP_PARAM) => {
                al::ParamKind::Exp(il::Typ::decode(arena, *value_typ)?)
            }
            [value_id, value_ids, value_params, value_typ]
                if get::matches!(arena, &value_meta, m::FUN_PARAM) =>
            {
                al::ParamKind::Def(
                    Id::decode(arena, *value_id)?,
                    Vec::<Id>::decode(arena, *value_ids)?,
                    Vec::<al::Param>::decode(arena, *value_params)?,
                    il::Typ::decode(arena, *value_typ)?,
                )
            }
            _ => return Err(Error::unexpected(arena, value_meta, "an AL parameter")),
        };
        Ok(phrase!(node: param_kind_al, span: span))
    }
}

// - Premises

/// Decodes an AL premise.
impl Decode for al::Prem {
    fn decode(arena: &mut Arena, value_meta: Value) -> Result<Self, Error> {
        let (values_meta, span) = case(arena, value_meta)?;
        let prem_kind_al = match values_meta.as_slice() {
            [value_id, value_input, value_output]
                if get::matches!(arena, &value_meta, m::REL_PREM) =>
            {
                let (not_exp, input_hint) = stub::not_exp(
                    Vec::<il::Exp>::decode(arena, *value_input)?,
                    Vec::<il::Exp>::decode(arena, *value_output)?,
                )?;
                al::PremKind::Rule(al::RulePrem {
                    id: Id::decode(arena, *value_id)?,
                    not_exp,
                    input_hint,
                })
            }
            [value_exp] if get::matches!(arena, &value_meta, m::IF) => {
                al::PremKind::If(al::IfPrem { exp: il::Exp::decode(arena, *value_exp)? })
            }
            [value_id, value_exps] if get::matches!(arena, &value_meta, m::IFHOLD) => {
                let (not_exp, _) =
                    stub::not_exp(Vec::<il::Exp>::decode(arena, *value_exps)?, vec![])?;
                al::PremKind::IfHold(al::IfHoldPrem { id: Id::decode(arena, *value_id)?, not_exp })
            }
            [value_id, value_exps] if get::matches!(arena, &value_meta, m::IFNOTHOLD) => {
                let (not_exp, _) =
                    stub::not_exp(Vec::<il::Exp>::decode(arena, *value_exps)?, vec![])?;
                al::PremKind::IfNotHold(al::IfNotHoldPrem {
                    id: Id::decode(arena, *value_id)?,
                    not_exp,
                })
            }
            [value_l, value_r] if get::matches!(arena, &value_meta, m::LET) => {
                al::PremKind::Let(al::LetPrem {
                    exp_l: il::Exp::decode(arena, *value_l)?,
                    exp_r: il::Exp::decode(arena, *value_r)?,
                })
            }
            [value_prem, value_iter] if get::matches!(arena, &value_meta, m::ITER_PREM) => {
                al::PremKind::Iter(al::IterPrem {
                    prem: Box::new(al::Prem::decode(arena, *value_prem)?),
                    prem_iter: al::PremIter::decode(arena, *value_iter)?,
                })
            }
            [value_exp] if get::matches!(arena, &value_meta, m::DEBUG) => {
                al::PremKind::Debug(al::DebugPrem { exp: il::Exp::decode(arena, *value_exp)? })
            }
            _ => return Err(Error::unexpected(arena, value_meta, "an AL premise")),
        };
        Ok(phrase!(node: prem_kind_al, span: span))
    }
}

/// Decodes bound and bound-by-iteration variable lists in order.
impl Decode for al::PremIter {
    fn decode(arena: &mut Arena, value_meta: Value) -> Result<Self, Error> {
        let (values_meta, _) = case(arena, value_meta)?;
        match values_meta.as_slice() {
            [value_iter, value_bound, value_bind]
                if get::matches!(arena, &value_meta, m::ITERPREM) =>
            {
                Ok(al::PremIter {
                    iter: Iter::decode(arena, *value_iter)?,
                    vars_bound: Vec::<il::Var>::decode(arena, *value_bound)?,
                    vars_bind: Vec::<il::Var>::decode(arena, *value_bind)?,
                })
            }
            _ => Err(Error::unexpected(arena, value_meta, "iter vari* vari*")),
        }
    }
}

// - Rules

/// Decodes a rule match without its printing signature.
impl Decode for al::RuleMatch {
    fn decode(arena: &mut Arena, value_meta: Value) -> Result<Self, Error> {
        let (values_meta, _) = case(arena, value_meta)?;
        match values_meta.as_slice() {
            [value_exps, value_prems] if get::matches!(arena, &value_meta, m::RULMATCH) => {
                let exps_input = Vec::<il::Exp>::decode(arena, *value_exps)?;
                Ok(al::RuleMatch {
                    exps_signature: stub::rule_match_signature(&exps_input),
                    exps_input,
                    prems: Vec::<al::Prem>::decode(arena, *value_prems)?,
                })
            }
            _ => Err(Error::unexpected(arena, value_meta, "exp* '-' prem*")),
        }
    }
}

/// Decodes a rule path and its outputs.
impl Decode for al::RulePath {
    fn decode(arena: &mut Arena, value_meta: Value) -> Result<Self, Error> {
        let (values_meta, _) = case(arena, value_meta)?;
        match values_meta.as_slice() {
            [value_id, value_exps, value_prems]
                if get::matches!(arena, &value_meta, m::RULPATH) =>
            {
                Ok(al::RulePath {
                    id: Id::decode(arena, *value_id)?,
                    exps_output: Vec::<il::Exp>::decode(arena, *value_exps)?,
                    prems: Vec::<al::Prem>::decode(arena, *value_prems)?,
                })
            }
            _ => Err(Error::unexpected(arena, value_meta, "id '=' exp* '-' prem*")),
        }
    }
}

/// Decodes a rule group with its shared match.
impl Decode for al::RuleGroup {
    fn decode(arena: &mut Arena, value_meta: Value) -> Result<Self, Error> {
        let (values_meta, span) = case(arena, value_meta)?;
        match values_meta.as_slice() {
            [value_id, value_match, value_paths]
                if get::matches!(arena, &value_meta, m::RULGROUP) =>
            {
                Ok(phrase!(node: al::RuleGroupKind {
                    id: Id::decode(arena, *value_id)?,
                    rule_match: al::RuleMatch::decode(arena, *value_match)?,
                    rule_paths: Vec::<al::RulePath>::decode(arena, *value_paths)?,
                }, span: span))
            }
            _ => Err(Error::unexpected(arena, value_meta, "id ':' rulmatch '=' rulpath*")),
        }
    }
}

/// Decodes an otherwise group with one rule path.
impl Decode for al::ElseGroup {
    fn decode(arena: &mut Arena, value_meta: Value) -> Result<Self, Error> {
        let (values_meta, span) = case(arena, value_meta)?;
        match values_meta.as_slice() {
            [value_id, value_match, value_path]
                if get::matches!(arena, &value_meta, m::ELSGROUP) =>
            {
                Ok(phrase!(node: al::ElseGroupKind {
                    id: Id::decode(arena, *value_id)?,
                    rule_match: al::RuleMatch::decode(arena, *value_match)?,
                    rule_path: al::RulePath::decode(arena, *value_path)?,
                }, span: span))
            }
            _ => Err(Error::unexpected(arena, value_meta, "id ':' rulmatch '=' rulpath")),
        }
    }
}

// - Clauses

/// Decodes a function clause.
impl Decode for al::Clause {
    fn decode(arena: &mut Arena, value_meta: Value) -> Result<Self, Error> {
        let (values_meta, span) = case(arena, value_meta)?;
        match values_meta.as_slice() {
            [value_args, value_exp, value_prems]
                if get::matches!(arena, &value_meta, m::CLAUSE) =>
            {
                Ok(phrase!(node: al::ClauseKind {
                    args: Vec::<il::Arg>::decode(arena, *value_args)?,
                    exp: il::Exp::decode(arena, *value_exp)?,
                    prems: Vec::<al::Prem>::decode(arena, *value_prems)?,
                }, span: span))
            }
            _ => Err(Error::unexpected(arena, value_meta, "arg* '=' exp '-' prem*")),
        }
    }
}

/// Decodes a table row without its printing signature.
impl Decode for al::TableRow {
    fn decode(arena: &mut Arena, value_meta: Value) -> Result<Self, Error> {
        let clause_al = al::Clause::decode(arena, value_meta)?;
        let al::ClauseKind { args: args_al, exp: exp_al, prems: prems_al } = clause_al.node;
        Ok(phrase!(node: al::TableRowKind {
            exps_signature: stub::table_row_signature(),
            args: args_al,
            exp: exp_al,
            prems: prems_al,
        }, span: clause_al.span))
    }
}
