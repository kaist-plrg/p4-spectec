//! Decodes syntax shared by IL and AL from meta data
//!
//! `Decode` implementations check constructor handles in the supplied arena.
//! Syntax is reconstructed canonically for editing and re-encoding;
//! type notes are restored from the encoded annotations.

use super::{Cache, Error, mixops as m, stub};
use crate::lang::common::prim::num::Number;
use crate::{
    frontend::parse::parse_typ,
    lang::{
        common::{
            Id, Iter,
            notation::atom::Atom,
            prim::{bool as boolean, num},
        },
        data::{
            arena::Arena,
            notation::{
                flat::{
                    Mixop as FlatMixop,
                    parse::{MatrixMixopError, ToMixop},
                },
                tree::{Mixfix as TreeMixfix, Mixop},
            },
            value::flat::{Value, get},
        },
        el::ast as el,
        il::ast as il,
        traits::print::Print,
    },
    note_phrase, phrase,
};
use num_traits::ToPrimitive;
use std::rc::Rc;

/// Reconstructs source IL or AL data from a meta value.
pub trait Decode: Sized {
    /// Decodes a value without retaining conversion results.
    fn decode(arena: &mut Arena, value_meta: Value) -> Result<Self, Error> {
        Self::decode_cached(arena, &mut Cache::new(false), value_meta)
    }

    /// Decodes a value with a cache belonging to the supplied arena.
    fn decode_cached(
        arena: &mut Arena,
        cache: &mut Cache,
        value_meta: Value,
    ) -> Result<Self, Error>;
}

// == Helpers

/// Decodes the type annotation of a typed expression or path.
fn typed(
    arena: &mut Arena,
    cache: &mut Cache,
    value_kind: &Value,
    value_typ: &Value,
) -> Result<(Value, il::Typ), Error> {
    Ok((*value_kind, il::Typ::decode_cached(arena, cache, *value_typ)?))
}

/// Recovers a numeric operator type from its operand or result type.
fn op_typ(typ_kind: &il::TypKind, op_typ_decoded: il::OpTyp) -> il::OpTyp {
    match (op_typ_decoded, typ_kind) {
        // Boolean operators carry their own type
        (il::OpTyp::Bool, _) => il::OpTyp::Bool,
        // Integer operands select integer arithmetic
        (_, il::TypKind::Num(num::Typ::Int)) => il::OpTyp::Int,
        // Every other numeric operator works on naturals
        (_, _) => il::OpTyp::Nat,
    }
}

// == Decoding

// - Containers

/// Decodes a checked meta list element by element.
impl<T: Decode> Decode for Vec<T> {
    fn decode_cached(
        arena: &mut Arena,
        cache: &mut Cache,
        value_meta: Value,
    ) -> Result<Self, Error> {
        get::list(arena, &value_meta)?
            .to_vec()
            .into_iter()
            .map(|value_elem| T::decode_cached(arena, cache, value_elem))
            .collect()
    }
}

/// Decodes a checked optional meta value.
impl<T: Decode> Decode for Option<T> {
    fn decode_cached(
        arena: &mut Arena,
        cache: &mut Cache,
        value_meta: Value,
    ) -> Result<Self, Error> {
        get::opt(arena, &value_meta)?
            .map(|value_inner| T::decode_cached(arena, cache, value_inner))
            .transpose()
    }
}

// - Identifiers

/// Decodes an owned identifier from a meta text value.
impl Decode for Id {
    fn decode_cached(
        arena: &mut Arena,
        _cache: &mut Cache,
        value_meta: Value,
    ) -> Result<Self, Error> {
        Ok(
            phrase!(node: get::text(arena, &value_meta)?.into(), span: *arena.value.span(value_meta.span)),
        )
    }
}

// - Atoms

/// Decodes an atom, rejecting strings containing several notation tokens.
impl Decode for il::Atom {
    fn decode_cached(
        arena: &mut Arena,
        _cache: &mut Cache,
        value_meta: Value,
    ) -> Result<Self, Error> {
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
            _ => {
                // Read the atom directly from the parser's notation type
                let typ_el = parse_typ(text_meta).map_err(|report| Error::from(report).at(span))?;
                match typ_el {
                    el::Typ::Notation(not_typ_el) => match not_typ_el.node {
                        el::NotTypKind::Atom(atom) => atom.node,
                        _ => return Err(Error::unexpected(arena, value_meta, "one notation atom")),
                    },
                    _ => return Err(Error::unexpected(arena, value_meta, "one notation atom")),
                }
            }
        };
        Ok(phrase!(node: atom_kind, span: span))
    }
}

// - Mixops

/// Reconstructs notation from atoms between argument positions.
impl Decode for Rc<Mixop> {
    fn decode_cached(
        arena: &mut Arena,
        cache: &mut Cache,
        value_meta: Value,
    ) -> Result<Self, Error> {
        let atoms_matrix_al = Vec::<Vec<il::Atom>>::decode_cached(arena, cache, value_meta)?;
        let span = *arena.value.span(value_meta.span);
        // A matrix with n holes has n + 1 rows, including empty boundary rows
        if atoms_matrix_al.is_empty() {
            return Err(Error::unexpected(arena, value_meta, "a nonempty atom matrix"));
        }
        // Preserve empty notation as the single empty boundary row
        if atoms_matrix_al.len() == 1 && atoms_matrix_al[0].is_empty() {
            return Ok(Rc::new(Mixop::Seq(vec![])));
        }
        // Insert one parser argument between each pair of boundary rows
        let text_mixop = atoms_matrix_al
            .iter()
            .map(|atoms_al| {
                atoms_al
                    .iter()
                    .map(Print::to_string)
                    .collect::<Vec<_>>()
                    .join(" ")
            })
            .collect::<Vec<_>>()
            .join(" nat ");
        let mixop_al = text_mixop
            .to_mixop(&mut arena.mixop)
            .map_err(|error| Error::from(error).at(span))?
            .into_tree(&arena.mixop);
        // Atom text must not inject additional argument positions
        if mixop_al.arity() + 1 != atoms_matrix_al.len() {
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
    fn decode_cached(
        arena: &mut Arena,
        cache: &mut Cache,
        value_meta: Value,
    ) -> Result<Self, Error> {
        // Skip matrix decoding and notation reconstruction on a hit
        let id_value = arena.value.canon_id(value_meta.node);
        if let Some(mixop) = cache.mixops_decode.get(&id_value) {
            return Ok(*mixop);
        }
        let atoms_matrix = Vec::<Vec<il::Atom>>::decode_cached(arena, cache, value_meta)?;
        let span = *arena.value.span(value_meta.span);
        // Retain the encoded value's location and shape in conversion diagnostics
        let mixop =
            atoms_matrix
                .as_slice()
                .to_mixop(&mut arena.mixop)
                .map_err(|error| match error {
                    MatrixMixopError::EmptyMatrix => {
                        Error::unexpected(arena, value_meta, "a nonempty atom matrix")
                    }
                    MatrixMixopError::ArityMismatch => Error::unexpected(
                        arena,
                        value_meta,
                        "an atom matrix with one row per argument boundary",
                    ),
                    MatrixMixopError::Parse(error) => Error::from(error).at(span),
                })?;
        cache.mixops_decode.insert(id_value, mixop);
        Ok(mixop)
    }
}

// - Mixfixes

/// Decodes a type notation case.
impl Decode for TreeMixfix<il::Typ> {
    fn decode_cached(
        arena: &mut Arena,
        cache: &mut Cache,
        value_meta: Value,
    ) -> Result<Self, Error> {
        get::matches_string! { arena, &value_meta,
            m::TYPCASE => |[value_mixop, values_arg]| {
                Ok(TreeMixfix::new(
                    Rc::<Mixop>::decode_cached(arena, cache, *value_mixop)?,
                    Vec::<il::Typ>::decode_cached(arena, cache, *values_arg)?,
                )?)
            },
            _ => Err(Error::unexpected(arena, value_meta, "mixop elem*")),
        }
    }
}

/// Decodes an expression notation case.
impl Decode for TreeMixfix<il::Exp> {
    fn decode_cached(
        arena: &mut Arena,
        cache: &mut Cache,
        value_meta: Value,
    ) -> Result<Self, Error> {
        get::matches_string! { arena, &value_meta,
            m::EXPCASE => |[value_mixop, values_arg]| {
                Ok(TreeMixfix::new(
                    Rc::<Mixop>::decode_cached(arena, cache, *value_mixop)?,
                    Vec::<il::Exp>::decode_cached(arena, cache, *values_arg)?,
                )?)
            },
            _ => Err(Error::unexpected(arena, value_meta, "mixop elem*")),
        }
    }
}

// - Iterators

/// Decodes an iterator with its nullary constructor checked.
impl Decode for Iter {
    fn decode_cached(
        arena: &mut Arena,
        _cache: &mut Cache,
        value_meta: Value,
    ) -> Result<Self, Error> {
        get::matches_string! { arena, &value_meta,
            m::QUEST => |[]| Ok(Iter::Opt),
            m::STAR => |[]| Ok(Iter::List),
            _ => Err(Error::unexpected(arena, value_meta, "QUEST or STAR")),
        }
    }
}

// - Variables

/// Decodes a variable's underlying type and dimensions.
impl Decode for il::Var {
    fn decode_cached(
        arena: &mut Arena,
        cache: &mut Cache,
        value_meta: Value,
    ) -> Result<Self, Error> {
        get::matches_string! { arena, &value_meta,
            m::VARI => |[value_id, value_typ, value_iters]| {
                Ok(il::Var {
                    id: Id::decode_cached(arena, cache, *value_id)?,
                    typ: il::Typ::decode_cached(arena, cache, *value_typ)?,
                    iters: Vec::<Iter>::decode_cached(arena, cache, *value_iters)?,
                })
            },
            _ => Err(Error::unexpected(arena, value_meta, "id typ iter*")),
        }
    }
}

// - Types

/// Decodes a source type, checking every constructor's shape.
impl Decode for il::Typ {
    fn decode_cached(
        arena: &mut Arena,
        cache: &mut Cache,
        value_meta: Value,
    ) -> Result<Self, Error> {
        // Type representations share the value arena's canonical identity
        let id_value = arena.value.canon_id(value_meta.node);
        if let Some(typ_al) = cache.typs_decode.get(&id_value) {
            return Ok(typ_al.clone());
        }
        let span = *arena.value.span(value_meta.span);
        let typ_kind_al = get::matches_string! { arena, &value_meta,
            m::BOOL_TYP => |[]| il::TypKind::Bool,
            m::NAT_TYP => |[]| il::TypKind::Num(num::Typ::Nat),
            m::INT_TYP => |[]| il::TypKind::Num(num::Typ::Int),
            m::TEXT_TYP => |[]| il::TypKind::Text,
            m::VAR_TYP => |[value_id, value_typs]| {
                il::TypKind::Var(
                    Id::decode_cached(arena, cache, *value_id)?,
                    Vec::<il::Typ>::decode_cached(arena, cache, *value_typs)?,
                )
            },
            m::TUP_TYP => |[value_typs]| {
                il::TypKind::Tuple(Vec::<il::Typ>::decode_cached(arena, cache, *value_typs)?)
            },
            m::ITER_TYP => |[value_typ, value_iter]| {
                il::TypKind::Iter(
                    Box::new(il::Typ::decode_cached(arena, cache, *value_typ)?),
                    Iter::decode_cached(arena, cache, *value_iter)?,
                )
            },
            m::FUNC_TYP => |[value_ids, value_typs, value_ret]| {
                il::TypKind::Func(il::FuncTyp {
                    tparams: Vec::<Id>::decode_cached(arena, cache, *value_ids)?,
                    typs_params: Vec::<il::Typ>::decode_cached(arena, cache, *value_typs)?,
                    typ_ret: Box::new(il::Typ::decode_cached(arena, cache, *value_ret)?),
                })
            },
            _ => {
                            return Err(Error::unexpected(
                                arena,
                                value_meta,
                                "an AL type constructor with its declared arguments",
                            ));
                        },
        };
        let typ_al = phrase!(node: typ_kind_al, span: span);
        cache.typs_decode.insert(id_value, typ_al.clone());
        Ok(typ_al)
    }
}

// - Defined types

/// Decodes an alias, struct, or variant without general hints.
impl Decode for il::DefTyp {
    fn decode_cached(
        arena: &mut Arena,
        cache: &mut Cache,
        value_meta: Value,
    ) -> Result<Self, Error> {
        let span = *arena.value.span(value_meta.span);
        let deftyp_kind_al = get::matches_string! { arena, &value_meta,
            m::ALIAS => |[value_typ]| {
                il::DefTypKind::Plain(il::Typ::decode_cached(arena, cache, *value_typ)?)
            },
            m::STRUCT => |[value_fields]| {
                il::DefTypKind::Struct(Vec::<il::TypField>::decode_cached(arena, cache, *value_fields)?)
            },
            m::VARIANT => |[value_cases]| {
                il::DefTypKind::Variant(Vec::<il::TypCase>::decode_cached(arena, cache, *value_cases)?)
            },
            _ => return Err(Error::unexpected(arena, value_meta, "ALIAS, STRUCT, or VARIANT")),
        };
        Ok(phrase!(node: deftyp_kind_al, span: span))
    }
}

/// Decodes one struct type field.
impl Decode for il::TypField {
    fn decode_cached(
        arena: &mut Arena,
        cache: &mut Cache,
        value_meta: Value,
    ) -> Result<Self, Error> {
        get::matches_string! { arena, &value_meta,
            m::TYPFIELD => |[value_atom, value_typ]| {
                Ok(il::TypField {
                    atom: il::Atom::decode_cached(arena, cache, *value_atom)?,
                    typ: il::Typ::decode_cached(arena, cache, *value_typ)?,
                })
            },
            _ => Err(Error::unexpected(arena, value_meta, "atom typ")),
        }
    }
}

/// Decodes one variant case without its origin or hints.
impl Decode for il::TypCase {
    fn decode_cached(
        arena: &mut Arena,
        cache: &mut Cache,
        value_meta: Value,
    ) -> Result<Self, Error> {
        let span = *arena.value.span(value_meta.span);
        let mixfix_typ_al = TreeMixfix::<il::Typ>::decode_cached(arena, cache, value_meta)?;
        Ok(il::TypCase {
            not_typ: phrase!(node: mixfix_typ_al, span: span),
            typ_origin: stub::typ_origin(),
            hints: stub::hints(),
        })
    }
}

// - Operators

/// Decodes a unary operator.
impl Decode for (il::UnOp, il::OpTyp) {
    fn decode_cached(
        arena: &mut Arena,
        _cache: &mut Cache,
        value_meta: Value,
    ) -> Result<Self, Error> {
        get::matches_string! { arena, &value_meta,
            m::NOT => |[]| {
                Ok((il::UnOp::Bool(boolean::UnOp::Not), il::OpTyp::Bool))
            },
            m::PLUS => |[]| {
                Ok((il::UnOp::Num(num::UnOp::Plus), il::OpTyp::Nat))
            },
            m::MINUS => |[]| {
                Ok((il::UnOp::Num(num::UnOp::Minus), il::OpTyp::Nat))
            },
            _ => Err(Error::unexpected(arena, value_meta, "a nullary unop constructor")),
        }
    }
}

/// Decodes a binary operator.
impl Decode for (il::BinOp, il::OpTyp) {
    fn decode_cached(
        arena: &mut Arena,
        _cache: &mut Cache,
        value_meta: Value,
    ) -> Result<Self, Error> {
        get::matches_string! { arena, &value_meta,
            m::AND => |[]| {
                Ok((il::BinOp::Bool(boolean::BinOp::And), il::OpTyp::Bool))
            },
            m::OR => |[]| {
                Ok((il::BinOp::Bool(boolean::BinOp::Or), il::OpTyp::Bool))
            },
            m::IMPL => |[]| {
                Ok((il::BinOp::Bool(boolean::BinOp::Impl), il::OpTyp::Bool))
            },
            m::EQUIV => |[]| {
                Ok((il::BinOp::Bool(boolean::BinOp::Equiv), il::OpTyp::Bool))
            },
            m::ADD => |[]| {
                Ok((il::BinOp::Num(num::BinOp::Add), il::OpTyp::Nat))
            },
            m::SUB => |[]| {
                Ok((il::BinOp::Num(num::BinOp::Sub), il::OpTyp::Nat))
            },
            m::MUL => |[]| {
                Ok((il::BinOp::Num(num::BinOp::Mul), il::OpTyp::Nat))
            },
            m::DIV => |[]| {
                Ok((il::BinOp::Num(num::BinOp::Div), il::OpTyp::Nat))
            },
            m::MOD => |[]| {
                Ok((il::BinOp::Num(num::BinOp::Mod), il::OpTyp::Nat))
            },
            m::POW => |[]| {
                Ok((il::BinOp::Num(num::BinOp::Pow), il::OpTyp::Nat))
            },
            _ => Err(Error::unexpected(arena, value_meta, "a nullary binop constructor")),
        }
    }
}

/// Decodes a comparison operator.
impl Decode for (il::CmpOp, il::OpTyp) {
    fn decode_cached(
        arena: &mut Arena,
        _cache: &mut Cache,
        value_meta: Value,
    ) -> Result<Self, Error> {
        get::matches_string! { arena, &value_meta,
            m::EQ => |[]| {
                Ok((il::CmpOp::Bool(boolean::CmpOp::Eq), il::OpTyp::Bool))
            },
            m::NE => |[]| {
                Ok((il::CmpOp::Bool(boolean::CmpOp::Ne), il::OpTyp::Bool))
            },
            m::LT => |[]| {
                Ok((il::CmpOp::Num(num::CmpOp::Lt), il::OpTyp::Nat))
            },
            m::LE => |[]| {
                Ok((il::CmpOp::Num(num::CmpOp::Le), il::OpTyp::Nat))
            },
            m::GT => |[]| {
                Ok((il::CmpOp::Num(num::CmpOp::Gt), il::OpTyp::Nat))
            },
            m::GE => |[]| {
                Ok((il::CmpOp::Num(num::CmpOp::Ge), il::OpTyp::Nat))
            },
            _ => Err(Error::unexpected(arena, value_meta, "a nullary cmpop constructor")),
        }
    }
}

// - Expressions

/// Reconstructs an expression suitable for editing and re-encoding.
impl Decode for il::Exp {
    fn decode_cached(
        arena: &mut Arena,
        cache: &mut Cache,
        value_meta: Value,
    ) -> Result<Self, Error> {
        let span = *arena.value.span(value_meta.span);
        let (value_kind, typ_al) = get::matches_string! { arena, &value_meta,
            m::EXP => |[value_kind, value_typ]| typed(arena, cache, value_kind, value_typ)?,
            _ => return Err(Error::unexpected(arena, value_meta, "exp '#' typ")),
        };
        let exp_kind_al = get::matches_string! { arena, &value_kind,
            m::BOOL => |[value_bool]| {
                il::ExpKind::Bool(get::bool(arena, value_bool)?)
            },
            m::NAT => |[value_num]| {
                let Number::Nat(nat) = get::num(arena, value_num)? else {
                    return Err(Error::unexpected(arena, *value_num, "a natural-number payload"));
                };
                il::ExpKind::Num(Number::Nat(nat.clone()))
            },
            m::INT => |[value_num]| {
                il::ExpKind::Num(Number::Int(num::to_int(get::num(arena, value_num)?).clone()))
            },
            m::TEXT => |[value_text]| {
                il::ExpKind::Text(get::text(arena, value_text)?.into())
            },
            m::VAR_EXP => |[value_id]| {
                il::ExpKind::Id(Id::decode_cached(arena, cache, *value_id)?)
            },
            m::UN => |[value_op, value_exp]| {
                let (op_al, typ_op_al) = <(il::UnOp, il::OpTyp)>::decode_cached(arena, cache, *value_op)?;
                il::ExpKind::Un(op_al, op_typ(&typ_al.node, typ_op_al), Box::new(il::Exp::decode_cached(arena, cache, *value_exp)?))
            },
            m::BIN => |[value_op, value_l, value_r]| {
                let (op_al, typ_op_al) = <(il::BinOp, il::OpTyp)>::decode_cached(arena, cache, *value_op)?;
                il::ExpKind::Bin(
                    op_al,
                    op_typ(&typ_al.node, typ_op_al),
                    Box::new(il::Exp::decode_cached(arena, cache, *value_l)?),
                    Box::new(il::Exp::decode_cached(arena, cache, *value_r)?),
                )
            },
            m::CMP => |[value_op, value_l, value_r]| {
                let (op_al, typ_op_al) = <(il::CmpOp, il::OpTyp)>::decode_cached(arena, cache, *value_op)?;
                let exp_l_al = il::Exp::decode_cached(arena, cache, *value_l)?;
                il::ExpKind::Cmp(
                    op_al,
                    op_typ(&exp_l_al.note, typ_op_al),
                    Box::new(exp_l_al),
                    Box::new(il::Exp::decode_cached(arena, cache, *value_r)?),
                )
            },
            m::UPCAST => |[value_typ, value_exp]| {
                il::ExpKind::UpCast(
                    Box::new(il::Typ::decode_cached(arena, cache, *value_typ)?),
                    Box::new(il::Exp::decode_cached(arena, cache, *value_exp)?),
                )
            },
            m::DOWNCAST => |[value_typ, value_exp]| {
                il::ExpKind::DownCast(
                    Box::new(il::Typ::decode_cached(arena, cache, *value_typ)?),
                    Box::new(il::Exp::decode_cached(arena, cache, *value_exp)?),
                )
            },
            m::SUB_EXP => |[value_exp, value_typ]| {
                let typ_al = il::Typ::decode_cached(arena, cache, *value_typ)?;
                il::ExpKind::Sub(
                    Box::new(il::Exp::decode_cached(arena, cache, *value_exp)?),
                    Box::new(typ_al),
                )
            },
            m::MATCH => |[value_exp, value_pattern]| {
                il::ExpKind::Match(
                    Box::new(il::Exp::decode_cached(arena, cache, *value_exp)?),
                    il::Pattern::decode_cached(arena, cache, *value_pattern)?,
                )
            },
            m::TUP => |[value_exps]| {
                il::ExpKind::Tuple(Vec::<il::Exp>::decode_cached(arena, cache, *value_exps)?)
            },
            m::INJ => |[value_case]| {
                il::ExpKind::Case(Box::new(TreeMixfix::<il::Exp>::decode_cached(arena, cache, *value_case)?))
            },
            m::STR => |[value_fields]| {
                il::ExpKind::Str(Vec::<il::ExpField>::decode_cached(arena, cache, *value_fields)?)
            },
            m::OPT => |[value_opt]| {
                il::ExpKind::Opt(Option::<il::Exp>::decode_cached(arena, cache, *value_opt)?.map(Box::new))
            },
            m::LIST => |[value_exps]| {
                il::ExpKind::List(Vec::<il::Exp>::decode_cached(arena, cache, *value_exps)?)
            },
            m::CONS => |[value_head, value_tail]| {
                il::ExpKind::Cons(
                    Box::new(il::Exp::decode_cached(arena, cache, *value_head)?),
                    Box::new(il::Exp::decode_cached(arena, cache, *value_tail)?),
                )
            },
            m::CAT => |[value_l, value_r]| il::ExpKind::Cat(
                            Box::new(il::Exp::decode_cached(arena, cache, *value_l)?),
                            Box::new(il::Exp::decode_cached(arena, cache, *value_r)?),
                        ),
            m::MEM => |[value_elem, value_list]| {
                il::ExpKind::Mem(
                    Box::new(il::Exp::decode_cached(arena, cache, *value_elem)?),
                    Box::new(il::Exp::decode_cached(arena, cache, *value_list)?),
                )
            },
            m::LEN => |[value_exp]| {
                il::ExpKind::Len(Box::new(il::Exp::decode_cached(arena, cache, *value_exp)?))
            },
            m::DOT => |[value_base, value_atom]| {
                il::ExpKind::Dot(
                    Box::new(il::Exp::decode_cached(arena, cache, *value_base)?),
                    il::Atom::decode_cached(arena, cache, *value_atom)?,
                )
            },
            m::IDX => |[value_base, value_idx]| {
                il::ExpKind::Idx(
                    Box::new(il::Exp::decode_cached(arena, cache, *value_base)?),
                    Box::new(il::Exp::decode_cached(arena, cache, *value_idx)?),
                )
            },
            m::SLICE => |[value_base, value_idx, value_len]| {
                il::ExpKind::Slice(
                    Box::new(il::Exp::decode_cached(arena, cache, *value_base)?),
                    Box::new(il::Exp::decode_cached(arena, cache, *value_idx)?),
                    Box::new(il::Exp::decode_cached(arena, cache, *value_len)?),
                )
            },
            m::UPD => |[value_base, value_path, value_upd]| {
                il::ExpKind::Upd(
                    Box::new(il::Exp::decode_cached(arena, cache, *value_base)?),
                    Box::new(il::Path::decode_cached(arena, cache, *value_path)?),
                    Box::new(il::Exp::decode_cached(arena, cache, *value_upd)?),
                )
            },
            m::CALL => |[value_id, value_typs, value_args]| {
                il::ExpKind::Call(
                    Id::decode_cached(arena, cache, *value_id)?,
                    Vec::<il::Typ>::decode_cached(arena, cache, *value_typs)?,
                    Vec::<il::Arg>::decode_cached(arena, cache, *value_args)?,
                )
            },
            m::ITER_EXP => |[value_exp, value_iter]| {
                il::ExpKind::Iter(
                    Box::new(il::Exp::decode_cached(arena, cache, *value_exp)?),
                    il::ExpIter::decode_cached(arena, cache, *value_iter)?,
                )
            },
            _ => {
                            return Err(Error::unexpected(
                                arena,
                                value_meta,
                                "an AL expression constructor with its declared arguments",
                            ));
                        },
        };
        Ok(note_phrase!(node: exp_kind_al, note: Rc::new(typ_al.node), span: span))
    }
}

/// Decodes one source struct-expression field.
impl Decode for il::ExpField {
    fn decode_cached(
        arena: &mut Arena,
        cache: &mut Cache,
        value_meta: Value,
    ) -> Result<Self, Error> {
        get::matches_string! { arena, &value_meta,
            m::EXPFIELD => |[value_atom, value_exp]| {
                Ok(il::ExpField {
                    atom: il::Atom::decode_cached(arena, cache, *value_atom)?,
                    exp: il::Exp::decode_cached(arena, cache, *value_exp)?,
                })
            },
            _ => Err(Error::unexpected(arena, value_meta, "atom exp")),
        }
    }
}

/// Decodes an expression iterator and its variables.
impl Decode for il::ExpIter {
    fn decode_cached(
        arena: &mut Arena,
        cache: &mut Cache,
        value_meta: Value,
    ) -> Result<Self, Error> {
        get::matches_string! { arena, &value_meta,
            m::ITEREXP => |[value_iter, value_vars]| {
                Ok(il::ExpIter {
                    iter: Iter::decode_cached(arena, cache, *value_iter)?,
                    vars: Vec::<il::Var>::decode_cached(arena, cache, *value_vars)?,
                })
            },
            _ => Err(Error::unexpected(arena, value_meta, "iter vari*")),
        }
    }
}

// - Patterns

/// Decodes a case, list, or option shape pattern.
impl Decode for il::Pattern {
    fn decode_cached(
        arena: &mut Arena,
        cache: &mut Cache,
        value_meta: Value,
    ) -> Result<Self, Error> {
        Ok(get::matches_string! { arena, &value_meta,
            m::CASE_PATTERN => |[value_mixop]| {
                il::Pattern::Case(Rc::<Mixop>::decode_cached(arena, cache, *value_mixop)?)
            },
            m::CONS_PATTERN => |[]| {
                il::Pattern::List(il::ListPattern::Cons)
            },
            m::FIXED => |[value_nat]| {
                let Number::Nat(nat) = get::num(arena, value_nat)? else {
                    return Err(Error::unexpected(arena, *value_nat, "a natural list length"));
                };
                let num_len = nat.as_bigint().to_usize().ok_or_else(|| {
                    Error::unexpected(arena, *value_nat, "a list length fitting usize")
                })?;
                il::Pattern::List(il::ListPattern::Fixed(num_len))
            },
            m::NIL => |[]| {
                il::Pattern::List(il::ListPattern::Nil)
            },
            m::SOME => |[]| {
                il::Pattern::Opt(il::OptPattern::Some)
            },
            m::NONE => |[]| {
                il::Pattern::Opt(il::OptPattern::None)
            },
            _ => return Err(Error::unexpected(arena, value_meta, "an AL shape pattern")),
        })
    }
}

// - Paths

/// Decodes a source update path.
impl Decode for il::Path {
    fn decode_cached(
        arena: &mut Arena,
        cache: &mut Cache,
        value_meta: Value,
    ) -> Result<Self, Error> {
        let span = *arena.value.span(value_meta.span);
        let (value_kind, typ_al) = get::matches_string! { arena, &value_meta,
            m::PATH => |[value_kind, value_typ]| typed(arena, cache, value_kind, value_typ)?,
            _ => return Err(Error::unexpected(arena, value_meta, "path '#' typ")),
        };
        let path_kind_al = get::matches_string! { arena, &value_kind,
            m::ROOT => |[]| il::PathKind::Root,
            m::IDX => |[value_path, value_idx]| {
                il::PathKind::Idx(
                    Box::new(il::Path::decode_cached(arena, cache, *value_path)?),
                    Box::new(il::Exp::decode_cached(arena, cache, *value_idx)?),
                )
            },
            m::SLICE => |[value_path, value_idx, value_len]| {
                il::PathKind::Slice(
                    Box::new(il::Path::decode_cached(arena, cache, *value_path)?),
                    Box::new(il::Exp::decode_cached(arena, cache, *value_idx)?),
                    Box::new(il::Exp::decode_cached(arena, cache, *value_len)?),
                )
            },
            m::DOT => |[value_path, value_atom]| {
                il::PathKind::Dot(
                    Box::new(il::Path::decode_cached(arena, cache, *value_path)?),
                    il::Atom::decode_cached(arena, cache, *value_atom)?,
                )
            },
            _ => return Err(Error::unexpected(arena, value_meta, "an AL update path")),
        };
        Ok(note_phrase!(node: path_kind_al, note: Rc::new(typ_al.node), span: span))
    }
}

// - Parameters

/// Decodes a value or higher-order function parameter.
impl Decode for il::Param {
    fn decode_cached(
        arena: &mut Arena,
        cache: &mut Cache,
        value_meta: Value,
    ) -> Result<Self, Error> {
        let span = *arena.value.span(value_meta.span);
        let param_kind_al = get::matches_string! { arena, &value_meta,
            m::EXP_PARAM => |[value_typ]| {
                il::ParamKind::Exp(il::Typ::decode_cached(arena, cache, *value_typ)?)
            },
            m::FUN_PARAM => |[value_id, value_ids, value_params, value_typ]| {
                il::ParamKind::Def(
                    Id::decode_cached(arena, cache, *value_id)?,
                    Vec::<Id>::decode_cached(arena, cache, *value_ids)?,
                    Vec::<il::Param>::decode_cached(arena, cache, *value_params)?,
                    il::Typ::decode_cached(arena, cache, *value_typ)?,
                )
            },
            _ => return Err(Error::unexpected(arena, value_meta, "an AL parameter")),
        };
        Ok(phrase!(node: param_kind_al, span: span))
    }
}

// - Arguments

/// Decodes a value or function argument.
impl Decode for il::Arg {
    fn decode_cached(
        arena: &mut Arena,
        cache: &mut Cache,
        value_meta: Value,
    ) -> Result<Self, Error> {
        let span = *arena.value.span(value_meta.span);
        let arg_kind_al = get::matches_string! { arena, &value_meta,
            m::EXP_ARG => |[value_exp]| {
                il::ArgKind::Exp(Box::new(il::Exp::decode_cached(arena, cache, *value_exp)?))
            },
            m::FUN_ARG => |[value_id]| {
                il::ArgKind::Def(Id::decode_cached(arena, cache, *value_id)?)
            },
            _ => return Err(Error::unexpected(arena, value_meta, "EXP exp or FUN id")),
        };
        Ok(phrase!(node: arg_kind_al, span: span))
    }
}
