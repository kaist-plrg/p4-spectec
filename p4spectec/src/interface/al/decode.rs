//! Decodes AL meta data into source syntax and runtime values
//!
//! `Decode` implementations check constructor handles in the supplied arena
//! Syntax is reconstructed canonically for editing and re-encoding
//! Runtime payload decoding allocates only in that same arena

use super::{Error, mixops as m};
use crate::lang::{
    common::prim::num::Number,
    hints::input::{self, InputHint},
};
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
                tree::{Mixfix, Mixop},
            },
            typ::make as make_typ,
            value::flat::{Value, ValueField, get, make},
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

// == Common projections

/// Decodes an owned identifier from a meta text value.
impl Decode for Id {
    fn decode(arena: &mut Arena, value_meta: Value) -> Result<Self, Error> {
        Ok(
            phrase!(node: get::text(arena, &value_meta)?.into(), span: *arena.value.span(value_meta.span)),
        )
    }
}

/// Maps a checked meta list without allocating runtime values.
fn list<T>(
    arena: &mut Arena,
    value_meta: Value,
    mut func_decode: impl FnMut(&mut Arena, Value) -> Result<T, Error>,
) -> Result<Vec<T>, Error> {
    get::list(arena, &value_meta)?
        .to_vec()
        .into_iter()
        .map(|value_elem| func_decode(arena, value_elem))
        .collect()
}

/// Copies a case's children before recursive decoding or allocation.
fn case(arena: &mut Arena, value_meta: Value) -> Result<(FlatMixop, Vec<Value>, Span), Error> {
    let value_case = get::case(arena, &value_meta)?;
    Ok((*value_case.mixop(), value_case.args().to_vec(), *arena.value.span(value_meta.span)))
}

/// Decodes an atom, rejecting strings containing several notation tokens.
fn atom(arena: &mut Arena, value_meta: Value) -> Result<il::Atom, Error> {
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
fn mixop(arena: &mut Arena, value_meta: Value) -> Result<Rc<Mixop>, Error> {
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
fn iter(arena: &mut Arena, value_meta: Value) -> Result<Iter, Error> {
    let (mixop_meta, values_meta, _) = case(arena, value_meta)?;
    match values_meta.as_slice() {
        [] if m::is(arena, mixop_meta, m::QUEST)? => Ok(Iter::Opt),
        [] if m::is(arena, mixop_meta, m::STAR)? => Ok(Iter::List),
        _ => Err(Error::unexpected(arena, value_meta, "QUEST or STAR")),
    }
}

/// Decodes a variable's underlying type and dimensions.
impl Decode for il::Var {
    fn decode(arena: &mut Arena, value_meta: Value) -> Result<Self, Error> {
        let (mixop_meta, values_meta, _) = case(arena, value_meta)?;
        match values_meta.as_slice() {
            [value_id, value_typ, value_iters] if m::is(arena, mixop_meta, m::VARI)? => {
                decode_var(arena, *value_id, *value_typ, *value_iters)
            }
            _ => Err(Error::unexpected(arena, value_meta, "id typ iter*")),
        }
    }
}

/// Reconstructs a variable's type and iteration dimensions.
fn decode_var(
    arena: &mut Arena,
    value_id: Value,
    value_typ: Value,
    value_iters: Value,
) -> Result<il::Var, Error> {
    Ok(il::Var {
        id: <Id as Decode>::decode(arena, value_id)?,
        typ: <il::Typ as Decode>::decode(arena, value_typ)?,
        iters: list(arena, value_iters, iter)?,
    })
}

/// Maps a checked optional meta value.
fn opt<T>(
    arena: &mut Arena,
    value_meta: Value,
    func_decode: impl FnOnce(&mut Arena, Value) -> Result<T, Error>,
) -> Result<Option<T>, Error> {
    get::opt(arena, &value_meta)?
        .map(|value_inner| func_decode(arena, value_inner))
        .transpose()
}

// == Types

/// Decodes a source type, checking every constructor's shape.
impl Decode for il::Typ {
    fn decode(arena: &mut Arena, value_meta: Value) -> Result<Self, Error> {
        let (mixop_meta, values_meta, span) = case(arena, value_meta)?;
        let typ_kind_al = match values_meta.as_slice() {
            [] if m::is(arena, mixop_meta, m::BOOL_TYP)? => il::TypKind::Bool,
            [] if m::is(arena, mixop_meta, m::NAT_TYP)? => il::TypKind::Num(num::Typ::Nat),
            [] if m::is(arena, mixop_meta, m::INT_TYP)? => il::TypKind::Num(num::Typ::Int),
            [] if m::is(arena, mixop_meta, m::TEXT_TYP)? => il::TypKind::Text,
            [value_id, value_typs] if m::is(arena, mixop_meta, m::VAR_TYP)? => {
                decode_var_typ(arena, *value_id, *value_typs)?
            }
            [value_typs] if m::is(arena, mixop_meta, m::TUP_TYP)? => {
                decode_tuple_typ(arena, *value_typs)?
            }
            [value_typ, value_iter] if m::is(arena, mixop_meta, m::ITER_TYP)? => {
                decode_iter_typ(arena, *value_typ, *value_iter)?
            }
            [value_ids, value_typs, value_ret] if m::is(arena, mixop_meta, m::FUNC_TYP)? => {
                decode_func_typ(arena, *value_ids, *value_typs, *value_ret)?
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

fn decode_var_typ(
    arena: &mut Arena,
    value_id: Value,
    value_typs: Value,
) -> Result<il::TypKind, Error> {
    Ok(il::TypKind::Var(
        <Id as Decode>::decode(arena, value_id)?,
        <Vec<il::Typ> as Decode>::decode(arena, value_typs)?,
    ))
}

fn decode_tuple_typ(arena: &mut Arena, value_typs: Value) -> Result<il::TypKind, Error> {
    Ok(il::TypKind::Tuple(<Vec<il::Typ> as Decode>::decode(arena, value_typs)?))
}

fn decode_iter_typ(
    arena: &mut Arena,
    value_typ: Value,
    value_iter: Value,
) -> Result<il::TypKind, Error> {
    Ok(il::TypKind::Iter(
        Box::new(<il::Typ as Decode>::decode(arena, value_typ)?),
        iter(arena, value_iter)?,
    ))
}

/// Reconstructs a function type and its type parameters.
fn decode_func_typ(
    arena: &mut Arena,
    value_ids: Value,
    value_typs: Value,
    value_ret: Value,
) -> Result<il::TypKind, Error> {
    Ok(il::TypKind::Func(il::FuncTyp {
        tparams: list(arena, value_ids, <Id as Decode>::decode)?,
        typs_params: <Vec<il::Typ> as Decode>::decode(arena, value_typs)?,
        typ_ret: Box::new(<il::Typ as Decode>::decode(arena, value_ret)?),
    }))
}

/// Decodes an owned list of source types.
impl Decode for Vec<il::Typ> {
    fn decode(arena: &mut Arena, value_meta: Value) -> Result<Self, Error> {
        list(arena, value_meta, <il::Typ as Decode>::decode)
    }
}

/// Decodes an alias, struct, or variant without general hints.
impl Decode for il::DefTyp {
    fn decode(arena: &mut Arena, value_meta: Value) -> Result<Self, Error> {
        let (mixop_meta, values_meta, span) = case(arena, value_meta)?;
        let deftyp_kind_al = match values_meta.as_slice() {
            [value_typ] if m::is(arena, mixop_meta, m::ALIAS)? => {
                decode_alias_deftyp(arena, *value_typ)?
            }
            [value_fields] if m::is(arena, mixop_meta, m::STRUCT)? => {
                decode_struct_deftyp(arena, *value_fields)?
            }
            [value_cases] if m::is(arena, mixop_meta, m::VARIANT)? => {
                decode_variant_deftyp(arena, *value_cases)?
            }
            _ => return Err(Error::unexpected(arena, value_meta, "ALIAS, STRUCT, or VARIANT")),
        };
        Ok(phrase!(node: deftyp_kind_al, span: span))
    }
}

fn decode_alias_deftyp(arena: &mut Arena, value_typ: Value) -> Result<il::DefTypKind, Error> {
    Ok(il::DefTypKind::Plain(<il::Typ as Decode>::decode(arena, value_typ)?))
}

fn decode_struct_deftyp(arena: &mut Arena, value_fields: Value) -> Result<il::DefTypKind, Error> {
    Ok(il::DefTypKind::Struct(list(arena, value_fields, typfield)?))
}

fn decode_variant_deftyp(arena: &mut Arena, value_cases: Value) -> Result<il::DefTypKind, Error> {
    Ok(il::DefTypKind::Variant(list(arena, value_cases, typcase)?))
}

/// Decodes one struct type field.
fn typfield(arena: &mut Arena, value_meta: Value) -> Result<il::TypField, Error> {
    let (mixop_meta, values_meta, _) = case(arena, value_meta)?;
    match values_meta.as_slice() {
        [value_atom, value_typ] if m::is(arena, mixop_meta, m::TYPFIELD)? => {
            decode_typfield(arena, *value_atom, *value_typ)
        }
        _ => Err(Error::unexpected(arena, value_meta, "atom typ")),
    }
}

fn decode_typfield(
    arena: &mut Arena,
    value_atom: Value,
    value_typ: Value,
) -> Result<il::TypField, Error> {
    Ok(il::TypField {
        atom: atom(arena, value_atom)?,
        typ: <il::Typ as Decode>::decode(arena, value_typ)?,
    })
}

/// Decodes one variant case without its origin or hints.
fn typcase(arena: &mut Arena, value_meta: Value) -> Result<il::TypCase, Error> {
    let (mixop_meta, values_meta, span) = case(arena, value_meta)?;
    match values_meta.as_slice() {
        [value_mixop, value_typs] if m::is(arena, mixop_meta, m::TYPCASE)? => {
            decode_typcase(arena, *value_mixop, *value_typs, span)
        }
        _ => Err(Error::unexpected(arena, value_meta, "mixop typ*")),
    }
}

/// Rebuilds a variant case with an empty origin and no hints.
fn decode_typcase(
    arena: &mut Arena,
    value_mixop: Value,
    value_typs: Value,
    span: Span,
) -> Result<il::TypCase, Error> {
    // Decode the notation and its type arguments
    let mixfix_typ_al = Mixfix::new(
        mixop(arena, value_mixop)?,
        <Vec<il::Typ> as Decode>::decode(arena, value_typs)?,
    )?;
    // Supply the canonical origin omitted from the meta representation
    let id_origin_al = phrase!(node: "".into(), span: span);
    let typ_origin_al =
        phrase!(node: il::TypOriginKind { id: id_origin_al, targs: vec![] }, span: span);
    // Attach the decoded span and omit general hints
    Ok(il::TypCase {
        not_typ: phrase!(node: mixfix_typ_al, span: span),
        typ_origin: typ_origin_al,
        hints: vec![],
    })
}

// == Values

/// Decodes a runtime payload, allocating only in the supplied arena.
impl Decode for Value {
    fn decode(arena: &mut Arena, value_meta: Value) -> Result<Self, Error> {
        let (mixop_meta, values_meta, span) = case(arena, value_meta)?;
        let value_target = match values_meta.as_slice() {
            [value_bool] if m::is(arena, mixop_meta, m::BOOL)? => {
                decode_bool_value(arena, *value_bool, span)?
            }
            [value_num] if m::is(arena, mixop_meta, m::NAT)? => {
                decode_nat_value(arena, *value_num, span)?
            }
            [value_num] if m::is(arena, mixop_meta, m::INT)? => {
                decode_int_value(arena, *value_num, span)?
            }
            [value_text] if m::is(arena, mixop_meta, m::TEXT)? => {
                decode_text_value(arena, *value_text, span)?
            }
            [value_fields] if m::is(arena, mixop_meta, m::STR)? => {
                decode_struct_value(arena, *value_fields, span)?
            }
            [value_case] if m::is(arena, mixop_meta, m::INJ)? => {
                decode_case_value(arena, *value_case, span)?
            }
            [value_values] if m::is(arena, mixop_meta, m::TUP)? => {
                decode_tuple_value(arena, *value_values, span)?
            }
            [value_opt] if m::is(arena, mixop_meta, m::OPT)? => {
                decode_opt_value(arena, *value_opt, span)?
            }
            [value_values] if m::is(arena, mixop_meta, m::LIST)? => {
                decode_list_value(arena, *value_values, span)?
            }
            [value_id] if m::is(arena, mixop_meta, m::FUNC)? => {
                decode_func_value(arena, *value_id, span)?
            }
            [value_json] if m::is(arena, mixop_meta, m::EXT)? => {
                decode_external_value(arena, *value_json, span)?
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

fn decode_bool_value(arena: &mut Arena, value_bool: Value, span: Span) -> Result<Value, Error> {
    Ok(make::bool(arena, get::bool(arena, &value_bool)?, span)?)
}

/// Checks a natural payload before allocating its runtime value.
fn decode_nat_value(arena: &mut Arena, value_num: Value, span: Span) -> Result<Value, Error> {
    let Number::Nat(nat) = get::num(arena, &value_num)? else {
        return Err(Error::unexpected(arena, value_num, "a natural-number payload"));
    };
    let nat = nat.clone();
    Ok(make::nat(arena, nat, span)?)
}

/// Converts a numeric payload to its canonical integer representation.
fn decode_int_value(arena: &mut Arena, value_num: Value, span: Span) -> Result<Value, Error> {
    let num = get::num(arena, &value_num)?.clone();
    Ok(make::int(arena, crate::lang::common::prim::num::to_int(&num).clone(), span)?)
}

fn decode_text_value(arena: &mut Arena, value_text: Value, span: Span) -> Result<Value, Error> {
    Ok(make::text(arena, get::text(arena, &value_text)?.to_owned(), span)?)
}

/// Copies field handles before recursively allocating their payloads.
fn decode_struct_value(arena: &mut Arena, value_fields: Value, span: Span) -> Result<Value, Error> {
    let values_fields = get::list(arena, &value_fields)?.to_vec();
    let value_fields_target = values_fields
        .into_iter()
        .map(|value_meta| valuefield(arena, value_meta))
        .collect::<Result<_, _>>()?;
    Ok(make::structure(
        arena,
        make_typ::var(phrase!(node: "val".into(), span: Span::default()), vec![])
            .node
            .into(),
        value_fields_target,
        span,
    )?)
}

/// Checks case notation before recursively allocating its arguments.
fn decode_case_value(arena: &mut Arena, value_case: Value, span: Span) -> Result<Value, Error> {
    // Check the nested case shape before decoding its children
    let (mixop_meta, values_meta, _) = case(arena, value_case)?;
    let [value_mixop, value_values] = values_meta.as_slice() else {
        return Err(Error::unexpected(arena, value_case, "mixop val*"));
    };
    // Reject constructors with the right arity but the wrong notation
    if !m::is(arena, mixop_meta, m::VALCASE)? {
        return Err(Error::unexpected(arena, value_case, "mixop val*"));
    }
    // Rebuild the notation with its decoded runtime arguments
    let mixop_target = mixop(arena, *value_mixop)?;
    let values_target = <Vec<Value> as Decode>::decode(arena, *value_values)?;
    let mixfix_target = Mixfix::new(mixop_target, values_target)?;
    Ok(make::case(
        arena,
        make_typ::var(phrase!(node: "val".into(), span: Span::default()), vec![])
            .node
            .into(),
        mixfix_target,
        span,
    )?)
}

fn decode_tuple_value(arena: &mut Arena, value_values: Value, span: Span) -> Result<Value, Error> {
    let values_target = <Vec<Value> as Decode>::decode(arena, value_values)?;
    Ok(make::tuple(
        arena,
        make_typ::var(phrase!(node: "val".into(), span: Span::default()), vec![])
            .node
            .into(),
        values_target,
        span,
    )?)
}

fn decode_opt_value(arena: &mut Arena, value_opt: Value, span: Span) -> Result<Value, Error> {
    let value_target = get::opt(arena, &value_opt)?
        .map(|value_meta| <Value as Decode>::decode(arena, value_meta))
        .transpose()?;
    Ok(make::opt(
        arena,
        make_typ::iter(
            make_typ::var(phrase!(node: "val".into(), span: Span::default()), vec![]),
            Iter::Opt,
        )
        .node
        .into(),
        value_target,
        span,
    )?)
}

fn decode_list_value(arena: &mut Arena, value_values: Value, span: Span) -> Result<Value, Error> {
    let values_target = <Vec<Value> as Decode>::decode(arena, value_values)?;
    Ok(make::list(
        arena,
        make_typ::iter(
            make_typ::var(phrase!(node: "val".into(), span: Span::default()), vec![]),
            Iter::List,
        )
        .node
        .into(),
        values_target,
        span,
    )?)
}

fn decode_func_value(arena: &mut Arena, value_id: Value, span: Span) -> Result<Value, Error> {
    let id_target = <Id as Decode>::decode(arena, value_id)?;
    Ok(make::func(arena, id_target, vec![], vec![], make_typ::bool(), span)?)
}

fn decode_external_value(arena: &mut Arena, value_json: Value, span: Span) -> Result<Value, Error> {
    Ok(make::external(
        arena,
        make_typ::var(phrase!(node: "json".into(), span: Span::default()), vec![])
            .node
            .into(),
        get::external(arena, &value_json)?.clone(),
        span,
    )?)
}

/// Decodes a meta `val*` into an owned list of runtime handles.
impl Decode for Vec<Value> {
    fn decode(arena: &mut Arena, value_meta: Value) -> Result<Self, Error> {
        let values_meta = get::list(arena, &value_meta)?.to_vec();
        values_meta
            .into_iter()
            .map(|value_meta| <Value as Decode>::decode(arena, value_meta))
            .collect()
    }
}

/// Decodes one runtime struct field.
fn valuefield(arena: &mut Arena, value_meta: Value) -> Result<ValueField, Error> {
    let (mixop_meta, values_meta, _) = case(arena, value_meta)?;
    match values_meta.as_slice() {
        [value_atom, value_inner] if m::is(arena, mixop_meta, m::VALFIELD)? => {
            decode_valuefield(arena, *value_atom, *value_inner)
        }
        _ => Err(Error::unexpected(arena, value_meta, "atom val")),
    }
}

fn decode_valuefield(
    arena: &mut Arena,
    value_atom: Value,
    value_inner: Value,
) -> Result<ValueField, Error> {
    Ok(ValueField {
        atom: atom(arena, value_atom)?,
        value: <Value as Decode>::decode(arena, value_inner)?,
    })
}

// == Expressions and paths

/// Reconstructs an expression suitable for editing and re-encoding.
impl Decode for il::Exp {
    fn decode(arena: &mut Arena, value_meta: Value) -> Result<Self, Error> {
        let (mixop_meta, values_meta, span) = case(arena, value_meta)?;
        let exp_kind_al = match values_meta.as_slice() {
            [value_bool] if m::is(arena, mixop_meta, m::BOOL)? => {
                il::ExpKind::Bool(get::bool(arena, value_bool)?)
            }
            [value_num] if m::is(arena, mixop_meta, m::NAT)? => decode_nat_exp(arena, *value_num)?,
            [value_num] if m::is(arena, mixop_meta, m::INT)? => decode_int_exp(arena, *value_num)?,
            [value_text] if m::is(arena, mixop_meta, m::TEXT)? => {
                il::ExpKind::Text(get::text(arena, value_text)?.into())
            }
            [value_id] if m::is(arena, mixop_meta, m::VAR_EXP)? => decode_id_exp(arena, *value_id)?,
            [value_op, value_exp] if m::is(arena, mixop_meta, m::UN)? => {
                decode_un_exp(arena, *value_op, *value_exp)?
            }
            [value_op, value_l, value_r] if m::is(arena, mixop_meta, m::BIN)? => {
                decode_bin_exp(arena, *value_op, *value_l, *value_r)?
            }
            [value_op, value_l, value_r] if m::is(arena, mixop_meta, m::CMP)? => {
                decode_cmp_exp(arena, *value_op, *value_l, *value_r)?
            }
            [value_typ, value_exp] if m::is(arena, mixop_meta, m::UPCAST)? => {
                decode_upcast_exp(arena, *value_typ, *value_exp)?
            }
            [value_typ, value_exp] if m::is(arena, mixop_meta, m::DOWNCAST)? => {
                decode_downcast_exp(arena, *value_typ, *value_exp)?
            }
            [value_exp, value_typ] if m::is(arena, mixop_meta, m::SUB_EXP)? => {
                decode_sub_exp(arena, *value_exp, *value_typ)?
            }
            [value_exp, value_pattern] if m::is(arena, mixop_meta, m::MATCH)? => {
                decode_match_exp(arena, *value_exp, *value_pattern)?
            }
            [value_exps] if m::is(arena, mixop_meta, m::TUP)? => {
                decode_tuple_exp(arena, *value_exps)?
            }
            [value_case] if m::is(arena, mixop_meta, m::INJ)? => {
                decode_case_exp(arena, *value_case)?
            }
            [value_fields] if m::is(arena, mixop_meta, m::STR)? => {
                decode_struct_exp(arena, *value_fields)?
            }
            [value_opt] if m::is(arena, mixop_meta, m::OPT)? => decode_opt_exp(arena, *value_opt)?,
            [value_exps] if m::is(arena, mixop_meta, m::LIST)? => {
                decode_list_exp(arena, *value_exps)?
            }
            [value_head, value_tail] if m::is(arena, mixop_meta, m::CONS)? => {
                decode_cons_exp(arena, *value_head, *value_tail)?
            }
            [value_l, value_r] if m::is(arena, mixop_meta, m::CAT)? => {
                decode_cat_exp(arena, *value_l, *value_r)?
            }
            [value_elem, value_list] if m::is(arena, mixop_meta, m::MEM)? => {
                decode_mem_exp(arena, *value_elem, *value_list)?
            }
            [value_exp] if m::is(arena, mixop_meta, m::LEN)? => decode_len_exp(arena, *value_exp)?,
            [value_base, value_atom] if m::is(arena, mixop_meta, m::DOT)? => {
                decode_dot_exp(arena, *value_base, *value_atom)?
            }
            [value_base, value_idx] if m::is(arena, mixop_meta, m::IDX)? => {
                decode_idx_exp(arena, *value_base, *value_idx)?
            }
            [value_base, value_idx, value_len] if m::is(arena, mixop_meta, m::SLICE)? => {
                decode_slice_exp(arena, *value_base, *value_idx, *value_len)?
            }
            [value_base, value_path, value_upd] if m::is(arena, mixop_meta, m::UPD)? => {
                decode_upd_exp(arena, *value_base, *value_path, *value_upd)?
            }
            [value_id, value_typs, value_args] if m::is(arena, mixop_meta, m::CALL)? => {
                decode_call_exp(arena, *value_id, *value_typs, *value_args)?
            }
            [value_exp, value_iter] if m::is(arena, mixop_meta, m::ITER_EXP)? => {
                decode_iter_exp(arena, *value_exp, *value_iter)?
            }
            _ => {
                return Err(Error::unexpected(
                    arena,
                    value_meta,
                    "an AL expression constructor with its declared arguments",
                ));
            }
        };
        Ok(note_phrase!(node: exp_kind_al, note: il::TypKind::Bool, span: span))
    }
}

/// Rejects non-natural payloads before rebuilding a numeric expression.
fn decode_nat_exp(arena: &mut Arena, value_num: Value) -> Result<il::ExpKind, Error> {
    let num::Number::Nat(nat) = get::num(arena, &value_num)? else {
        return Err(Error::unexpected(arena, value_num, "a natural-number payload"));
    };
    Ok(il::ExpKind::Num(num::Number::Nat(nat.clone())))
}

/// Converts a numeric payload to the canonical integer expression.
fn decode_int_exp(arena: &mut Arena, value_num: Value) -> Result<il::ExpKind, Error> {
    Ok(il::ExpKind::Num(num::Number::Int(num::to_int(get::num(arena, &value_num)?).clone())))
}

fn decode_id_exp(arena: &mut Arena, value_id: Value) -> Result<il::ExpKind, Error> {
    Ok(il::ExpKind::Id(<Id as Decode>::decode(arena, value_id)?))
}

fn decode_un_exp(
    arena: &mut Arena,
    value_op: Value,
    value_exp: Value,
) -> Result<il::ExpKind, Error> {
    let (op_al, typ_op_al) = unop(arena, value_op)?;
    Ok(il::ExpKind::Un(op_al, typ_op_al, Box::new(<il::Exp as Decode>::decode(arena, value_exp)?)))
}

/// Reconstructs a binary expression with its canonical operator type.
fn decode_bin_exp(
    arena: &mut Arena,
    value_op: Value,
    value_l: Value,
    value_r: Value,
) -> Result<il::ExpKind, Error> {
    let (op_al, typ_op_al) = binop(arena, value_op)?;
    Ok(il::ExpKind::Bin(
        op_al,
        typ_op_al,
        Box::new(<il::Exp as Decode>::decode(arena, value_l)?),
        Box::new(<il::Exp as Decode>::decode(arena, value_r)?),
    ))
}

/// Reconstructs a comparison with its canonical operator type.
fn decode_cmp_exp(
    arena: &mut Arena,
    value_op: Value,
    value_l: Value,
    value_r: Value,
) -> Result<il::ExpKind, Error> {
    let (op_al, typ_op_al) = cmpop(arena, value_op)?;
    Ok(il::ExpKind::Cmp(
        op_al,
        typ_op_al,
        Box::new(<il::Exp as Decode>::decode(arena, value_l)?),
        Box::new(<il::Exp as Decode>::decode(arena, value_r)?),
    ))
}

fn decode_upcast_exp(
    arena: &mut Arena,
    value_typ: Value,
    value_exp: Value,
) -> Result<il::ExpKind, Error> {
    Ok(il::ExpKind::UpCast(
        Box::new(<il::Typ as Decode>::decode(arena, value_typ)?),
        Box::new(<il::Exp as Decode>::decode(arena, value_exp)?),
    ))
}

fn decode_downcast_exp(
    arena: &mut Arena,
    value_typ: Value,
    value_exp: Value,
) -> Result<il::ExpKind, Error> {
    Ok(il::ExpKind::DownCast(
        Box::new(<il::Typ as Decode>::decode(arena, value_typ)?),
        Box::new(<il::Exp as Decode>::decode(arena, value_exp)?),
    ))
}

/// Rebuilds a subtype test with a conservative recursive check.
fn decode_sub_exp(
    arena: &mut Arena,
    value_exp: Value,
    value_typ: Value,
) -> Result<il::ExpKind, Error> {
    let typ_al = <il::Typ as Decode>::decode(arena, value_typ)?;
    let subcheck_al = il::Subcheck::Recurse(typ_al.clone());
    Ok(il::ExpKind::Sub(
        Box::new(<il::Exp as Decode>::decode(arena, value_exp)?),
        Box::new(typ_al),
        Box::new(subcheck_al),
    ))
}

fn decode_match_exp(
    arena: &mut Arena,
    value_exp: Value,
    value_pattern: Value,
) -> Result<il::ExpKind, Error> {
    Ok(il::ExpKind::Match(
        Box::new(<il::Exp as Decode>::decode(arena, value_exp)?),
        pattern(arena, value_pattern)?,
    ))
}

fn decode_tuple_exp(arena: &mut Arena, value_exps: Value) -> Result<il::ExpKind, Error> {
    Ok(il::ExpKind::Tuple(<Vec<il::Exp> as Decode>::decode(arena, value_exps)?))
}

fn decode_case_exp(arena: &mut Arena, value_case: Value) -> Result<il::ExpKind, Error> {
    Ok(il::ExpKind::Case(Box::new(expcase(arena, value_case)?)))
}

fn decode_struct_exp(arena: &mut Arena, value_fields: Value) -> Result<il::ExpKind, Error> {
    Ok(il::ExpKind::Str(list(arena, value_fields, expfield)?))
}

fn decode_opt_exp(arena: &mut Arena, value_opt: Value) -> Result<il::ExpKind, Error> {
    Ok(il::ExpKind::Opt(opt(arena, value_opt, <il::Exp as Decode>::decode)?.map(Box::new)))
}

fn decode_list_exp(arena: &mut Arena, value_exps: Value) -> Result<il::ExpKind, Error> {
    Ok(il::ExpKind::List(<Vec<il::Exp> as Decode>::decode(arena, value_exps)?))
}

fn decode_cons_exp(
    arena: &mut Arena,
    value_head: Value,
    value_tail: Value,
) -> Result<il::ExpKind, Error> {
    Ok(il::ExpKind::Cons(
        Box::new(<il::Exp as Decode>::decode(arena, value_head)?),
        Box::new(<il::Exp as Decode>::decode(arena, value_tail)?),
    ))
}

fn decode_cat_exp(arena: &mut Arena, value_l: Value, value_r: Value) -> Result<il::ExpKind, Error> {
    Ok(il::ExpKind::Cat(
        Box::new(<il::Exp as Decode>::decode(arena, value_l)?),
        Box::new(<il::Exp as Decode>::decode(arena, value_r)?),
    ))
}

fn decode_mem_exp(
    arena: &mut Arena,
    value_elem: Value,
    value_list: Value,
) -> Result<il::ExpKind, Error> {
    Ok(il::ExpKind::Mem(
        Box::new(<il::Exp as Decode>::decode(arena, value_elem)?),
        Box::new(<il::Exp as Decode>::decode(arena, value_list)?),
    ))
}

fn decode_len_exp(arena: &mut Arena, value_exp: Value) -> Result<il::ExpKind, Error> {
    Ok(il::ExpKind::Len(Box::new(<il::Exp as Decode>::decode(arena, value_exp)?)))
}

fn decode_dot_exp(
    arena: &mut Arena,
    value_base: Value,
    value_atom: Value,
) -> Result<il::ExpKind, Error> {
    Ok(il::ExpKind::Dot(
        Box::new(<il::Exp as Decode>::decode(arena, value_base)?),
        atom(arena, value_atom)?,
    ))
}

fn decode_idx_exp(
    arena: &mut Arena,
    value_base: Value,
    value_idx: Value,
) -> Result<il::ExpKind, Error> {
    Ok(il::ExpKind::Idx(
        Box::new(<il::Exp as Decode>::decode(arena, value_base)?),
        Box::new(<il::Exp as Decode>::decode(arena, value_idx)?),
    ))
}

/// Reconstructs a slice from its base, index, and length expressions.
fn decode_slice_exp(
    arena: &mut Arena,
    value_base: Value,
    value_idx: Value,
    value_len: Value,
) -> Result<il::ExpKind, Error> {
    Ok(il::ExpKind::Slice(
        Box::new(<il::Exp as Decode>::decode(arena, value_base)?),
        Box::new(<il::Exp as Decode>::decode(arena, value_idx)?),
        Box::new(<il::Exp as Decode>::decode(arena, value_len)?),
    ))
}

/// Reconstructs an update from its base, path, and replacement.
fn decode_upd_exp(
    arena: &mut Arena,
    value_base: Value,
    value_path: Value,
    value_upd: Value,
) -> Result<il::ExpKind, Error> {
    Ok(il::ExpKind::Upd(
        Box::new(<il::Exp as Decode>::decode(arena, value_base)?),
        Box::new(path(arena, value_path)?),
        Box::new(<il::Exp as Decode>::decode(arena, value_upd)?),
    ))
}

fn decode_call_exp(
    arena: &mut Arena,
    value_id: Value,
    value_typs: Value,
    value_args: Value,
) -> Result<il::ExpKind, Error> {
    Ok(il::ExpKind::Call(
        <Id as Decode>::decode(arena, value_id)?,
        <Vec<il::Typ> as Decode>::decode(arena, value_typs)?,
        args(arena, value_args)?,
    ))
}

fn decode_iter_exp(
    arena: &mut Arena,
    value_exp: Value,
    value_iter: Value,
) -> Result<il::ExpKind, Error> {
    Ok(il::ExpKind::Iter(
        Box::new(<il::Exp as Decode>::decode(arena, value_exp)?),
        iterexp(arena, value_iter)?,
    ))
}

/// Decodes an owned source expression list.
impl Decode for Vec<il::Exp> {
    fn decode(arena: &mut Arena, value_meta: Value) -> Result<Self, Error> {
        list(arena, value_meta, <il::Exp as Decode>::decode)
    }
}

/// Decodes a notation and its expression arguments.
fn expcase(arena: &mut Arena, value_meta: Value) -> Result<il::NotExp, Error> {
    let (mixop_meta, values_meta, _) = case(arena, value_meta)?;
    match values_meta.as_slice() {
        [value_mixop, value_exps] if m::is(arena, mixop_meta, m::EXPCASE)? => {
            decode_expcase(arena, *value_mixop, *value_exps)
        }
        _ => Err(Error::unexpected(arena, value_meta, "mixop exp*")),
    }
}

fn decode_expcase(
    arena: &mut Arena,
    value_mixop: Value,
    value_exps: Value,
) -> Result<il::NotExp, Error> {
    Ok(Mixfix::new(
        mixop(arena, value_mixop)?,
        <Vec<il::Exp> as Decode>::decode(arena, value_exps)?,
    )?)
}

/// Decodes one source struct-expression field.
fn expfield(arena: &mut Arena, value_meta: Value) -> Result<il::ExpField, Error> {
    let (mixop_meta, values_meta, _) = case(arena, value_meta)?;
    match values_meta.as_slice() {
        [value_atom, value_exp] if m::is(arena, mixop_meta, m::EXPFIELD)? => {
            decode_expfield(arena, *value_atom, *value_exp)
        }
        _ => Err(Error::unexpected(arena, value_meta, "atom exp")),
    }
}

fn decode_expfield(
    arena: &mut Arena,
    value_atom: Value,
    value_exp: Value,
) -> Result<il::ExpField, Error> {
    Ok(il::ExpField {
        atom: atom(arena, value_atom)?,
        exp: <il::Exp as Decode>::decode(arena, value_exp)?,
    })
}

/// Decodes an expression iterator and its variables.
fn iterexp(arena: &mut Arena, value_meta: Value) -> Result<il::ExpIter, Error> {
    let (mixop_meta, values_meta, _) = case(arena, value_meta)?;
    match values_meta.as_slice() {
        [value_iter, value_vars] if m::is(arena, mixop_meta, m::ITEREXP)? => {
            decode_iterexp(arena, *value_iter, *value_vars)
        }
        _ => Err(Error::unexpected(arena, value_meta, "iter vari*")),
    }
}

fn decode_iterexp(
    arena: &mut Arena,
    value_iter: Value,
    value_vars: Value,
) -> Result<il::ExpIter, Error> {
    Ok(il::ExpIter {
        iter: iter(arena, value_iter)?,
        vars: list(arena, value_vars, <il::Var as Decode>::decode)?,
    })
}

/// Decodes a value or function argument.
fn arg(arena: &mut Arena, value_meta: Value) -> Result<il::Arg, Error> {
    let (mixop_meta, values_meta, span) = case(arena, value_meta)?;
    let arg_kind_al = match values_meta.as_slice() {
        [value_exp] if m::is(arena, mixop_meta, m::EXP_ARG)? => decode_exp_arg(arena, *value_exp)?,
        [value_id] if m::is(arena, mixop_meta, m::FUN_ARG)? => decode_func_arg(arena, *value_id)?,
        _ => return Err(Error::unexpected(arena, value_meta, "EXP exp or FUN id")),
    };
    Ok(phrase!(node: arg_kind_al, span: span))
}

fn decode_exp_arg(arena: &mut Arena, value_exp: Value) -> Result<il::ArgKind, Error> {
    Ok(il::ArgKind::Exp(Box::new(<il::Exp as Decode>::decode(arena, value_exp)?)))
}

fn decode_func_arg(arena: &mut Arena, value_id: Value) -> Result<il::ArgKind, Error> {
    Ok(il::ArgKind::Def(<Id as Decode>::decode(arena, value_id)?))
}

/// Decodes an owned argument list.
fn args(arena: &mut Arena, value_meta: Value) -> Result<Vec<il::Arg>, Error> {
    list(arena, value_meta, arg)
}

/// Decodes a case, list, or option shape pattern.
fn pattern(arena: &mut Arena, value_meta: Value) -> Result<il::Pattern, Error> {
    let (mixop_meta, values_meta, _) = case(arena, value_meta)?;
    Ok(match values_meta.as_slice() {
        [value_mixop] if m::is(arena, mixop_meta, m::CASE_PATTERN)? => {
            decode_case_pattern(arena, *value_mixop)?
        }
        [] if m::is(arena, mixop_meta, m::CONS_PATTERN)? => {
            il::Pattern::List(il::ListPattern::Cons)
        }
        [value_nat] if m::is(arena, mixop_meta, m::FIXED)? => {
            decode_fixed_pattern(arena, *value_nat)?
        }
        [] if m::is(arena, mixop_meta, m::NIL)? => il::Pattern::List(il::ListPattern::Nil),
        [] if m::is(arena, mixop_meta, m::SOME)? => il::Pattern::Opt(il::OptPattern::Some),
        [] if m::is(arena, mixop_meta, m::NONE)? => il::Pattern::Opt(il::OptPattern::None),
        _ => return Err(Error::unexpected(arena, value_meta, "an AL shape pattern")),
    })
}

fn decode_case_pattern(arena: &mut Arena, value_mixop: Value) -> Result<il::Pattern, Error> {
    Ok(il::Pattern::Case(mixop(arena, value_mixop)?))
}

/// Checks that a fixed-list length fits the native pattern representation.
fn decode_fixed_pattern(arena: &mut Arena, value_nat: Value) -> Result<il::Pattern, Error> {
    // Require a natural payload before checking its native size
    let num::Number::Nat(nat) = get::num(arena, &value_nat)? else {
        return Err(Error::unexpected(arena, value_nat, "a natural list length"));
    };
    // Reject lengths that cannot be represented by the host
    let num_len = nat
        .as_bigint()
        .to_usize()
        .ok_or_else(|| Error::unexpected(arena, value_nat, "a list length fitting usize"))?;
    Ok(il::Pattern::List(il::ListPattern::Fixed(num_len)))
}

/// Decodes a source update path.
fn path(arena: &mut Arena, value_meta: Value) -> Result<il::Path, Error> {
    let (mixop_meta, values_meta, span) = case(arena, value_meta)?;
    let path_kind_al = match values_meta.as_slice() {
        [] if m::is(arena, mixop_meta, m::ROOT)? => il::PathKind::Root,
        [value_path, value_idx] if m::is(arena, mixop_meta, m::IDX)? => {
            decode_idx_path(arena, *value_path, *value_idx)?
        }
        [value_path, value_idx, value_len] if m::is(arena, mixop_meta, m::SLICE)? => {
            decode_slice_path(arena, *value_path, *value_idx, *value_len)?
        }
        [value_path, value_atom] if m::is(arena, mixop_meta, m::DOT)? => {
            decode_dot_path(arena, *value_path, *value_atom)?
        }
        _ => return Err(Error::unexpected(arena, value_meta, "an AL update path")),
    };
    Ok(note_phrase!(node: path_kind_al, note: il::TypKind::Bool, span: span))
}

fn decode_idx_path(
    arena: &mut Arena,
    value_path: Value,
    value_idx: Value,
) -> Result<il::PathKind, Error> {
    Ok(il::PathKind::Idx(
        Box::new(path(arena, value_path)?),
        Box::new(<il::Exp as Decode>::decode(arena, value_idx)?),
    ))
}

/// Reconstructs a slice path and its bound expressions.
fn decode_slice_path(
    arena: &mut Arena,
    value_path: Value,
    value_idx: Value,
    value_len: Value,
) -> Result<il::PathKind, Error> {
    Ok(il::PathKind::Slice(
        Box::new(path(arena, value_path)?),
        Box::new(<il::Exp as Decode>::decode(arena, value_idx)?),
        Box::new(<il::Exp as Decode>::decode(arena, value_len)?),
    ))
}

fn decode_dot_path(
    arena: &mut Arena,
    value_path: Value,
    value_atom: Value,
) -> Result<il::PathKind, Error> {
    Ok(il::PathKind::Dot(Box::new(path(arena, value_path)?), atom(arena, value_atom)?))
}

/// Decodes a unary operator.
fn unop(arena: &mut Arena, value_meta: Value) -> Result<(il::UnOp, il::OpTyp), Error> {
    let (mixop_meta, values_meta, _) = case(arena, value_meta)?;
    match values_meta.as_slice() {
        [] if m::is(arena, mixop_meta, m::NOT)? => {
            Ok((il::UnOp::Bool(boolean::UnOp::Not), il::OpTyp::Bool))
        }
        [] if m::is(arena, mixop_meta, m::PLUS)? => {
            Ok((il::UnOp::Num(num::UnOp::Plus), il::OpTyp::Nat))
        }
        [] if m::is(arena, mixop_meta, m::MINUS)? => {
            Ok((il::UnOp::Num(num::UnOp::Minus), il::OpTyp::Nat))
        }
        _ => Err(Error::unexpected(arena, value_meta, "a nullary unop constructor")),
    }
}

/// Decodes a binary operator.
fn binop(arena: &mut Arena, value_meta: Value) -> Result<(il::BinOp, il::OpTyp), Error> {
    let (mixop_meta, values_meta, _) = case(arena, value_meta)?;
    match values_meta.as_slice() {
        [] if m::is(arena, mixop_meta, m::AND)? => {
            Ok((il::BinOp::Bool(boolean::BinOp::And), il::OpTyp::Bool))
        }
        [] if m::is(arena, mixop_meta, m::OR)? => {
            Ok((il::BinOp::Bool(boolean::BinOp::Or), il::OpTyp::Bool))
        }
        [] if m::is(arena, mixop_meta, m::IMPL)? => {
            Ok((il::BinOp::Bool(boolean::BinOp::Impl), il::OpTyp::Bool))
        }
        [] if m::is(arena, mixop_meta, m::EQUIV)? => {
            Ok((il::BinOp::Bool(boolean::BinOp::Equiv), il::OpTyp::Bool))
        }
        [] if m::is(arena, mixop_meta, m::ADD)? => {
            Ok((il::BinOp::Num(num::BinOp::Add), il::OpTyp::Nat))
        }
        [] if m::is(arena, mixop_meta, m::SUB)? => {
            Ok((il::BinOp::Num(num::BinOp::Sub), il::OpTyp::Nat))
        }
        [] if m::is(arena, mixop_meta, m::MUL)? => {
            Ok((il::BinOp::Num(num::BinOp::Mul), il::OpTyp::Nat))
        }
        [] if m::is(arena, mixop_meta, m::DIV)? => {
            Ok((il::BinOp::Num(num::BinOp::Div), il::OpTyp::Nat))
        }
        [] if m::is(arena, mixop_meta, m::MOD)? => {
            Ok((il::BinOp::Num(num::BinOp::Mod), il::OpTyp::Nat))
        }
        [] if m::is(arena, mixop_meta, m::POW)? => {
            Ok((il::BinOp::Num(num::BinOp::Pow), il::OpTyp::Nat))
        }
        _ => Err(Error::unexpected(arena, value_meta, "a nullary binop constructor")),
    }
}

/// Decodes a comparison operator.
fn cmpop(arena: &mut Arena, value_meta: Value) -> Result<(il::CmpOp, il::OpTyp), Error> {
    let (mixop_meta, values_meta, _) = case(arena, value_meta)?;
    match values_meta.as_slice() {
        [] if m::is(arena, mixop_meta, m::EQ)? => {
            Ok((il::CmpOp::Bool(boolean::CmpOp::Eq), il::OpTyp::Bool))
        }
        [] if m::is(arena, mixop_meta, m::NE)? => {
            Ok((il::CmpOp::Bool(boolean::CmpOp::Ne), il::OpTyp::Bool))
        }
        [] if m::is(arena, mixop_meta, m::LT)? => {
            Ok((il::CmpOp::Num(num::CmpOp::Lt), il::OpTyp::Nat))
        }
        [] if m::is(arena, mixop_meta, m::LE)? => {
            Ok((il::CmpOp::Num(num::CmpOp::Le), il::OpTyp::Nat))
        }
        [] if m::is(arena, mixop_meta, m::GT)? => {
            Ok((il::CmpOp::Num(num::CmpOp::Gt), il::OpTyp::Nat))
        }
        [] if m::is(arena, mixop_meta, m::GE)? => {
            Ok((il::CmpOp::Num(num::CmpOp::Ge), il::OpTyp::Nat))
        }
        _ => Err(Error::unexpected(arena, value_meta, "a nullary cmpop constructor")),
    }
}

// == Definitions and scripts

/// Decodes a source AL script in definition order.
impl Decode for al::Spec {
    fn decode(arena: &mut Arena, value_meta: Value) -> Result<Self, Error> {
        list(arena, value_meta, <al::Def as Decode>::decode)
    }
}

/// Decodes one definition without inventing omitted general hints.
impl Decode for al::Def {
    fn decode(arena: &mut Arena, value_meta: Value) -> Result<Self, Error> {
        let (mixop_meta, values_meta, span) = case(arena, value_meta)?;
        let def_kind_al = match values_meta.as_slice() {
            [value_id] if m::is(arena, mixop_meta, m::EXTTYP)? => {
                decode_extern_typ_def(arena, *value_id)?
            }
            [value_id, value_ids, value_typ] if m::is(arena, mixop_meta, m::TYP_DEF)? => {
                decode_defined_typ_def(arena, *value_id, *value_ids, *value_typ)?
            }
            [value_id, value_input, value_output] if m::is(arena, mixop_meta, m::EXTREL)? => {
                decode_extern_rel_def(arena, *value_id, *value_input, *value_output, span)?
            }
            [value_id, value_input, value_output, value_groups, value_else]
                if m::is(arena, mixop_meta, m::REL_DEF)? =>
            {
                decode_defined_rel_def(
                    arena,
                    *value_id,
                    *value_input,
                    *value_output,
                    *value_groups,
                    *value_else,
                    span,
                )?
            }
            [value_id, value_ids, value_params, value_typ]
                if m::is(arena, mixop_meta, m::EXTFUNC)? =>
            {
                decode_extern_func_def(arena, *value_id, *value_ids, *value_params, *value_typ)?
            }
            [value_id, value_ids, value_params, value_typ]
                if m::is(arena, mixop_meta, m::BUILTINFUNC)? =>
            {
                decode_builtin_func_def(arena, *value_id, *value_ids, *value_params, *value_typ)?
            }
            [value_id, value_params, value_typ, value_rows]
                if m::is(arena, mixop_meta, m::TABLEFUNC)? =>
            {
                decode_table_func_def(arena, *value_id, *value_params, *value_typ, *value_rows)?
            }
            [value_id, value_ids, value_params, value_typ, value_clauses, value_else]
                if m::is(arena, mixop_meta, m::FUNC_DEF)? =>
            {
                decode_defined_func_def(
                    arena,
                    *value_id,
                    *value_ids,
                    *value_params,
                    *value_typ,
                    *value_clauses,
                    *value_else,
                )?
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

/// Rebuilds an external type without general hints.
fn decode_extern_typ_def(arena: &mut Arena, value_id: Value) -> Result<al::DefKind, Error> {
    Ok(al::DefKind::Typ(al::TypDef::Extern(al::ExternTyp {
        id: <Id as Decode>::decode(arena, value_id)?,
        hints: vec![],
    })))
}

/// Rebuilds a type definition without general hints.
fn decode_defined_typ_def(
    arena: &mut Arena,
    value_id: Value,
    value_ids: Value,
    value_typ: Value,
) -> Result<al::DefKind, Error> {
    Ok(al::DefKind::Typ(al::TypDef::Defined(Box::new(al::DefinedTyp {
        id: <Id as Decode>::decode(arena, value_id)?,
        tparams: list(arena, value_ids, <Id as Decode>::decode)?,
        def_typ: <il::DefTyp as Decode>::decode(arena, value_typ)?,
        hints: vec![],
    }))))
}

/// Rebuilds an external relation with canonical input positions.
fn decode_extern_rel_def(
    arena: &mut Arena,
    value_id: Value,
    value_input: Value,
    value_output: Value,
    span: Span,
) -> Result<al::DefKind, Error> {
    // Reconstruct the input hint from contiguous argument positions
    let (mixfix_typ_al, input_hint) = notation(
        <Vec<il::Typ> as Decode>::decode(arena, value_input)?,
        <Vec<il::Typ> as Decode>::decode(arena, value_output)?,
        span,
    )?;
    // Decode the remaining definition fields without general hints
    Ok(al::DefKind::Rel(al::RelDef::Extern(Box::new(al::ExternRel {
        id: <Id as Decode>::decode(arena, value_id)?,
        not_typ: phrase!(node: mixfix_typ_al, span: span),
        input_hint,
        hints: vec![],
    }))))
}

/// Rebuilds a relation and its rule groups with canonical notation.
fn decode_defined_rel_def(
    arena: &mut Arena,
    value_id: Value,
    value_input: Value,
    value_output: Value,
    value_groups: Value,
    value_else: Value,
    span: Span,
) -> Result<al::DefKind, Error> {
    // Reconstruct the input hint from contiguous argument positions
    let (mixfix_typ_al, input_hint) = notation(
        <Vec<il::Typ> as Decode>::decode(arena, value_input)?,
        <Vec<il::Typ> as Decode>::decode(arena, value_output)?,
        span,
    )?;
    // Decode the remaining definition fields without general hints
    Ok(al::DefKind::Rel(al::RelDef::Defined(Box::new(al::DefinedRel {
        id: <Id as Decode>::decode(arena, value_id)?,
        not_typ: phrase!(node: mixfix_typ_al, span: span),
        input_hint,
        rule_groups: list(arena, value_groups, rulegroup)?,
        else_group: opt(arena, value_else, elsegroup)?,
        hints: vec![],
    }))))
}

/// Rebuilds an external function without general hints.
fn decode_extern_func_def(
    arena: &mut Arena,
    value_id: Value,
    value_ids: Value,
    value_params: Value,
    value_typ: Value,
) -> Result<al::DefKind, Error> {
    Ok(al::DefKind::MetaFunc(al::MetaFuncDef::Extern(al::ExternFunc {
        id: <Id as Decode>::decode(arena, value_id)?,
        tparams: list(arena, value_ids, <Id as Decode>::decode)?,
        params: list(arena, value_params, param)?,
        typ: <il::Typ as Decode>::decode(arena, value_typ)?,
        hints: vec![],
    })))
}

/// Rebuilds a builtin function without general hints.
fn decode_builtin_func_def(
    arena: &mut Arena,
    value_id: Value,
    value_ids: Value,
    value_params: Value,
    value_typ: Value,
) -> Result<al::DefKind, Error> {
    Ok(al::DefKind::MetaFunc(al::MetaFuncDef::Builtin(al::BuiltinFunc {
        id: <Id as Decode>::decode(arena, value_id)?,
        tparams: list(arena, value_ids, <Id as Decode>::decode)?,
        params: list(arena, value_params, param)?,
        typ: <il::Typ as Decode>::decode(arena, value_typ)?,
        hints: vec![],
    })))
}

/// Rebuilds a table function without printing signatures or hints.
fn decode_table_func_def(
    arena: &mut Arena,
    value_id: Value,
    value_params: Value,
    value_typ: Value,
    value_rows: Value,
) -> Result<al::DefKind, Error> {
    Ok(al::DefKind::MetaFunc(al::MetaFuncDef::Table(al::TableFunc {
        id: <Id as Decode>::decode(arena, value_id)?,
        params: list(arena, value_params, param)?,
        typ: <il::Typ as Decode>::decode(arena, value_typ)?,
        table_rows: list(arena, value_rows, tablerow)?,
        hints: vec![],
    })))
}

/// Rebuilds function clauses and the otherwise clause without hints.
fn decode_defined_func_def(
    arena: &mut Arena,
    value_id: Value,
    value_ids: Value,
    value_params: Value,
    value_typ: Value,
    value_clauses: Value,
    value_else: Value,
) -> Result<al::DefKind, Error> {
    Ok(al::DefKind::MetaFunc(al::MetaFuncDef::Defined(Box::new(al::DefinedFunc {
        id: <Id as Decode>::decode(arena, value_id)?,
        tparams: list(arena, value_ids, <Id as Decode>::decode)?,
        params: list(arena, value_params, param)?,
        typ: <il::Typ as Decode>::decode(arena, value_typ)?,
        clauses: list(arena, value_clauses, clause)?,
        else_clause: opt(arena, value_else, clause)?,
        hints: vec![],
    }))))
}

/// Rebuilds relation notation with contiguous input positions.
fn notation<T>(
    args_input: Vec<T>,
    args_output: Vec<T>,
    span: Span,
) -> Result<(Mixfix<T>, InputHint), Error> {
    // Input positions are canonical, independently of the original notation
    let input_hint = InputHint::new(
        (0..args_input.len())
            .map(|idx| phrase!(node: idx, span: span))
            .collect(),
    );
    // Separate canonical input and output positions with the relation arrow
    let mixop_input = Mixop::Seq(vec![Mixop::Arg; args_input.len()]);
    let mixop_output = Mixop::Seq(vec![Mixop::Arg; args_output.len()]);
    let atom_arrow = phrase!(node: Atom::Operator("->".into()), span: span);
    let mixop_notation =
        Rc::new(Mixop::Infix(Box::new(mixop_input), atom_arrow, Box::new(mixop_output)));
    // Validate the operational hint, including the zero-arity case
    let args_notation = input::combine(&input_hint, args_input, args_output)?;
    Ok((Mixfix::new(mixop_notation, args_notation)?, input_hint))
}

/// Decodes a value or higher-order function parameter.
fn param(arena: &mut Arena, value_meta: Value) -> Result<al::Param, Error> {
    let (mixop_meta, values_meta, span) = case(arena, value_meta)?;
    let param_kind_al = match values_meta.as_slice() {
        [value_typ] if m::is(arena, mixop_meta, m::EXP_PARAM)? => {
            decode_exp_param(arena, *value_typ)?
        }
        [value_id, value_ids, value_params, value_typ]
            if m::is(arena, mixop_meta, m::FUN_PARAM)? =>
        {
            decode_func_param(arena, *value_id, *value_ids, *value_params, *value_typ)?
        }
        _ => return Err(Error::unexpected(arena, value_meta, "an AL parameter")),
    };
    Ok(phrase!(node: param_kind_al, span: span))
}

fn decode_exp_param(arena: &mut Arena, value_typ: Value) -> Result<al::ParamKind, Error> {
    Ok(al::ParamKind::Exp(<il::Typ as Decode>::decode(arena, value_typ)?))
}

/// Reconstructs a higher-order parameter and its function signature.
fn decode_func_param(
    arena: &mut Arena,
    value_id: Value,
    value_ids: Value,
    value_params: Value,
    value_typ: Value,
) -> Result<al::ParamKind, Error> {
    Ok(al::ParamKind::Def(
        <Id as Decode>::decode(arena, value_id)?,
        list(arena, value_ids, <Id as Decode>::decode)?,
        list(arena, value_params, param)?,
        <il::Typ as Decode>::decode(arena, value_typ)?,
    ))
}

/// Decodes an AL premise.
fn prem(arena: &mut Arena, value_meta: Value) -> Result<al::Prem, Error> {
    let (mixop_meta, values_meta, span) = case(arena, value_meta)?;
    let prem_kind_al = match values_meta.as_slice() {
        [value_id, value_input, value_output] if m::is(arena, mixop_meta, m::REL_PREM)? => {
            decode_rule_prem(arena, *value_id, *value_input, *value_output, span)?
        }
        [value_exp] if m::is(arena, mixop_meta, m::IF)? => decode_if_prem(arena, *value_exp)?,
        [value_id, value_exps] if m::is(arena, mixop_meta, m::IFHOLD)? => {
            decode_if_hold_prem(arena, *value_id, *value_exps, span)?
        }
        [value_id, value_exps] if m::is(arena, mixop_meta, m::IFNOTHOLD)? => {
            decode_if_not_hold_prem(arena, *value_id, *value_exps, span)?
        }
        [value_l, value_r] if m::is(arena, mixop_meta, m::LET)? => {
            decode_let_prem(arena, *value_l, *value_r)?
        }
        [value_prem, value_iter] if m::is(arena, mixop_meta, m::ITER_PREM)? => {
            decode_iter_prem(arena, *value_prem, *value_iter)?
        }
        [value_exp] if m::is(arena, mixop_meta, m::DEBUG)? => decode_debug_prem(arena, *value_exp)?,
        _ => return Err(Error::unexpected(arena, value_meta, "an AL premise")),
    };
    Ok(phrase!(node: prem_kind_al, span: span))
}

/// Rebuilds relation notation with canonical input positions.
fn decode_rule_prem(
    arena: &mut Arena,
    value_id: Value,
    value_input: Value,
    value_output: Value,
    span: Span,
) -> Result<al::PremKind, Error> {
    let (mixfix_exp_al, input_hint) = notation(
        <Vec<il::Exp> as Decode>::decode(arena, value_input)?,
        <Vec<il::Exp> as Decode>::decode(arena, value_output)?,
        span,
    )?;
    Ok(al::PremKind::Rule(al::RulePrem {
        id: <Id as Decode>::decode(arena, value_id)?,
        not_exp: mixfix_exp_al,
        input_hint,
    }))
}

fn decode_if_prem(arena: &mut Arena, value_exp: Value) -> Result<al::PremKind, Error> {
    Ok(al::PremKind::If(al::IfPrem { exp: <il::Exp as Decode>::decode(arena, value_exp)? }))
}

/// Rebuilds the relation test with all arguments as inputs.
fn decode_if_hold_prem(
    arena: &mut Arena,
    value_id: Value,
    value_exps: Value,
    span: Span,
) -> Result<al::PremKind, Error> {
    let (mixfix_exp_al, _) =
        notation(<Vec<il::Exp> as Decode>::decode(arena, value_exps)?, vec![], span)?;
    Ok(al::PremKind::IfHold(al::IfHoldPrem {
        id: <Id as Decode>::decode(arena, value_id)?,
        not_exp: mixfix_exp_al,
    }))
}

/// Rebuilds the negative relation test with all arguments as inputs.
fn decode_if_not_hold_prem(
    arena: &mut Arena,
    value_id: Value,
    value_exps: Value,
    span: Span,
) -> Result<al::PremKind, Error> {
    let (mixfix_exp_al, _) =
        notation(<Vec<il::Exp> as Decode>::decode(arena, value_exps)?, vec![], span)?;
    Ok(al::PremKind::IfNotHold(al::IfNotHoldPrem {
        id: <Id as Decode>::decode(arena, value_id)?,
        not_exp: mixfix_exp_al,
    }))
}

fn decode_let_prem(
    arena: &mut Arena,
    value_l: Value,
    value_r: Value,
) -> Result<al::PremKind, Error> {
    Ok(al::PremKind::Let(al::LetPrem {
        exp_l: <il::Exp as Decode>::decode(arena, value_l)?,
        exp_r: <il::Exp as Decode>::decode(arena, value_r)?,
    }))
}

fn decode_iter_prem(
    arena: &mut Arena,
    value_prem: Value,
    value_iter: Value,
) -> Result<al::PremKind, Error> {
    Ok(al::PremKind::Iter(al::IterPrem {
        prem: Box::new(prem(arena, value_prem)?),
        prem_iter: iterprem(arena, value_iter)?,
    }))
}

fn decode_debug_prem(arena: &mut Arena, value_exp: Value) -> Result<al::PremKind, Error> {
    Ok(al::PremKind::Debug(al::DebugPrem { exp: <il::Exp as Decode>::decode(arena, value_exp)? }))
}

/// Decodes bound and bound-by-iteration variable lists in order.
fn iterprem(arena: &mut Arena, value_meta: Value) -> Result<al::PremIter, Error> {
    let (mixop_meta, values_meta, _) = case(arena, value_meta)?;
    match values_meta.as_slice() {
        [value_iter, value_bound, value_bind] if m::is(arena, mixop_meta, m::ITERPREM)? => {
            decode_iterprem(arena, *value_iter, *value_bound, *value_bind)
        }
        _ => Err(Error::unexpected(arena, value_meta, "iter vari* vari*")),
    }
}

/// Reconstructs a premise iterator with both variable lists.
fn decode_iterprem(
    arena: &mut Arena,
    value_iter: Value,
    value_bound: Value,
    value_bind: Value,
) -> Result<al::PremIter, Error> {
    Ok(al::PremIter {
        iter: iter(arena, value_iter)?,
        vars_bound: list(arena, value_bound, <il::Var as Decode>::decode)?,
        vars_bind: list(arena, value_bind, <il::Var as Decode>::decode)?,
    })
}

/// Decodes a rule match without its printing signature.
fn rulematch(arena: &mut Arena, value_meta: Value) -> Result<al::RuleMatch, Error> {
    let (mixop_meta, values_meta, _) = case(arena, value_meta)?;
    match values_meta.as_slice() {
        [value_exps, value_prems] if m::is(arena, mixop_meta, m::RULMATCH)? => {
            decode_rulematch(arena, *value_exps, *value_prems)
        }
        _ => Err(Error::unexpected(arena, value_meta, "exp* '-' prem*")),
    }
}

/// Uses input patterns as the omitted rule printing signature.
fn decode_rulematch(
    arena: &mut Arena,
    value_exps: Value,
    value_prems: Value,
) -> Result<al::RuleMatch, Error> {
    let exps_input_al = <Vec<il::Exp> as Decode>::decode(arena, value_exps)?;
    Ok(al::RuleMatch {
        exps_signature: exps_input_al.clone(),
        exps_input: exps_input_al,
        prems: list(arena, value_prems, prem)?,
    })
}

/// Decodes a rule path and its outputs.
fn rulepath(arena: &mut Arena, value_meta: Value) -> Result<al::RulePath, Error> {
    let (mixop_meta, values_meta, _) = case(arena, value_meta)?;
    match values_meta.as_slice() {
        [value_id, value_exps, value_prems] if m::is(arena, mixop_meta, m::RULPATH)? => {
            decode_rulepath(arena, *value_id, *value_exps, *value_prems)
        }
        _ => Err(Error::unexpected(arena, value_meta, "id '=' exp* '-' prem*")),
    }
}

/// Reconstructs a rule path with its outputs and premises.
fn decode_rulepath(
    arena: &mut Arena,
    value_id: Value,
    value_exps: Value,
    value_prems: Value,
) -> Result<al::RulePath, Error> {
    Ok(al::RulePath {
        id: <Id as Decode>::decode(arena, value_id)?,
        exps_output: <Vec<il::Exp> as Decode>::decode(arena, value_exps)?,
        prems: list(arena, value_prems, prem)?,
    })
}

/// Decodes a rule group with its shared match.
fn rulegroup(arena: &mut Arena, value_meta: Value) -> Result<al::RuleGroup, Error> {
    let (mixop_meta, values_meta, span) = case(arena, value_meta)?;
    match values_meta.as_slice() {
        [value_id, value_match, value_paths] if m::is(arena, mixop_meta, m::RULGROUP)? => {
            decode_rulegroup(arena, *value_id, *value_match, *value_paths, span)
        }
        _ => Err(Error::unexpected(arena, value_meta, "id ':' rulmatch '=' rulpath*")),
    }
}

/// Reconstructs a rule group with its shared match and paths.
fn decode_rulegroup(
    arena: &mut Arena,
    value_id: Value,
    value_match: Value,
    value_paths: Value,
    span: Span,
) -> Result<al::RuleGroup, Error> {
    Ok(phrase!(node: al::RuleGroupKind {
        id: <Id as Decode>::decode(arena, value_id)?,
        rule_match: rulematch(arena, value_match)?,
        rule_paths: list(arena, value_paths, rulepath)?,
    }, span: span))
}

/// Decodes an otherwise group with one rule path.
fn elsegroup(arena: &mut Arena, value_meta: Value) -> Result<al::ElseGroup, Error> {
    let (mixop_meta, values_meta, span) = case(arena, value_meta)?;
    match values_meta.as_slice() {
        [value_id, value_match, value_path] if m::is(arena, mixop_meta, m::ELSGROUP)? => {
            decode_elsegroup(arena, *value_id, *value_match, *value_path, span)
        }
        _ => Err(Error::unexpected(arena, value_meta, "id ':' rulmatch '=' rulpath")),
    }
}

/// Reconstructs an otherwise group with its single path.
fn decode_elsegroup(
    arena: &mut Arena,
    value_id: Value,
    value_match: Value,
    value_path: Value,
    span: Span,
) -> Result<al::ElseGroup, Error> {
    Ok(phrase!(node: al::ElseGroupKind {
        id: <Id as Decode>::decode(arena, value_id)?,
        rule_match: rulematch(arena, value_match)?,
        rule_path: rulepath(arena, value_path)?,
    }, span: span))
}

/// Decodes a function clause.
fn clause(arena: &mut Arena, value_meta: Value) -> Result<al::Clause, Error> {
    let (mixop_meta, values_meta, span) = case(arena, value_meta)?;
    match values_meta.as_slice() {
        [value_args, value_exp, value_prems] if m::is(arena, mixop_meta, m::CLAUSE)? => {
            decode_clause(arena, *value_args, *value_exp, *value_prems, span)
        }
        _ => Err(Error::unexpected(arena, value_meta, "arg* '=' exp '-' prem*")),
    }
}

/// Reconstructs a function clause and retains its source span.
fn decode_clause(
    arena: &mut Arena,
    value_args: Value,
    value_exp: Value,
    value_prems: Value,
    span: Span,
) -> Result<al::Clause, Error> {
    Ok(phrase!(node: al::ClauseKind {
        args: args(arena, value_args)?,
        exp: <il::Exp as Decode>::decode(arena, value_exp)?,
        prems: list(arena, value_prems, prem)?,
    }, span: span))
}

/// Decodes a table row without its printing signature.
fn tablerow(arena: &mut Arena, value_meta: Value) -> Result<al::TableRow, Error> {
    let clause_al = clause(arena, value_meta)?;
    let al::ClauseKind { args: args_al, exp: exp_body_al, prems: prems_al } = clause_al.node;
    Ok(phrase!(node: al::TableRowKind { exps_signature: vec![],
            args: args_al,
            exp: exp_body_al,
            prems: prems_al, }, span: clause_al.span))
}
