//! Encodes source AL syntax and runtime values as meta data
//!
//! `Encode` implementations borrow the caller's arena without resetting it
//! Constructor matches dispatch to helpers after encoding their children
//! Scripts omit variable declarations and non-semantic metadata

use super::{Error, mixops as m};
use crate::lang::common::prim::num::Number;
use crate::{
    lang::{
        al::ast as al,
        common::{
            Id, Iter,
            prim::{bool as boolean, num},
            source::Span,
        },
        data::{
            arena::Arena,
            notation::tree::{Mixfix, Mixop, parse},
            typ::make as make_typ,
            value::{
                ValueError,
                flat::{Value, ValueField, ValueKind, make},
            },
        },
        hints::input,
        il::ast as il,
        traits::print::Print,
    },
    phrase,
};

/// Converts source AL data into its meta representation.
pub trait Encode {
    /// The encoded value, or an omitted source declaration.
    type Output;

    /// Encodes this data in the supplied arena.
    fn encode(&self, arena: &mut Arena) -> Result<Self::Output, Error>;
}

// == Common constructors

/// Adapts encoding to the arena-first list mapper.
fn encode_ref<T: Encode + ?Sized>(arena: &mut Arena, source: &T) -> Result<T::Output, Error> {
    source.encode(arena)
}

/// Encodes an identifier as text, retaining its span.
impl Encode for Id {
    type Output = Value;

    fn encode(&self, arena: &mut Arena) -> Result<Self::Output, Error> {
        let id_al = self;
        Ok(make::text(arena, id_al.node.to_string(), id_al.span)?)
    }
}

/// Encodes a notation atom by its source spelling.
fn atom(arena: &mut Arena, atom_al: &il::Atom) -> Result<Value, Error> {
    Ok(make::text(arena, atom_al.to_string(), atom_al.span)?)
}

/// Constructs a typed meta case after its children have been encoded.
fn case(
    arena: &mut Arena,
    shape: &str,
    name: &str,
    values_meta: Vec<Value>,
    span: Span,
) -> Result<Value, Error> {
    let mixfix = Mixfix::new(parse::mixop(shape), values_meta)?;
    Ok(make::case(
        arena,
        make_typ::var(phrase!(node: name.into(), span: Span::default()), vec![])
            .node
            .into(),
        mixfix,
        span,
    )?)
}

/// Maps a source list and annotates its meta element type.
fn list<T>(
    arena: &mut Arena,
    name: &str,
    source: &[T],
    mut func_encode: impl FnMut(&mut Arena, &T) -> Result<Value, Error>,
) -> Result<Value, Error> {
    let values_meta = source
        .iter()
        .map(|value_source| func_encode(arena, value_source))
        .collect::<Result<_, _>>()?;
    Ok(make::list(
        arena,
        make_typ::iter(
            make_typ::var(phrase!(node: name.into(), span: Span::default()), vec![]),
            Iter::List,
        )
        .node
        .into(),
        values_meta,
        Span::default(),
    )?)
}

/// Maps an optional source value and retains its meta element type.
fn opt<T>(
    arena: &mut Arena,
    name: &str,
    source: Option<&T>,
    func_encode: impl FnOnce(&mut Arena, &T) -> Result<Value, Error>,
) -> Result<Value, Error> {
    let value_meta = source
        .map(|value_source| func_encode(arena, value_source))
        .transpose()?;
    Ok(make::opt(
        arena,
        make_typ::iter(
            make_typ::var(phrase!(node: name.into(), span: Span::default()), vec![]),
            Iter::Opt,
        )
        .node
        .into(),
        value_meta,
        Span::default(),
    )?)
}

/// Encodes a source iterator.
fn iter(arena: &mut Arena, iter_al: &Iter) -> Result<Value, Error> {
    let shape = match iter_al {
        Iter::Opt => m::QUEST,
        Iter::List => m::STAR,
    };
    case(arena, shape, "iter", vec![], Span::default())
}

/// Encodes a source variable with its underlying type and dimensions.
impl Encode for il::Var {
    type Output = Value;

    fn encode(&self, arena: &mut Arena) -> Result<Self::Output, Error> {
        let var_al = self;
        let value_id = var_al.id.encode(arena)?;
        let value_typ = var_al.typ.encode(arena)?;
        let value_iters = list(arena, "iter", &var_al.iters, iter)?;
        case(arena, m::VARI, "vari", vec![value_id, value_typ, value_iters], var_al.id.span)
    }
}

/// Encodes atoms between argument positions as a nonempty matrix.
fn mixop(arena: &mut Arena, mixop_al: &Mixop) -> Result<Value, Error> {
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
    atoms_tail_al: &mut Vec<&'a il::Atom>,
    atoms_rows_al: &mut Vec<Vec<&'a il::Atom>>,
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
    atom_l_al: &'a il::Atom,
    mixop_inner_al: &'a Mixop,
    atom_r_al: &'a il::Atom,
    atoms_tail_al: &mut Vec<&'a il::Atom>,
    atoms_rows_al: &mut Vec<Vec<&'a il::Atom>>,
) {
    atoms_tail_al.push(atom_l_al);
    split_mixop(mixop_inner_al, atoms_tail_al, atoms_rows_al);
    atoms_tail_al.push(atom_r_al);
}

/// Collects infix notation in source order around its argument holes.
fn split_infix_mixop<'a>(
    mixop_l_al: &'a Mixop,
    atom_al: &'a il::Atom,
    mixop_r_al: &'a Mixop,
    atoms_tail_al: &mut Vec<&'a il::Atom>,
    atoms_rows_al: &mut Vec<Vec<&'a il::Atom>>,
) {
    split_mixop(mixop_l_al, atoms_tail_al, atoms_rows_al);
    atoms_tail_al.push(atom_al);
    split_mixop(mixop_r_al, atoms_tail_al, atoms_rows_al);
}

fn split_seq_mixop<'a>(
    mixops_al: &'a [Mixop],
    atoms_tail_al: &mut Vec<&'a il::Atom>,
    atoms_rows_al: &mut Vec<Vec<&'a il::Atom>>,
) {
    for mixop_al in mixops_al {
        split_mixop(mixop_al, atoms_tail_al, atoms_rows_al);
    }
}

/// Encodes a successful function result as `OK val : valres`.
pub fn ok_value(arena: &mut Arena, value_target: Value) -> Result<Value, Error> {
    let span = *arena.value.span(value_target.span);
    let value_meta = value_target.encode(arena)?;
    case(arena, m::OK, "valres", vec![value_meta], span)
}

/// Encodes successful relation outputs as `OK val* : valsres`.
pub fn ok_values(arena: &mut Arena, values_target: &[Value]) -> Result<Value, Error> {
    let values_meta = values_target.encode(arena)?;
    case(arena, m::OK, "valsres", vec![values_meta], Span::default())
}

// == Types

/// Encodes a source type, including complete function signatures.
impl Encode for il::Typ {
    type Output = Value;

    fn encode(&self, arena: &mut Arena) -> Result<Self::Output, Error> {
        let typ_al = self;
        let span = typ_al.span;
        match &typ_al.node {
            il::TypKind::Bool => {
                case(arena, m::BOOL_TYP, "typ", vec![], span).map_err(|error| error.at(span))
            }
            il::TypKind::Num(num::Typ::Nat) => {
                case(arena, m::NAT_TYP, "typ", vec![], span).map_err(|error| error.at(span))
            }
            il::TypKind::Num(num::Typ::Int) => {
                case(arena, m::INT_TYP, "typ", vec![], span).map_err(|error| error.at(span))
            }
            il::TypKind::Text => {
                case(arena, m::TEXT_TYP, "typ", vec![], span).map_err(|error| error.at(span))
            }
            il::TypKind::Var(id_al, targs_al) => encode_var_typ(arena, id_al, targs_al, span),
            il::TypKind::Tuple(typs_al) => encode_tuple_typ(arena, typs_al, span),
            il::TypKind::Iter(typ_inner_al, iter_al) => {
                encode_iter_typ(arena, typ_inner_al, iter_al, span)
            }
            il::TypKind::Func(typ_func_al) => encode_func_typ(arena, typ_func_al, span),
        }
    }
}

/// Encodes a named type and its arguments.
fn encode_var_typ(
    arena: &mut Arena,
    id_al: &il::Id,
    targs_al: &[il::Typ],
    span: Span,
) -> Result<Value, Error> {
    let values_meta = vec![id_al.encode(arena)?, targs_al.encode(arena)?];
    case(arena, m::VAR_TYP, "typ", values_meta, span).map_err(|error| error.at(span))
}

/// Encodes the component types of a tuple.
fn encode_tuple_typ(arena: &mut Arena, typs_al: &[il::Typ], span: Span) -> Result<Value, Error> {
    let value_typs = typs_al.encode(arena)?;
    case(arena, m::TUP_TYP, "typ", vec![value_typs], span).map_err(|error| error.at(span))
}

/// Encodes an iterated type and its dimension.
fn encode_iter_typ(
    arena: &mut Arena,
    typ_inner_al: &il::Typ,
    iter_al: &crate::lang::common::Iter,
    span: Span,
) -> Result<Value, Error> {
    let values_meta = vec![typ_inner_al.encode(arena)?, iter(arena, iter_al)?];
    case(arena, m::ITER_TYP, "typ", values_meta, span).map_err(|error| error.at(span))
}

/// Encodes a complete function type.
fn encode_func_typ(
    arena: &mut Arena,
    typ_func_al: &il::FuncTyp,
    span: Span,
) -> Result<Value, Error> {
    let values_meta = vec![
        list(arena, "id", &typ_func_al.tparams, encode_ref)?,
        typ_func_al.typs_params.encode(arena)?,
        typ_func_al.typ_ret.encode(arena)?,
    ];
    case(arena, m::FUNC_TYP, "typ", values_meta, span).map_err(|error| error.at(span))
}

/// Encodes a list of source types.
impl Encode for [il::Typ] {
    type Output = Value;

    fn encode(&self, arena: &mut Arena) -> Result<Self::Output, Error> {
        let typs_al = self;
        list(arena, "typ", typs_al, encode_ref)
    }
}

/// Encodes an alias, struct, or variant definition.
impl Encode for il::DefTyp {
    type Output = Value;

    fn encode(&self, arena: &mut Arena) -> Result<Self::Output, Error> {
        let deftyp_al = self;
        let (shape, values_meta) = match &deftyp_al.node {
            il::DefTypKind::Plain(typ_al) => (m::ALIAS, vec![typ_al.encode(arena)?]),
            il::DefTypKind::Struct(typ_fields_al) => {
                (m::STRUCT, vec![list(arena, "typfield", typ_fields_al, typfield)?])
            }
            il::DefTypKind::Variant(typ_cases_al) => {
                (m::VARIANT, vec![list(arena, "typcase", typ_cases_al, typcase)?])
            }
        };
        case(arena, shape, "deftyp", values_meta, deftyp_al.span)
    }
}

/// Encodes one struct type field.
fn typfield(arena: &mut Arena, typ_field_al: &il::TypField) -> Result<Value, Error> {
    let values_meta = vec![atom(arena, &typ_field_al.atom)?, typ_field_al.typ.encode(arena)?];
    case(arena, m::TYPFIELD, "typfield", values_meta, typ_field_al.atom.span)
}

/// Encodes one variant case without its origin or hints.
fn typcase(arena: &mut Arena, typ_case_al: &il::TypCase) -> Result<Value, Error> {
    let values_meta = vec![
        mixop(arena, typ_case_al.not_typ.node.mixop())?,
        (typ_case_al.not_typ.node.args()).encode(arena)?,
    ];
    case(arena, m::TYPCASE, "typcase", values_meta, typ_case_al.not_typ.span)
}

// == Values

/// Encodes a runtime value's payload in the same arena.
impl Encode for Value {
    type Output = Value;

    fn encode(&self, arena: &mut Arena) -> Result<Self::Output, Error> {
        let value_target = *self;
        let span = *arena.value.span(value_target.span);
        match arena.value.kind(value_target.node) {
            ValueKind::Bool(value_bool) => {
                let value_bool = *value_bool;
                encode_bool_value(arena, value_bool, span)
            }
            ValueKind::Num(num) => {
                let num = num.clone();
                encode_num_value(arena, num, span)
            }
            ValueKind::Text(text) => {
                let text = text.clone();
                encode_text_value(arena, text, span)
            }
            ValueKind::Struct(value_fields) => {
                let value_fields = value_fields.clone();
                encode_struct_value(arena, &value_fields, span)
            }
            ValueKind::Case(value_case) => {
                let mixop_target = value_case.mixop().into_tree(&arena.mixop);
                let values_target = value_case.args().to_vec();
                encode_case_value(arena, &mixop_target, &values_target, span)
            }
            ValueKind::Tuple(values_target) => {
                let values_target = values_target.clone();
                encode_tuple_value(arena, &values_target, span)
            }
            ValueKind::Opt(value_inner) => {
                let value_inner = *value_inner;
                encode_opt_value(arena, value_inner, span)
            }
            ValueKind::List(values_target) => {
                let values_target = values_target.clone();
                encode_list_value(arena, &values_target, span)
            }
            ValueKind::Func(id_target) => {
                let id_target = id_target.clone();
                encode_func_value(arena, &id_target, span)
            }
            ValueKind::Extern(json) => {
                let json = json.clone();
                encode_extern_value(arena, json, span)
            }
        }
    }
}

/// Encodes a boolean payload.
fn encode_bool_value(arena: &mut Arena, value_bool: bool, span: Span) -> Result<Value, Error> {
    let values_meta = vec![make::bool(arena, value_bool, span)?];
    case(arena, m::BOOL, "val", values_meta, span).map_err(|error| error.at(span))
}

/// Encodes a numeric payload with its natural or integer constructor.
fn encode_num_value(arena: &mut Arena, num: Number, span: Span) -> Result<Value, Error> {
    let shape = match &num {
        Number::Nat(_) => m::NAT,
        Number::Int(_) => m::INT,
    };
    let values_meta = vec![make::num(arena, num, span)?];
    case(arena, shape, "val", values_meta, span).map_err(|error| error.at(span))
}

/// Encodes a text payload.
fn encode_text_value(arena: &mut Arena, text: String, span: Span) -> Result<Value, Error> {
    let values_meta = vec![make::text(arena, text, span)?];
    case(arena, m::TEXT, "val", values_meta, span).map_err(|error| error.at(span))
}

/// Encodes the fields of a runtime struct.
fn encode_struct_value(
    arena: &mut Arena,
    value_fields: &[ValueField],
    span: Span,
) -> Result<Value, Error> {
    let values_meta = vec![list(arena, "valfield", value_fields, valuefield)?];
    case(arena, m::STR, "val", values_meta, span).map_err(|error| error.at(span))
}

/// Encodes a case notation together with its runtime arguments.
fn encode_case_value(
    arena: &mut Arena,
    mixop_target: &Mixop,
    values_target: &[Value],
    span: Span,
) -> Result<Value, Error> {
    let values_case = vec![mixop(arena, mixop_target)?, values_target.encode(arena)?];
    let values_meta = vec![case(arena, m::VALCASE, "valcase", values_case, span)?];
    case(arena, m::INJ, "val", values_meta, span).map_err(|error| error.at(span))
}

/// Encodes the elements of a runtime tuple.
fn encode_tuple_value(
    arena: &mut Arena,
    values_target: &[Value],
    span: Span,
) -> Result<Value, Error> {
    let values_meta = vec![values_target.encode(arena)?];
    case(arena, m::TUP, "val", values_meta, span).map_err(|error| error.at(span))
}

/// Encodes an optional runtime value.
fn encode_opt_value(
    arena: &mut Arena,
    value_inner: Option<Value>,
    span: Span,
) -> Result<Value, Error> {
    let values_meta = vec![opt(arena, "val", value_inner.as_ref(), |arena, value_inner| {
        (*value_inner).encode(arena)
    })?];
    case(arena, m::OPT, "val", values_meta, span).map_err(|error| error.at(span))
}

/// Encodes the elements of a runtime list.
fn encode_list_value(
    arena: &mut Arena,
    values_target: &[Value],
    span: Span,
) -> Result<Value, Error> {
    let values_meta = vec![values_target.encode(arena)?];
    case(arena, m::LIST, "val", values_meta, span).map_err(|error| error.at(span))
}

/// Encodes a function identifier without its native signature.
fn encode_func_value(arena: &mut Arena, id_target: &Id, span: Span) -> Result<Value, Error> {
    let values_meta = vec![id_target.encode(arena)?];
    case(arena, m::FUNC, "val", values_meta, span).map_err(|error| error.at(span))
}

/// Encodes an external JSON payload.
fn encode_extern_value(
    arena: &mut Arena,
    json: std::rc::Rc<crate::util::json::json>,
    span: Span,
) -> Result<Value, Error> {
    let values_meta = vec![make::external(
        arena,
        make_typ::var(phrase!(node: "json".into(), span: Span::default()), vec![])
            .node
            .into(),
        json,
        span,
    )?];
    case(arena, m::EXT, "val", values_meta, span).map_err(|error| error.at(span))
}

/// Encodes target values as a meta `val*`.
impl Encode for [Value] {
    type Output = Value;

    fn encode(&self, arena: &mut Arena) -> Result<Self::Output, Error> {
        let values_target = self;
        list(arena, "val", values_target, |arena, value_target| (*value_target).encode(arena))
    }
}

/// Encodes one runtime struct field.
fn valuefield(arena: &mut Arena, value_field: &ValueField) -> Result<Value, Error> {
    let values_meta = vec![atom(arena, &value_field.atom)?, value_field.value.encode(arena)?];
    case(arena, m::VALFIELD, "valfield", values_meta, value_field.atom.span)
}

// == Expressions and paths

/// Encodes every source expression form without native optimization data.
impl Encode for il::Exp {
    type Output = Value;

    fn encode(&self, arena: &mut Arena) -> Result<Self::Output, Error> {
        let exp_al = self;
        let span = exp_al.span;
        match &exp_al.node {
            il::ExpKind::Bool(value_bool) => encode_bool_exp(arena, *value_bool, span),
            il::ExpKind::Num(num) => encode_num_exp(arena, num, span),
            il::ExpKind::Text(text) => encode_text_exp(arena, text, span),
            il::ExpKind::Id(id_al) => encode_id_exp(arena, id_al, span),
            il::ExpKind::Un(op_al, _, exp_inner_al) => {
                encode_un_exp(arena, *op_al, exp_inner_al, span)
            }
            il::ExpKind::Bin(op_al, _, exp_l_al, exp_r_al) => {
                encode_bin_exp(arena, *op_al, exp_l_al, exp_r_al, span)
            }
            il::ExpKind::Cmp(op_al, _, exp_l_al, exp_r_al) => {
                encode_cmp_exp(arena, *op_al, exp_l_al, exp_r_al, span)
            }
            il::ExpKind::UpCast(typ_al, exp_inner_al) => {
                encode_upcast_exp(arena, typ_al, exp_inner_al, span)
            }
            il::ExpKind::DownCast(typ_al, exp_inner_al) => {
                encode_downcast_exp(arena, typ_al, exp_inner_al, span)
            }
            il::ExpKind::Sub(exp_inner_al, typ_al, _) => {
                encode_sub_exp(arena, exp_inner_al, typ_al, span)
            }
            il::ExpKind::Match(exp_inner_al, pattern_al) => {
                encode_match_exp(arena, exp_inner_al, pattern_al, span)
            }
            il::ExpKind::Tuple(exps_al) => encode_tuple_exp(arena, exps_al, span),
            il::ExpKind::Case(exp_case_al) => encode_case_exp(arena, exp_case_al, span),
            il::ExpKind::Str(exp_fields_al) => encode_str_exp(arena, exp_fields_al, span),
            il::ExpKind::Opt(exp_inner_al) => encode_opt_exp(arena, exp_inner_al.as_deref(), span),
            il::ExpKind::List(exps_al) => encode_list_exp(arena, exps_al, span),
            il::ExpKind::Cons(exp_head_al, exp_tail_al) => {
                encode_cons_exp(arena, exp_head_al, exp_tail_al, span)
            }
            il::ExpKind::Cat(exp_l_al, exp_r_al) => encode_cat_exp(arena, exp_l_al, exp_r_al, span),
            il::ExpKind::Mem(exp_elem_al, exp_list_al) => {
                encode_mem_exp(arena, exp_elem_al, exp_list_al, span)
            }
            il::ExpKind::Len(exp_inner_al) => encode_len_exp(arena, exp_inner_al, span),
            il::ExpKind::Dot(exp_base_al, atom_al) => {
                encode_dot_exp(arena, exp_base_al, atom_al, span)
            }
            il::ExpKind::Idx(exp_base_al, exp_idx_al) => {
                encode_idx_exp(arena, exp_base_al, exp_idx_al, span)
            }
            il::ExpKind::Slice(exp_base_al, exp_idx_al, exp_len_al) => {
                encode_slice_exp(arena, exp_base_al, exp_idx_al, exp_len_al, span)
            }
            il::ExpKind::Upd(exp_base_al, path_al, exp_upd_al) => {
                encode_upd_exp(arena, exp_base_al, path_al, exp_upd_al, span)
            }
            il::ExpKind::Call(id_al, targs_al, args_al) => {
                encode_call_exp(arena, id_al, targs_al, args_al, span)
            }
            il::ExpKind::Iter(exp_inner_al, exp_iter_al) => {
                encode_iter_exp(arena, exp_inner_al, exp_iter_al, span)
            }
        }
    }
}

/// Encodes a boolean literal.
fn encode_bool_exp(arena: &mut Arena, value_bool: bool, span: Span) -> Result<Value, Error> {
    let (shape, values_meta) = (m::BOOL, vec![make::bool(arena, value_bool, span)?]);
    case(arena, shape, "exp", values_meta, span)
}

/// Encodes a numeric literal with its natural or integer constructor.
fn encode_num_exp(arena: &mut Arena, num: &num::Number, span: Span) -> Result<Value, Error> {
    let (shape, values_meta) = {
        let shape = match num {
            num::Number::Nat(_) => m::NAT,
            num::Number::Int(_) => m::INT,
        };
        (shape, vec![make::num(arena, num.clone(), span)?])
    };
    case(arena, shape, "exp", values_meta, span)
}

/// Encodes a text literal.
fn encode_text_exp(arena: &mut Arena, text: &str, span: Span) -> Result<Value, Error> {
    let (shape, values_meta) = (m::TEXT, vec![make::text(arena, text.into(), span)?]);
    case(arena, shape, "exp", values_meta, span)
}

/// Encodes a variable identifier.
fn encode_id_exp(arena: &mut Arena, id_al: &il::Id, span: Span) -> Result<Value, Error> {
    let (shape, values_meta) = (m::VAR_EXP, vec![id_al.encode(arena)?]);
    case(arena, shape, "exp", values_meta, span)
}

/// Encodes a unary operator and its operand.
fn encode_un_exp(
    arena: &mut Arena,
    op_al: il::UnOp,
    exp_inner_al: &il::Exp,
    span: Span,
) -> Result<Value, Error> {
    let (shape, values_meta) = (m::UN, vec![unop(arena, op_al)?, exp_inner_al.encode(arena)?]);
    case(arena, shape, "exp", values_meta, span)
}

/// Encodes a binary operator and its operands.
fn encode_bin_exp(
    arena: &mut Arena,
    op_al: il::BinOp,
    exp_l_al: &il::Exp,
    exp_r_al: &il::Exp,
    span: Span,
) -> Result<Value, Error> {
    let (shape, values_meta) =
        (m::BIN, vec![binop(arena, op_al)?, exp_l_al.encode(arena)?, exp_r_al.encode(arena)?]);
    case(arena, shape, "exp", values_meta, span)
}

/// Encodes a comparison operator and its operands.
fn encode_cmp_exp(
    arena: &mut Arena,
    op_al: il::CmpOp,
    exp_l_al: &il::Exp,
    exp_r_al: &il::Exp,
    span: Span,
) -> Result<Value, Error> {
    let (shape, values_meta) =
        (m::CMP, vec![cmpop(arena, op_al)?, exp_l_al.encode(arena)?, exp_r_al.encode(arena)?]);
    case(arena, shape, "exp", values_meta, span)
}

/// Encodes an upward cast and its target type.
fn encode_upcast_exp(
    arena: &mut Arena,
    typ_al: &il::Typ,
    exp_inner_al: &il::Exp,
    span: Span,
) -> Result<Value, Error> {
    let (shape, values_meta) =
        (m::UPCAST, vec![typ_al.encode(arena)?, exp_inner_al.encode(arena)?]);
    case(arena, shape, "exp", values_meta, span)
}

/// Encodes a downward cast and its target type.
fn encode_downcast_exp(
    arena: &mut Arena,
    typ_al: &il::Typ,
    exp_inner_al: &il::Exp,
    span: Span,
) -> Result<Value, Error> {
    let (shape, values_meta) =
        (m::DOWNCAST, vec![typ_al.encode(arena)?, exp_inner_al.encode(arena)?]);
    case(arena, shape, "exp", values_meta, span)
}

/// Encodes a subtype test without its native check plan.
fn encode_sub_exp(
    arena: &mut Arena,
    exp_inner_al: &il::Exp,
    typ_al: &il::Typ,
    span: Span,
) -> Result<Value, Error> {
    let (shape, values_meta) =
        (m::SUB_EXP, vec![exp_inner_al.encode(arena)?, typ_al.encode(arena)?]);
    case(arena, shape, "exp", values_meta, span)
}

/// Encodes a shape test and its pattern.
fn encode_match_exp(
    arena: &mut Arena,
    exp_inner_al: &il::Exp,
    pattern_al: &il::Pattern,
    span: Span,
) -> Result<Value, Error> {
    let (shape, values_meta) =
        (m::MATCH, vec![exp_inner_al.encode(arena)?, pattern(arena, pattern_al)?]);
    case(arena, shape, "exp", values_meta, span)
}

/// Encodes the expressions in a tuple.
fn encode_tuple_exp(arena: &mut Arena, exps_al: &[il::Exp], span: Span) -> Result<Value, Error> {
    let (shape, values_meta) = (m::TUP, vec![exps_al.encode(arena)?]);
    case(arena, shape, "exp", values_meta, span)
}

/// Encodes a case notation together with its expression arguments.
fn encode_case_exp(
    arena: &mut Arena,
    exp_case_al: &il::NotExp,
    span: Span,
) -> Result<Value, Error> {
    let (shape, values_meta) = {
        let values_meta =
            vec![mixop(arena, exp_case_al.mixop())?, (exp_case_al.args()).encode(arena)?];
        (m::INJ, vec![case(arena, m::EXPCASE, "expcase", values_meta, span)?])
    };
    case(arena, shape, "exp", values_meta, span)
}

/// Encodes the fields of a struct expression.
fn encode_str_exp(
    arena: &mut Arena,
    exp_fields_al: &[il::ExpField],
    span: Span,
) -> Result<Value, Error> {
    let (shape, values_meta) = (m::STR, vec![list(arena, "expfield", exp_fields_al, expfield)?]);
    case(arena, shape, "exp", values_meta, span)
}

/// Encodes an optional expression.
fn encode_opt_exp(
    arena: &mut Arena,
    exp_inner_al: Option<&il::Exp>,
    span: Span,
) -> Result<Value, Error> {
    let (shape, values_meta) = (m::OPT, vec![opt(arena, "exp", exp_inner_al, encode_ref)?]);
    case(arena, shape, "exp", values_meta, span)
}

/// Encodes the expressions in a list.
fn encode_list_exp(arena: &mut Arena, exps_al: &[il::Exp], span: Span) -> Result<Value, Error> {
    let (shape, values_meta) = (m::LIST, vec![exps_al.encode(arena)?]);
    case(arena, shape, "exp", values_meta, span)
}

/// Encodes a list head and tail.
fn encode_cons_exp(
    arena: &mut Arena,
    exp_head_al: &il::Exp,
    exp_tail_al: &il::Exp,
    span: Span,
) -> Result<Value, Error> {
    let (shape, values_meta) =
        (m::CONS, vec![exp_head_al.encode(arena)?, exp_tail_al.encode(arena)?]);
    case(arena, shape, "exp", values_meta, span)
}

/// Encodes the operands of a concatenation.
fn encode_cat_exp(
    arena: &mut Arena,
    exp_l_al: &il::Exp,
    exp_r_al: &il::Exp,
    span: Span,
) -> Result<Value, Error> {
    let (shape, values_meta) = (m::CAT, vec![exp_l_al.encode(arena)?, exp_r_al.encode(arena)?]);
    case(arena, shape, "exp", values_meta, span)
}

/// Encodes a list membership test.
fn encode_mem_exp(
    arena: &mut Arena,
    exp_elem_al: &il::Exp,
    exp_list_al: &il::Exp,
    span: Span,
) -> Result<Value, Error> {
    let (shape, values_meta) =
        (m::MEM, vec![exp_elem_al.encode(arena)?, exp_list_al.encode(arena)?]);
    case(arena, shape, "exp", values_meta, span)
}

/// Encodes the operand of a length expression.
fn encode_len_exp(arena: &mut Arena, exp_inner_al: &il::Exp, span: Span) -> Result<Value, Error> {
    let (shape, values_meta) = (m::LEN, vec![exp_inner_al.encode(arena)?]);
    case(arena, shape, "exp", values_meta, span)
}

/// Encodes a field access and its base expression.
fn encode_dot_exp(
    arena: &mut Arena,
    exp_base_al: &il::Exp,
    atom_al: &il::Atom,
    span: Span,
) -> Result<Value, Error> {
    let (shape, values_meta) = (m::DOT, vec![exp_base_al.encode(arena)?, atom(arena, atom_al)?]);
    case(arena, shape, "exp", values_meta, span)
}

/// Encodes an index access and its base expression.
fn encode_idx_exp(
    arena: &mut Arena,
    exp_base_al: &il::Exp,
    exp_idx_al: &il::Exp,
    span: Span,
) -> Result<Value, Error> {
    let (shape, values_meta) =
        (m::IDX, vec![exp_base_al.encode(arena)?, exp_idx_al.encode(arena)?]);
    case(arena, shape, "exp", values_meta, span)
}

/// Encodes a slice with its base, index, and length.
fn encode_slice_exp(
    arena: &mut Arena,
    exp_base_al: &il::Exp,
    exp_idx_al: &il::Exp,
    exp_len_al: &il::Exp,
    span: Span,
) -> Result<Value, Error> {
    let (shape, values_meta) = (
        m::SLICE,
        vec![exp_base_al.encode(arena)?, exp_idx_al.encode(arena)?, exp_len_al.encode(arena)?],
    );
    case(arena, shape, "exp", values_meta, span)
}

/// Encodes an update with its base, path, and replacement.
fn encode_upd_exp(
    arena: &mut Arena,
    exp_base_al: &il::Exp,
    path_al: &il::Path,
    exp_upd_al: &il::Exp,
    span: Span,
) -> Result<Value, Error> {
    let (shape, values_meta) = (
        m::UPD,
        vec![exp_base_al.encode(arena)?, path(arena, path_al)?, exp_upd_al.encode(arena)?],
    );
    case(arena, shape, "exp", values_meta, span)
}

/// Encodes a call with its type arguments and runtime arguments.
fn encode_call_exp(
    arena: &mut Arena,
    id_al: &il::Id,
    targs_al: &[il::Typ],
    args_al: &[il::Arg],
    span: Span,
) -> Result<Value, Error> {
    let (shape, values_meta) =
        (m::CALL, vec![id_al.encode(arena)?, targs_al.encode(arena)?, args(arena, args_al)?]);
    case(arena, shape, "exp", values_meta, span)
}

/// Encodes an iterated expression and its bound variables.
fn encode_iter_exp(
    arena: &mut Arena,
    exp_inner_al: &il::Exp,
    exp_iter_al: &il::ExpIter,
    span: Span,
) -> Result<Value, Error> {
    let (shape, values_meta) =
        (m::ITER_EXP, vec![exp_inner_al.encode(arena)?, iterexp(arena, exp_iter_al)?]);
    case(arena, shape, "exp", values_meta, span)
}

/// Encodes a list of source expressions.
impl Encode for [il::Exp] {
    type Output = Value;

    fn encode(&self, arena: &mut Arena) -> Result<Self::Output, Error> {
        let exps_al = self;
        list(arena, "exp", exps_al, encode_ref)
    }
}

/// Encodes one source struct-expression field.
fn expfield(arena: &mut Arena, exp_field_al: &il::ExpField) -> Result<Value, Error> {
    let values_meta = vec![atom(arena, &exp_field_al.atom)?, exp_field_al.exp.encode(arena)?];
    case(arena, m::EXPFIELD, "expfield", values_meta, exp_field_al.atom.span)
}

/// Encodes an expression iterator and its variables.
fn iterexp(arena: &mut Arena, exp_iter_al: &il::ExpIter) -> Result<Value, Error> {
    let values_meta =
        vec![iter(arena, &exp_iter_al.iter)?, list(arena, "vari", &exp_iter_al.vars, encode_ref)?];
    case(arena, m::ITEREXP, "iterexp", values_meta, Span::default())
}

/// Encodes a value or function argument.
fn arg(arena: &mut Arena, arg_al: &il::Arg) -> Result<Value, Error> {
    let (shape, value) = match &arg_al.node {
        il::ArgKind::Exp(exp_al) => (m::EXP_ARG, exp_al.encode(arena)?),
        il::ArgKind::Def(id_al) => (m::FUN_ARG, id_al.encode(arena)?),
    };
    case(arena, shape, "arg", vec![value], arg_al.span)
}

/// Encodes a source argument list.
fn args(arena: &mut Arena, args_al: &[il::Arg]) -> Result<Value, Error> {
    list(arena, "arg", args_al, arg)
}

/// Encodes a case, list, or option shape pattern.
fn pattern(arena: &mut Arena, pattern_al: &il::Pattern) -> Result<Value, Error> {
    let (shape, values_meta) = match pattern_al {
        il::Pattern::Case(mixop_al) => (m::CASE_PATTERN, vec![mixop(arena, mixop_al)?]),
        il::Pattern::List(il::ListPattern::Cons) => (m::CONS_PATTERN, vec![]),
        il::Pattern::List(il::ListPattern::Fixed(num_len)) => {
            return encode_fixed_pattern(arena, *num_len);
        }
        il::Pattern::List(il::ListPattern::Nil) => (m::NIL, vec![]),
        il::Pattern::Opt(il::OptPattern::Some) => (m::SOME, vec![]),
        il::Pattern::Opt(il::OptPattern::None) => (m::NONE, vec![]),
    };
    case(arena, shape, "pattern", values_meta, Span::default())
}

/// Encodes a source update path.
fn path(arena: &mut Arena, path_al: &il::Path) -> Result<Value, Error> {
    let span = path_al.span;
    match &path_al.node {
        il::PathKind::Root => case(arena, m::ROOT, "path", vec![], span),
        il::PathKind::Idx(path_base_al, exp_idx_al) => {
            encode_idx_path(arena, path_base_al, exp_idx_al, span)
        }
        il::PathKind::Slice(path_base_al, exp_idx_al, exp_len_al) => {
            encode_slice_path(arena, path_base_al, exp_idx_al, exp_len_al, span)
        }
        il::PathKind::Dot(path_base_al, atom_al) => {
            encode_dot_path(arena, path_base_al, atom_al, span)
        }
    }
}

/// Encodes an index step in an update path.
fn encode_idx_path(
    arena: &mut Arena,
    path_base_al: &il::Path,
    exp_idx_al: &il::Exp,
    span: Span,
) -> Result<Value, Error> {
    let values_meta = vec![path(arena, path_base_al)?, exp_idx_al.encode(arena)?];
    case(arena, m::IDX, "path", values_meta, span)
}

/// Encodes a slice step in an update path.
fn encode_slice_path(
    arena: &mut Arena,
    path_base_al: &il::Path,
    exp_idx_al: &il::Exp,
    exp_len_al: &il::Exp,
    span: Span,
) -> Result<Value, Error> {
    let values_meta =
        vec![path(arena, path_base_al)?, exp_idx_al.encode(arena)?, exp_len_al.encode(arena)?];
    case(arena, m::SLICE, "path", values_meta, span)
}

/// Encodes a field step in an update path.
fn encode_dot_path(
    arena: &mut Arena,
    path_base_al: &il::Path,
    atom_al: &il::Atom,
    span: Span,
) -> Result<Value, Error> {
    let values_meta = vec![path(arena, path_base_al)?, atom(arena, atom_al)?];
    case(arena, m::DOT, "path", values_meta, span)
}

/// Encodes a unary operator.
fn unop(arena: &mut Arena, op_al: il::UnOp) -> Result<Value, Error> {
    let shape = match op_al {
        il::UnOp::Bool(boolean::UnOp::Not) => m::NOT,
        il::UnOp::Num(num::UnOp::Plus) => m::PLUS,
        il::UnOp::Num(num::UnOp::Minus) => m::MINUS,
    };
    case(arena, shape, "unop", vec![], Span::default())
}

/// Encodes a binary operator.
fn binop(arena: &mut Arena, op_al: il::BinOp) -> Result<Value, Error> {
    let shape = match op_al {
        il::BinOp::Bool(boolean::BinOp::And) => m::AND,
        il::BinOp::Bool(boolean::BinOp::Or) => m::OR,
        il::BinOp::Bool(boolean::BinOp::Impl) => m::IMPL,
        il::BinOp::Bool(boolean::BinOp::Equiv) => m::EQUIV,
        il::BinOp::Num(num::BinOp::Add) => m::ADD,
        il::BinOp::Num(num::BinOp::Sub) => m::SUB,
        il::BinOp::Num(num::BinOp::Mul) => m::MUL,
        il::BinOp::Num(num::BinOp::Div) => m::DIV,
        il::BinOp::Num(num::BinOp::Mod) => m::MOD,
        il::BinOp::Num(num::BinOp::Pow) => m::POW,
    };
    case(arena, shape, "binop", vec![], Span::default())
}

/// Encodes a comparison operator.
fn cmpop(arena: &mut Arena, op_al: il::CmpOp) -> Result<Value, Error> {
    let shape = match op_al {
        il::CmpOp::Bool(boolean::CmpOp::Eq) => m::EQ,
        il::CmpOp::Bool(boolean::CmpOp::Ne) => m::NE,
        il::CmpOp::Num(num::CmpOp::Lt) => m::LT,
        il::CmpOp::Num(num::CmpOp::Le) => m::LE,
        il::CmpOp::Num(num::CmpOp::Gt) => m::GT,
        il::CmpOp::Num(num::CmpOp::Ge) => m::GE,
    };
    case(arena, shape, "cmpop", vec![], Span::default())
}

/// Encodes a fixed-length list pattern.
fn encode_fixed_pattern(arena: &mut Arena, num_len: usize) -> Result<Value, Error> {
    let nat = u64::try_from(num_len).map_err(ValueError::from)?.into();
    let value_nat = make::nat(arena, nat, Span::default())?;
    case(arena, m::FIXED, "pattern", vec![value_nat], Span::default())
}

// == Definitions and scripts

/// Encodes a whole source AL script, omitting only variable definitions.
impl Encode for al::Spec {
    type Output = Value;

    fn encode(&self, arena: &mut Arena) -> Result<Self::Output, Error> {
        let spec_al = self;
        // Preserve definition order while filtering the explicitly omitted form
        let mut values_meta = Vec::with_capacity(spec_al.len());
        for def_al in spec_al {
            if let Some(value) = def_al.encode(arena)? {
                values_meta.push(value);
            }
        }
        Ok(make::list(
            arena,
            make_typ::var(phrase!(node: "script".into(), span: Span::default()), vec![])
                .node
                .into(),
            values_meta,
            Span::default(),
        )?)
    }
}

/// Encodes a definition; only a Var definition returns None.
impl Encode for al::Def {
    type Output = Option<Value>;

    fn encode(&self, arena: &mut Arena) -> Result<Self::Output, Error> {
        let def_al = self;
        let span = def_al.span;
        let value_meta = match &def_al.node {
            al::DefKind::Var(_) => return Ok(None),
            al::DefKind::Typ(al::TypDef::Extern(typdef_al)) => {
                encode_extern_typ_def(arena, typdef_al, span)?
            }
            al::DefKind::Typ(al::TypDef::Defined(typdef_al)) => {
                encode_defined_typ_def(arena, typdef_al, span)?
            }
            al::DefKind::Rel(al::RelDef::Extern(rel_al)) => {
                encode_extern_rel_def(arena, rel_al, span)?
            }
            al::DefKind::Rel(al::RelDef::Defined(rel_al)) => {
                encode_defined_rel_def(arena, rel_al, span)?
            }
            al::DefKind::MetaFunc(al::MetaFuncDef::Extern(func_al)) => {
                encode_extern_func_def(arena, func_al, span)?
            }
            al::DefKind::MetaFunc(al::MetaFuncDef::Builtin(func_al)) => {
                encode_builtin_func_def(arena, func_al, span)?
            }
            al::DefKind::MetaFunc(al::MetaFuncDef::Table(func_al)) => {
                encode_table_func_def(arena, func_al, span)?
            }
            al::DefKind::MetaFunc(al::MetaFuncDef::Defined(func_al)) => {
                encode_defined_func_def(arena, func_al, span)?
            }
        };
        Ok(Some(value_meta))
    }
}

/// Encodes an external type declaration.
fn encode_extern_typ_def(
    arena: &mut Arena,
    typdef_al: &al::ExternTyp,
    span: Span,
) -> Result<Value, Error> {
    let (shape, values_meta) = (m::EXTTYP, vec![typdef_al.id.encode(arena)?]);
    case(arena, shape, "defn", values_meta, span)
}

/// Encodes a type definition with its parameters.
fn encode_defined_typ_def(
    arena: &mut Arena,
    typdef_al: &al::DefinedTyp,
    span: Span,
) -> Result<Value, Error> {
    let (shape, values_meta) = (
        m::TYP_DEF,
        vec![
            typdef_al.id.encode(arena)?,
            list(arena, "id", &typdef_al.tparams, encode_ref)?,
            typdef_al.def_typ.encode(arena)?,
        ],
    );
    case(arena, shape, "defn", values_meta, span)
}

/// Encodes an external relation signature.
fn encode_extern_rel_def(
    arena: &mut Arena,
    rel_al: &al::ExternRel,
    span: Span,
) -> Result<Value, Error> {
    let (shape, values_meta) = {
        // Split source arguments according to the operational input hint
        let (typs_input_al, typs_output_al) =
            input::split(&rel_al.input_hint, rel_al.not_typ.node.args().to_vec())?;
        // Encode input types before output types in the meta signature
        (
            m::EXTREL,
            vec![
                rel_al.id.encode(arena)?,
                typs_input_al.encode(arena)?,
                typs_output_al.encode(arena)?,
            ],
        )
    };
    // Retain the definition span on the enclosing meta constructor
    case(arena, shape, "defn", values_meta, span)
}

/// Encodes a relation signature and its rule groups.
fn encode_defined_rel_def(
    arena: &mut Arena,
    rel_al: &al::DefinedRel,
    span: Span,
) -> Result<Value, Error> {
    let (shape, values_meta) = {
        // Split source arguments according to the operational input hint
        let (typs_input_al, typs_output_al) =
            input::split(&rel_al.input_hint, rel_al.not_typ.node.args().to_vec())?;
        // Encode the canonical signature before the rule groups
        (
            m::REL_DEF,
            vec![
                rel_al.id.encode(arena)?,
                typs_input_al.encode(arena)?,
                typs_output_al.encode(arena)?,
                list(arena, "rulgroup", &rel_al.rule_groups, rulegroup)?,
                opt(arena, "elsgroup", rel_al.else_group.as_ref(), elsegroup)?,
            ],
        )
    };
    // Retain the definition span on the enclosing meta constructor
    case(arena, shape, "defn", values_meta, span)
}

/// Encodes an external function signature.
fn encode_extern_func_def(
    arena: &mut Arena,
    func_al: &al::ExternFunc,
    span: Span,
) -> Result<Value, Error> {
    let (shape, values_meta) = (
        m::EXTFUNC,
        vec![
            func_al.id.encode(arena)?,
            list(arena, "id", &func_al.tparams, encode_ref)?,
            list(arena, "param", &func_al.params, param)?,
            func_al.typ.encode(arena)?,
        ],
    );
    case(arena, shape, "defn", values_meta, span)
}

/// Encodes a builtin function signature.
fn encode_builtin_func_def(
    arena: &mut Arena,
    func_al: &al::BuiltinFunc,
    span: Span,
) -> Result<Value, Error> {
    let (shape, values_meta) = (
        m::BUILTINFUNC,
        vec![
            func_al.id.encode(arena)?,
            list(arena, "id", &func_al.tparams, encode_ref)?,
            list(arena, "param", &func_al.params, param)?,
            func_al.typ.encode(arena)?,
        ],
    );
    case(arena, shape, "defn", values_meta, span)
}

/// Encodes a table function and its rows.
fn encode_table_func_def(
    arena: &mut Arena,
    func_al: &al::TableFunc,
    span: Span,
) -> Result<Value, Error> {
    let (shape, values_meta) = (
        m::TABLEFUNC,
        vec![
            func_al.id.encode(arena)?,
            list(arena, "param", &func_al.params, param)?,
            func_al.typ.encode(arena)?,
            list(arena, "tblrow", &func_al.table_rows, tablerow)?,
        ],
    );
    case(arena, shape, "defn", values_meta, span)
}

/// Encodes a function signature and its clauses.
fn encode_defined_func_def(
    arena: &mut Arena,
    func_al: &al::DefinedFunc,
    span: Span,
) -> Result<Value, Error> {
    let (shape, values_meta) = (
        m::FUNC_DEF,
        vec![
            func_al.id.encode(arena)?,
            list(arena, "id", &func_al.tparams, encode_ref)?,
            list(arena, "param", &func_al.params, param)?,
            func_al.typ.encode(arena)?,
            list(arena, "clause", &func_al.clauses, clause)?,
            opt(arena, "elsclause", func_al.else_clause.as_ref(), clause)?,
        ],
    );
    case(arena, shape, "defn", values_meta, span)
}

/// Encodes a value or higher-order function parameter.
fn param(arena: &mut Arena, param_al: &al::Param) -> Result<Value, Error> {
    match &param_al.node {
        al::ParamKind::Exp(typ_al) => encode_exp_param(arena, typ_al, param_al.span),
        al::ParamKind::Def(id_al, tparams_al, params_al, typ_al) => {
            encode_func_param(arena, id_al, tparams_al, params_al, typ_al, param_al.span)
        }
    }
}

fn encode_exp_param(arena: &mut Arena, typ_al: &al::Typ, span: Span) -> Result<Value, Error> {
    let values_meta = vec![typ_al.encode(arena)?];
    case(arena, m::EXP_PARAM, "param", values_meta, span)
}

/// Encodes a higher-order parameter and its complete function signature.
fn encode_func_param(
    arena: &mut Arena,
    id_al: &al::Id,
    tparams_al: &[al::Id],
    params_al: &[al::Param],
    typ_al: &al::Typ,
    span: Span,
) -> Result<Value, Error> {
    let values_meta = vec![
        id_al.encode(arena)?,
        list(arena, "id", tparams_al, encode_ref)?,
        list(arena, "param", params_al, param)?,
        typ_al.encode(arena)?,
    ];
    case(arena, m::FUN_PARAM, "param", values_meta, span)
}

/// Encodes an AL premise.
fn prem(arena: &mut Arena, prem_al: &al::Prem) -> Result<Value, Error> {
    let span = prem_al.span;
    match &prem_al.node {
        al::PremKind::Rule(prem_inner_al) => encode_rule_prem(arena, prem_inner_al, span),
        al::PremKind::If(prem_inner_al) => encode_if_prem(arena, prem_inner_al, span),
        al::PremKind::IfHold(prem_inner_al) => encode_ifhold_prem(arena, prem_inner_al, span),
        al::PremKind::IfNotHold(prem_inner_al) => encode_ifnothold_prem(arena, prem_inner_al, span),
        al::PremKind::Let(prem_inner_al) => encode_let_prem(arena, prem_inner_al, span),
        al::PremKind::Iter(prem_inner_al) => encode_iter_prem(arena, prem_inner_al, span),
        al::PremKind::Debug(prem_inner_al) => encode_debug_prem(arena, prem_inner_al, span),
    }
}

/// Encodes a boolean condition premise.
fn encode_if_prem(
    arena: &mut Arena,
    prem_inner_al: &al::IfPrem,
    span: Span,
) -> Result<Value, Error> {
    let values_meta = vec![prem_inner_al.exp.encode(arena)?];
    case(arena, m::IF, "prem", values_meta, span)
}

/// Encodes a positive relation test.
fn encode_ifhold_prem(
    arena: &mut Arena,
    prem_inner_al: &al::IfHoldPrem,
    span: Span,
) -> Result<Value, Error> {
    let values_meta =
        vec![prem_inner_al.id.encode(arena)?, (prem_inner_al.not_exp.args()).encode(arena)?];
    case(arena, m::IFHOLD, "prem", values_meta, span)
}

/// Encodes a negative relation test.
fn encode_ifnothold_prem(
    arena: &mut Arena,
    prem_inner_al: &al::IfNotHoldPrem,
    span: Span,
) -> Result<Value, Error> {
    let values_meta =
        vec![prem_inner_al.id.encode(arena)?, (prem_inner_al.not_exp.args()).encode(arena)?];
    case(arena, m::IFNOTHOLD, "prem", values_meta, span)
}

/// Encodes the two expressions of a binding premise.
fn encode_let_prem(
    arena: &mut Arena,
    prem_inner_al: &al::LetPrem,
    span: Span,
) -> Result<Value, Error> {
    let values_meta = vec![prem_inner_al.exp_l.encode(arena)?, prem_inner_al.exp_r.encode(arena)?];
    case(arena, m::LET, "prem", values_meta, span)
}

/// Encodes an iterated premise and its bound variables.
fn encode_iter_prem(
    arena: &mut Arena,
    prem_inner_al: &al::IterPrem,
    span: Span,
) -> Result<Value, Error> {
    let values_meta =
        vec![prem(arena, &prem_inner_al.prem)?, iterprem(arena, &prem_inner_al.prem_iter)?];
    case(arena, m::ITER_PREM, "prem", values_meta, span)
}

/// Encodes the expression of a debug premise.
fn encode_debug_prem(
    arena: &mut Arena,
    prem_inner_al: &al::DebugPrem,
    span: Span,
) -> Result<Value, Error> {
    let values_meta = vec![prem_inner_al.exp.encode(arena)?];
    case(arena, m::DEBUG, "prem", values_meta, span)
}

/// Encodes bound and bound-by-iteration variable lists in order.
fn iterprem(arena: &mut Arena, prem_iter_al: &al::PremIter) -> Result<Value, Error> {
    let values_meta = vec![
        iter(arena, &prem_iter_al.iter)?,
        list(arena, "vari", &prem_iter_al.vars_bound, encode_ref)?,
        list(arena, "vari", &prem_iter_al.vars_bind, encode_ref)?,
    ];
    case(arena, m::ITERPREM, "iterprem", values_meta, Span::default())
}

/// Encodes a rule match without its printing signature.
fn rulematch(arena: &mut Arena, rule_match_al: &al::RuleMatch) -> Result<Value, Error> {
    let values_meta = vec![
        rule_match_al.exps_input.encode(arena)?,
        list(arena, "prem", &rule_match_al.prems, prem)?,
    ];
    case(arena, m::RULMATCH, "rulmatch", values_meta, Span::default())
}

/// Encodes a rule path and its outputs.
fn rulepath(arena: &mut Arena, rule_path_al: &al::RulePath) -> Result<Value, Error> {
    let values_meta = vec![
        rule_path_al.id.encode(arena)?,
        rule_path_al.exps_output.encode(arena)?,
        list(arena, "prem", &rule_path_al.prems, prem)?,
    ];
    case(arena, m::RULPATH, "rulpath", values_meta, rule_path_al.id.span)
}

/// Encodes a rule group with its shared match.
fn rulegroup(arena: &mut Arena, rule_group_al: &al::RuleGroup) -> Result<Value, Error> {
    let values_meta = vec![
        rule_group_al.node.id.encode(arena)?,
        rulematch(arena, &rule_group_al.node.rule_match)?,
        list(arena, "rulpath", &rule_group_al.node.rule_paths, rulepath)?,
    ];
    case(arena, m::RULGROUP, "rulgroup", values_meta, rule_group_al.span)
}

/// Encodes an otherwise group with one rule path.
fn elsegroup(arena: &mut Arena, group_else_al: &al::ElseGroup) -> Result<Value, Error> {
    let values_meta = vec![
        group_else_al.node.id.encode(arena)?,
        rulematch(arena, &group_else_al.node.rule_match)?,
        rulepath(arena, &group_else_al.node.rule_path)?,
    ];
    case(arena, m::ELSGROUP, "elsgroup", values_meta, group_else_al.span)
}

/// Encodes a function clause.
fn clause(arena: &mut Arena, clause_al: &al::Clause) -> Result<Value, Error> {
    let values_meta = vec![
        args(arena, &clause_al.node.args)?,
        clause_al.node.exp.encode(arena)?,
        list(arena, "prem", &clause_al.node.prems, prem)?,
    ];
    case(arena, m::CLAUSE, "clause", values_meta, clause_al.span)
}

/// Encodes a table row without its printing signature.
fn tablerow(arena: &mut Arena, row_al: &al::TableRow) -> Result<Value, Error> {
    let values_meta = vec![
        args(arena, &row_al.node.args)?,
        row_al.node.exp.encode(arena)?,
        list(arena, "prem", &row_al.node.prems, prem)?,
    ];
    case(arena, m::CLAUSE, "tblrow", values_meta, row_al.span)
}

/// Encodes a relation premise after splitting its operational input positions.
fn encode_rule_prem(
    arena: &mut Arena,
    prem_rule_al: &al::RulePrem,
    span: Span,
) -> Result<Value, Error> {
    // Preserve source order within each input and output group
    let (exps_input_al, exps_output_al) =
        input::split(&prem_rule_al.input_hint, prem_rule_al.not_exp.args().to_vec())?;
    let values_meta = vec![
        prem_rule_al.id.encode(arena)?,
        exps_input_al.encode(arena)?,
        exps_output_al.encode(arena)?,
    ];
    case(arena, m::REL_PREM, "prem", values_meta, span)
}
