//! Encodes source AL syntax and runtime values as meta data
//!
//! `Encode` implementations borrow the caller's arena without resetting it
//! Type implementations encode children before allocating their outer case
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

// == Helpers

/// Adapts encoding to the arena-first list mapper.
fn encode_ref<T: Encode + ?Sized>(arena: &mut Arena, source: &T) -> Result<T::Output, Error> {
    source.encode(arena)
}

/// Constructs a typed meta case after its children have been encoded.
fn case(
    arena: &mut Arena,
    shape: &str,
    name: &str,
    values_meta: Vec<Value>,
    span: Span,
) -> Result<Value, Error> {
    let mixfix_meta = Mixfix::new(parse::mixop(shape), values_meta)?;
    Ok(make::case(
        arena,
        make_typ::var(phrase!(node: name.into(), span: Span::default()), vec![])
            .node
            .into(),
        mixfix_meta,
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

fn split_mixop<'a>(
    mixop_al: &'a Mixop,
    atoms_tail_al: &mut Vec<&'a il::Atom>,
    atoms_rows_al: &mut Vec<Vec<&'a il::Atom>>,
) {
    match mixop_al {
        Mixop::Arg => atoms_rows_al.push(std::mem::take(atoms_tail_al)),
        Mixop::Atom(atom_al) => atoms_tail_al.push(atom_al),
        Mixop::Brack(atom_l_al, mixop_inner_al, atom_r_al) => {
            atoms_tail_al.push(atom_l_al);
            split_mixop(mixop_inner_al, atoms_tail_al, atoms_rows_al);
            atoms_tail_al.push(atom_r_al);
        }
        Mixop::Infix(mixop_l_al, atom_al, mixop_r_al) => {
            split_mixop(mixop_l_al, atoms_tail_al, atoms_rows_al);
            atoms_tail_al.push(atom_al);
            split_mixop(mixop_r_al, atoms_tail_al, atoms_rows_al);
        }
        Mixop::Seq(mixops_al) => {
            for mixop_al in mixops_al {
                split_mixop(mixop_al, atoms_tail_al, atoms_rows_al);
            }
        }
    }
}

// == Encoding

// - Identifiers

/// Encodes an identifier as text, retaining its span.
impl Encode for Id {
    type Output = Value;

    fn encode(&self, arena: &mut Arena) -> Result<Self::Output, Error> {
        Ok(make::text(arena, self.node.to_string(), self.span)?)
    }
}

// - Atoms

/// Encodes a notation atom by its source spelling.
impl Encode for il::Atom {
    type Output = Value;

    fn encode(&self, arena: &mut Arena) -> Result<Self::Output, Error> {
        Ok(make::text(arena, self.to_string(), self.span)?)
    }
}

// - Mixops

/// Encodes atoms between argument positions as a nonempty matrix.
impl Encode for Mixop {
    type Output = Value;

    fn encode(&self, arena: &mut Arena) -> Result<Self::Output, Error> {
        // Split atoms at argument holes, including empty boundary rows
        let mut atoms_rows_al = Vec::new();
        let mut atoms_tail_al = Vec::new();
        split_mixop(self, &mut atoms_tail_al, &mut atoms_rows_al);
        atoms_rows_al.push(atoms_tail_al);

        // Encode every row before allocating the outer matrix
        let mut values_meta = Vec::with_capacity(atoms_rows_al.len());
        for atoms_row_al in atoms_rows_al {
            values_meta
                .push(list(arena, "atom", &atoms_row_al, |arena, atom_al| atom_al.encode(arena))?);
        }

        // Retain both list dimensions even when a row is empty
        let typ_matrix_meta = crate::lang::data::typ::make::list(
            crate::lang::data::typ::make::list(crate::lang::data::typ::make::text()),
        );
        Ok(make::list(arena, std::rc::Rc::new(typ_matrix_meta.node), values_meta, Span::default())?)
    }
}

// - Iterators

/// Encodes a source iterator.
impl Encode for Iter {
    type Output = Value;

    fn encode(&self, arena: &mut Arena) -> Result<Self::Output, Error> {
        let shape = match self {
            Iter::Opt => m::QUEST,
            Iter::List => m::STAR,
        };
        case(arena, shape, "iter", vec![], Span::default())
    }
}

// - Variables

/// Encodes a source variable with its underlying type and dimensions.
impl Encode for il::Var {
    type Output = Value;

    fn encode(&self, arena: &mut Arena) -> Result<Self::Output, Error> {
        let value_id = self.id.encode(arena)?;
        let value_typ = self.typ.encode(arena)?;
        let value_iters = list(arena, "iter", &self.iters, encode_ref)?;
        case(arena, m::VARI, "vari", vec![value_id, value_typ, value_iters], self.id.span)
    }
}

// - Results

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

// - Types

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
            il::TypKind::Var(id_al, targs_al) => {
                let values_meta = vec![id_al.encode(arena)?, targs_al.encode(arena)?];
                case(arena, m::VAR_TYP, "typ", values_meta, span).map_err(|error| error.at(span))
            }
            il::TypKind::Tuple(typs_al) => {
                let value_typs = typs_al.encode(arena)?;
                case(arena, m::TUP_TYP, "typ", vec![value_typs], span)
                    .map_err(|error| error.at(span))
            }
            il::TypKind::Iter(typ_inner_al, iter_al) => {
                let values_meta = vec![typ_inner_al.encode(arena)?, iter_al.encode(arena)?];
                case(arena, m::ITER_TYP, "typ", values_meta, span).map_err(|error| error.at(span))
            }
            il::TypKind::Func(typ_func_al) => {
                let values_meta = vec![
                    list(arena, "id", &typ_func_al.tparams, encode_ref)?,
                    typ_func_al.typs_params.encode(arena)?,
                    typ_func_al.typ_ret.encode(arena)?,
                ];
                case(arena, m::FUNC_TYP, "typ", values_meta, span).map_err(|error| error.at(span))
            }
        }
    }
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
                (m::STRUCT, vec![list(arena, "typfield", typ_fields_al, encode_ref)?])
            }
            il::DefTypKind::Variant(typ_cases_al) => {
                (m::VARIANT, vec![list(arena, "typcase", typ_cases_al, encode_ref)?])
            }
        };
        case(arena, shape, "deftyp", values_meta, deftyp_al.span)
    }
}

/// Encodes one struct type field.
impl Encode for il::TypField {
    type Output = Value;

    fn encode(&self, arena: &mut Arena) -> Result<Self::Output, Error> {
        let values_meta = vec![self.atom.encode(arena)?, self.typ.encode(arena)?];
        case(arena, m::TYPFIELD, "typfield", values_meta, self.atom.span)
    }
}

/// Encodes one variant case without its origin or hints.
impl Encode for il::TypCase {
    type Output = Value;

    fn encode(&self, arena: &mut Arena) -> Result<Self::Output, Error> {
        let values_meta =
            vec![self.not_typ.node.mixop().encode(arena)?, self.not_typ.node.args().encode(arena)?];
        case(arena, m::TYPCASE, "typcase", values_meta, self.not_typ.span)
    }
}

// - Values

/// Encodes a runtime value's payload in the same arena.
impl Encode for Value {
    type Output = Value;

    fn encode(&self, arena: &mut Arena) -> Result<Self::Output, Error> {
        let value_target = *self;
        let span = *arena.value.span(value_target.span);
        match arena.value.kind(value_target.node) {
            ValueKind::Bool(value_bool) => {
                let value_bool = *value_bool;
                let values_meta = vec![make::bool(arena, value_bool, span)?];
                case(arena, m::BOOL, "val", values_meta, span).map_err(|error| error.at(span))
            }
            ValueKind::Num(num) => {
                let num_target = num.clone();
                let shape = match &num_target {
                    Number::Nat(_) => m::NAT,
                    Number::Int(_) => m::INT,
                };
                let values_meta = vec![make::num(arena, num_target, span)?];
                case(arena, shape, "val", values_meta, span).map_err(|error| error.at(span))
            }
            ValueKind::Text(text) => {
                let text_target = text.clone();
                let values_meta = vec![make::text(arena, text_target, span)?];
                case(arena, m::TEXT, "val", values_meta, span).map_err(|error| error.at(span))
            }
            ValueKind::Struct(value_fields) => {
                let value_fields = value_fields.clone();
                let values_meta = vec![list(arena, "valfield", &value_fields, encode_ref)?];
                case(arena, m::STR, "val", values_meta, span).map_err(|error| error.at(span))
            }
            ValueKind::Case(value_case) => {
                let mixop_target = value_case.mixop().into_tree(&arena.mixop);
                let values_target = value_case.args().to_vec();
                let values_case =
                    vec![mixop_target.encode(arena)?, values_target.as_slice().encode(arena)?];
                let values_meta = vec![case(arena, m::VALCASE, "valcase", values_case, span)?];
                case(arena, m::INJ, "val", values_meta, span).map_err(|error| error.at(span))
            }
            ValueKind::Tuple(values_target) => {
                let values_target = values_target.clone();
                let values_meta = vec![values_target.as_slice().encode(arena)?];
                case(arena, m::TUP, "val", values_meta, span).map_err(|error| error.at(span))
            }
            ValueKind::Opt(value_inner) => {
                let value_inner = *value_inner;
                let values_meta =
                    vec![opt(arena, "val", value_inner.as_ref(), |arena, value_inner| {
                        (*value_inner).encode(arena)
                    })?];
                case(arena, m::OPT, "val", values_meta, span).map_err(|error| error.at(span))
            }
            ValueKind::List(values_target) => {
                let values_target = values_target.clone();
                let values_meta = vec![values_target.as_slice().encode(arena)?];
                case(arena, m::LIST, "val", values_meta, span).map_err(|error| error.at(span))
            }
            ValueKind::Func(id_target) => {
                let id_target = id_target.clone();
                let values_meta = vec![id_target.encode(arena)?];
                case(arena, m::FUNC, "val", values_meta, span).map_err(|error| error.at(span))
            }
            ValueKind::Extern(json) => {
                let json_target = json.clone();
                let value_json = make::external(
                    arena,
                    make_typ::var(phrase!(node: "json".into(), span: Span::default()), vec![])
                        .node
                        .into(),
                    json_target,
                    span,
                )?;
                case(arena, m::EXT, "val", vec![value_json], span).map_err(|error| error.at(span))
            }
        }
    }
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
impl Encode for ValueField {
    type Output = Value;

    fn encode(&self, arena: &mut Arena) -> Result<Self::Output, Error> {
        let values_meta = vec![self.atom.encode(arena)?, self.value.encode(arena)?];
        case(arena, m::VALFIELD, "valfield", values_meta, self.atom.span)
    }
}

// - Expressions

/// Encodes every source expression form without native optimization data.
impl Encode for il::Exp {
    type Output = Value;

    fn encode(&self, arena: &mut Arena) -> Result<Self::Output, Error> {
        let span = self.span;
        let (shape, values_meta) = match &self.node {
            il::ExpKind::Bool(value_bool) => (m::BOOL, vec![make::bool(arena, *value_bool, span)?]),
            il::ExpKind::Num(num) => {
                let shape = match num {
                    Number::Nat(_) => m::NAT,
                    Number::Int(_) => m::INT,
                };
                (shape, vec![make::num(arena, num.clone(), span)?])
            }
            il::ExpKind::Text(text) => (m::TEXT, vec![make::text(arena, text.clone(), span)?]),
            il::ExpKind::Id(id_al) => (m::VAR_EXP, vec![id_al.encode(arena)?]),
            il::ExpKind::Un(op_al, _, exp_al) => {
                (m::UN, vec![op_al.encode(arena)?, exp_al.encode(arena)?])
            }
            il::ExpKind::Bin(op_al, _, exp_l_al, exp_r_al) => (
                m::BIN,
                vec![op_al.encode(arena)?, exp_l_al.encode(arena)?, exp_r_al.encode(arena)?],
            ),
            il::ExpKind::Cmp(op_al, _, exp_l_al, exp_r_al) => (
                m::CMP,
                vec![op_al.encode(arena)?, exp_l_al.encode(arena)?, exp_r_al.encode(arena)?],
            ),
            il::ExpKind::UpCast(typ_al, exp_al) => {
                (m::UPCAST, vec![typ_al.encode(arena)?, exp_al.encode(arena)?])
            }
            il::ExpKind::DownCast(typ_al, exp_al) => {
                (m::DOWNCAST, vec![typ_al.encode(arena)?, exp_al.encode(arena)?])
            }
            il::ExpKind::Sub(exp_al, typ_al, _) => {
                (m::SUB_EXP, vec![exp_al.encode(arena)?, typ_al.encode(arena)?])
            }
            il::ExpKind::Match(exp_al, pattern_al) => {
                (m::MATCH, vec![exp_al.encode(arena)?, pattern_al.encode(arena)?])
            }
            il::ExpKind::Tuple(exps_al) => (m::TUP, vec![exps_al.encode(arena)?]),
            il::ExpKind::Case(exp_case_al) => {
                let values_case =
                    vec![exp_case_al.mixop().encode(arena)?, exp_case_al.args().encode(arena)?];
                (m::INJ, vec![case(arena, m::EXPCASE, "expcase", values_case, span)?])
            }
            il::ExpKind::Str(fields_al) => {
                (m::STR, vec![list(arena, "expfield", fields_al, encode_ref)?])
            }
            il::ExpKind::Opt(exp_al) => {
                (m::OPT, vec![opt(arena, "exp", exp_al.as_deref(), encode_ref)?])
            }
            il::ExpKind::List(exps_al) => (m::LIST, vec![exps_al.encode(arena)?]),
            il::ExpKind::Cons(exp_head_al, exp_tail_al) => {
                (m::CONS, vec![exp_head_al.encode(arena)?, exp_tail_al.encode(arena)?])
            }
            il::ExpKind::Cat(exp_l_al, exp_r_al) => {
                (m::CAT, vec![exp_l_al.encode(arena)?, exp_r_al.encode(arena)?])
            }
            il::ExpKind::Mem(exp_elem_al, exp_list_al) => {
                (m::MEM, vec![exp_elem_al.encode(arena)?, exp_list_al.encode(arena)?])
            }
            il::ExpKind::Len(exp_al) => (m::LEN, vec![exp_al.encode(arena)?]),
            il::ExpKind::Dot(exp_al, atom_al) => {
                (m::DOT, vec![exp_al.encode(arena)?, atom_al.encode(arena)?])
            }
            il::ExpKind::Idx(exp_al, exp_idx_al) => {
                (m::IDX, vec![exp_al.encode(arena)?, exp_idx_al.encode(arena)?])
            }
            il::ExpKind::Slice(exp_al, exp_idx_al, exp_len_al) => (
                m::SLICE,
                vec![exp_al.encode(arena)?, exp_idx_al.encode(arena)?, exp_len_al.encode(arena)?],
            ),
            il::ExpKind::Upd(exp_al, path_al, exp_upd_al) => (
                m::UPD,
                vec![exp_al.encode(arena)?, path_al.encode(arena)?, exp_upd_al.encode(arena)?],
            ),
            il::ExpKind::Call(id_al, targs_al, args_al) => (
                m::CALL,
                vec![
                    id_al.encode(arena)?,
                    targs_al.encode(arena)?,
                    list(arena, "arg", args_al, encode_ref)?,
                ],
            ),
            il::ExpKind::Iter(exp_al, exp_iter_al) => {
                (m::ITER_EXP, vec![exp_al.encode(arena)?, exp_iter_al.encode(arena)?])
            }
        };
        case(arena, shape, "exp", values_meta, span)
    }
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
impl Encode for il::ExpField {
    type Output = Value;

    fn encode(&self, arena: &mut Arena) -> Result<Self::Output, Error> {
        let values_meta = vec![self.atom.encode(arena)?, self.exp.encode(arena)?];
        case(arena, m::EXPFIELD, "expfield", values_meta, self.atom.span)
    }
}

/// Encodes an expression iterator and its variables.
impl Encode for il::ExpIter {
    type Output = Value;

    fn encode(&self, arena: &mut Arena) -> Result<Self::Output, Error> {
        let values_meta =
            vec![self.iter.encode(arena)?, list(arena, "vari", &self.vars, encode_ref)?];
        case(arena, m::ITEREXP, "iterexp", values_meta, Span::default())
    }
}

/// Encodes a value or function argument.
impl Encode for il::Arg {
    type Output = Value;

    fn encode(&self, arena: &mut Arena) -> Result<Self::Output, Error> {
        let (shape, value) = match &self.node {
            il::ArgKind::Exp(exp_al) => (m::EXP_ARG, exp_al.encode(arena)?),
            il::ArgKind::Def(id_al) => (m::FUN_ARG, id_al.encode(arena)?),
        };
        case(arena, shape, "arg", vec![value], self.span)
    }
}

// - Patterns

/// Encodes a case, list, or option shape pattern.
impl Encode for il::Pattern {
    type Output = Value;

    fn encode(&self, arena: &mut Arena) -> Result<Self::Output, Error> {
        let (shape, values_meta) = match self {
            il::Pattern::Case(mixop_al) => (m::CASE_PATTERN, vec![mixop_al.encode(arena)?]),
            il::Pattern::List(il::ListPattern::Cons) => (m::CONS_PATTERN, vec![]),
            il::Pattern::List(il::ListPattern::Fixed(num_len)) => {
                let nat_meta = u64::try_from(*num_len).map_err(ValueError::from)?.into();
                let value_nat = make::nat(arena, nat_meta, Span::default())?;
                (m::FIXED, vec![value_nat])
            }
            il::Pattern::List(il::ListPattern::Nil) => (m::NIL, vec![]),
            il::Pattern::Opt(il::OptPattern::Some) => (m::SOME, vec![]),
            il::Pattern::Opt(il::OptPattern::None) => (m::NONE, vec![]),
        };
        case(arena, shape, "pattern", values_meta, Span::default())
    }
}

// - Paths

/// Encodes a source update path.
impl Encode for il::Path {
    type Output = Value;

    fn encode(&self, arena: &mut Arena) -> Result<Self::Output, Error> {
        let span = self.span;
        match &self.node {
            il::PathKind::Root => case(arena, m::ROOT, "path", vec![], span),
            il::PathKind::Idx(path_base_al, exp_idx_al) => {
                let values_meta = vec![path_base_al.encode(arena)?, exp_idx_al.encode(arena)?];
                case(arena, m::IDX, "path", values_meta, span)
            }
            il::PathKind::Slice(path_base_al, exp_idx_al, exp_len_al) => {
                let values_meta = vec![
                    path_base_al.encode(arena)?,
                    exp_idx_al.encode(arena)?,
                    exp_len_al.encode(arena)?,
                ];
                case(arena, m::SLICE, "path", values_meta, span)
            }
            il::PathKind::Dot(path_base_al, atom_al) => {
                let values_meta = vec![path_base_al.encode(arena)?, atom_al.encode(arena)?];
                case(arena, m::DOT, "path", values_meta, span)
            }
        }
    }
}

// - Operators

/// Encodes a unary operator.
impl Encode for il::UnOp {
    type Output = Value;

    fn encode(&self, arena: &mut Arena) -> Result<Self::Output, Error> {
        let shape = match self {
            il::UnOp::Bool(boolean::UnOp::Not) => m::NOT,
            il::UnOp::Num(num::UnOp::Plus) => m::PLUS,
            il::UnOp::Num(num::UnOp::Minus) => m::MINUS,
        };
        case(arena, shape, "unop", vec![], Span::default())
    }
}

/// Encodes a binary operator.
impl Encode for il::BinOp {
    type Output = Value;

    fn encode(&self, arena: &mut Arena) -> Result<Self::Output, Error> {
        let shape = match self {
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
}

/// Encodes a comparison operator.
impl Encode for il::CmpOp {
    type Output = Value;

    fn encode(&self, arena: &mut Arena) -> Result<Self::Output, Error> {
        let shape = match self {
            il::CmpOp::Bool(boolean::CmpOp::Eq) => m::EQ,
            il::CmpOp::Bool(boolean::CmpOp::Ne) => m::NE,
            il::CmpOp::Num(num::CmpOp::Lt) => m::LT,
            il::CmpOp::Num(num::CmpOp::Le) => m::LE,
            il::CmpOp::Num(num::CmpOp::Gt) => m::GT,
            il::CmpOp::Num(num::CmpOp::Ge) => m::GE,
        };
        case(arena, shape, "cmpop", vec![], Span::default())
    }
}

// - Definitions

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
        let span = self.span;
        let (shape, values_meta) = match &self.node {
            al::DefKind::Var(_) => return Ok(None),
            al::DefKind::Typ(al::TypDef::Extern(typdef_al)) => {
                (m::EXTTYP, vec![typdef_al.id.encode(arena)?])
            }
            al::DefKind::Typ(al::TypDef::Defined(typdef_al)) => (
                m::TYP_DEF,
                vec![
                    typdef_al.id.encode(arena)?,
                    list(arena, "id", &typdef_al.tparams, encode_ref)?,
                    typdef_al.def_typ.encode(arena)?,
                ],
            ),
            al::DefKind::Rel(al::RelDef::Extern(rel_al)) => {
                let (typs_input_al, typs_output_al) =
                    input::split(&rel_al.input_hint, rel_al.not_typ.node.args().to_vec())?;
                (
                    m::EXTREL,
                    vec![
                        rel_al.id.encode(arena)?,
                        typs_input_al.encode(arena)?,
                        typs_output_al.encode(arena)?,
                    ],
                )
            }
            al::DefKind::Rel(al::RelDef::Defined(rel_al)) => {
                let (typs_input_al, typs_output_al) =
                    input::split(&rel_al.input_hint, rel_al.not_typ.node.args().to_vec())?;
                (
                    m::REL_DEF,
                    vec![
                        rel_al.id.encode(arena)?,
                        typs_input_al.encode(arena)?,
                        typs_output_al.encode(arena)?,
                        list(arena, "rulgroup", &rel_al.rule_groups, encode_ref)?,
                        opt(arena, "elsgroup", rel_al.else_group.as_ref(), encode_ref)?,
                    ],
                )
            }
            al::DefKind::MetaFunc(al::MetaFuncDef::Extern(func_al)) => (
                m::EXTFUNC,
                vec![
                    func_al.id.encode(arena)?,
                    list(arena, "id", &func_al.tparams, encode_ref)?,
                    list(arena, "param", &func_al.params, encode_ref)?,
                    func_al.typ.encode(arena)?,
                ],
            ),
            al::DefKind::MetaFunc(al::MetaFuncDef::Builtin(func_al)) => (
                m::BUILTINFUNC,
                vec![
                    func_al.id.encode(arena)?,
                    list(arena, "id", &func_al.tparams, encode_ref)?,
                    list(arena, "param", &func_al.params, encode_ref)?,
                    func_al.typ.encode(arena)?,
                ],
            ),
            al::DefKind::MetaFunc(al::MetaFuncDef::Table(func_al)) => (
                m::TABLEFUNC,
                vec![
                    func_al.id.encode(arena)?,
                    list(arena, "param", &func_al.params, encode_ref)?,
                    func_al.typ.encode(arena)?,
                    list(arena, "tblrow", &func_al.table_rows, encode_ref)?,
                ],
            ),
            al::DefKind::MetaFunc(al::MetaFuncDef::Defined(func_al)) => (
                m::FUNC_DEF,
                vec![
                    func_al.id.encode(arena)?,
                    list(arena, "id", &func_al.tparams, encode_ref)?,
                    list(arena, "param", &func_al.params, encode_ref)?,
                    func_al.typ.encode(arena)?,
                    list(arena, "clause", &func_al.clauses, encode_ref)?,
                    opt(arena, "elsclause", func_al.else_clause.as_ref(), encode_ref)?,
                ],
            ),
        };
        Ok(Some(case(arena, shape, "defn", values_meta, span)?))
    }
}

// - Parameters

/// Encodes a value or higher-order function parameter.
impl Encode for al::Param {
    type Output = Value;

    fn encode(&self, arena: &mut Arena) -> Result<Self::Output, Error> {
        let (shape, values_meta) = match &self.node {
            al::ParamKind::Exp(typ_al) => (m::EXP_PARAM, vec![typ_al.encode(arena)?]),
            al::ParamKind::Def(id_al, tparams_al, params_al, typ_al) => (
                m::FUN_PARAM,
                vec![
                    id_al.encode(arena)?,
                    list(arena, "id", tparams_al, encode_ref)?,
                    list(arena, "param", params_al, encode_ref)?,
                    typ_al.encode(arena)?,
                ],
            ),
        };
        case(arena, shape, "param", values_meta, self.span)
    }
}

// - Premises

/// Encodes an AL premise.
impl Encode for al::Prem {
    type Output = Value;

    fn encode(&self, arena: &mut Arena) -> Result<Self::Output, Error> {
        let span = self.span;
        let (shape, values_meta) = match &self.node {
            al::PremKind::Rule(prem_rule_al) => {
                let (exps_input_al, exps_output_al) =
                    input::split(&prem_rule_al.input_hint, prem_rule_al.not_exp.args().to_vec())?;
                (
                    m::REL_PREM,
                    vec![
                        prem_rule_al.id.encode(arena)?,
                        exps_input_al.encode(arena)?,
                        exps_output_al.encode(arena)?,
                    ],
                )
            }
            al::PremKind::If(prem_inner_al) => (m::IF, vec![prem_inner_al.exp.encode(arena)?]),
            al::PremKind::IfHold(prem_inner_al) => (
                m::IFHOLD,
                vec![prem_inner_al.id.encode(arena)?, prem_inner_al.not_exp.args().encode(arena)?],
            ),
            al::PremKind::IfNotHold(prem_inner_al) => (
                m::IFNOTHOLD,
                vec![prem_inner_al.id.encode(arena)?, prem_inner_al.not_exp.args().encode(arena)?],
            ),
            al::PremKind::Let(prem_inner_al) => (
                m::LET,
                vec![prem_inner_al.exp_l.encode(arena)?, prem_inner_al.exp_r.encode(arena)?],
            ),
            al::PremKind::Iter(prem_inner_al) => (
                m::ITER_PREM,
                vec![prem_inner_al.prem.encode(arena)?, prem_inner_al.prem_iter.encode(arena)?],
            ),
            al::PremKind::Debug(prem_inner_al) => {
                (m::DEBUG, vec![prem_inner_al.exp.encode(arena)?])
            }
        };
        case(arena, shape, "prem", values_meta, span)
    }
}

/// Encodes bound and bound-by-iteration variable lists in order.
impl Encode for al::PremIter {
    type Output = Value;

    fn encode(&self, arena: &mut Arena) -> Result<Self::Output, Error> {
        let values_meta = vec![
            self.iter.encode(arena)?,
            list(arena, "vari", &self.vars_bound, encode_ref)?,
            list(arena, "vari", &self.vars_bind, encode_ref)?,
        ];
        case(arena, m::ITERPREM, "iterprem", values_meta, Span::default())
    }
}

// - Rules

/// Encodes a rule match without its printing signature.
impl Encode for al::RuleMatch {
    type Output = Value;

    fn encode(&self, arena: &mut Arena) -> Result<Self::Output, Error> {
        let values_meta =
            vec![self.exps_input.encode(arena)?, list(arena, "prem", &self.prems, encode_ref)?];
        case(arena, m::RULMATCH, "rulmatch", values_meta, Span::default())
    }
}

/// Encodes a rule path and its outputs.
impl Encode for al::RulePath {
    type Output = Value;

    fn encode(&self, arena: &mut Arena) -> Result<Self::Output, Error> {
        let values_meta = vec![
            self.id.encode(arena)?,
            self.exps_output.encode(arena)?,
            list(arena, "prem", &self.prems, encode_ref)?,
        ];
        case(arena, m::RULPATH, "rulpath", values_meta, self.id.span)
    }
}

/// Encodes a rule group with its shared match.
impl Encode for al::RuleGroup {
    type Output = Value;

    fn encode(&self, arena: &mut Arena) -> Result<Self::Output, Error> {
        let values_meta = vec![
            self.node.id.encode(arena)?,
            self.node.rule_match.encode(arena)?,
            list(arena, "rulpath", &self.node.rule_paths, encode_ref)?,
        ];
        case(arena, m::RULGROUP, "rulgroup", values_meta, self.span)
    }
}

/// Encodes an otherwise group with one rule path.
impl Encode for al::ElseGroup {
    type Output = Value;

    fn encode(&self, arena: &mut Arena) -> Result<Self::Output, Error> {
        let values_meta = vec![
            self.node.id.encode(arena)?,
            self.node.rule_match.encode(arena)?,
            self.node.rule_path.encode(arena)?,
        ];
        case(arena, m::ELSGROUP, "elsgroup", values_meta, self.span)
    }
}

// - Clauses

/// Encodes a function clause.
impl Encode for al::Clause {
    type Output = Value;

    fn encode(&self, arena: &mut Arena) -> Result<Self::Output, Error> {
        let values_meta = vec![
            list(arena, "arg", &self.node.args, encode_ref)?,
            self.node.exp.encode(arena)?,
            list(arena, "prem", &self.node.prems, encode_ref)?,
        ];
        case(arena, m::CLAUSE, "clause", values_meta, self.span)
    }
}

/// Encodes a table row without its printing signature.
impl Encode for al::TableRow {
    type Output = Value;

    fn encode(&self, arena: &mut Arena) -> Result<Self::Output, Error> {
        let values_meta = vec![
            list(arena, "arg", &self.node.args, encode_ref)?,
            self.node.exp.encode(arena)?,
            list(arena, "prem", &self.node.prems, encode_ref)?,
        ];
        case(arena, m::CLAUSE, "tblrow", values_meta, self.span)
    }
}
