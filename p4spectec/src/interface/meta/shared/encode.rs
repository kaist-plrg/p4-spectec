//! Encodes syntax shared by IL and AL as meta data
//!
//! `Encode` implementations borrow the caller's arena without resetting it.
//! Types, expressions, paths, arguments, parameters, and iteration premises
//! are encoded here for both the IL and the AL codecs.

use super::{Cache, Error, mixops as m};
use crate::lang::common::prim::num::Number;
use crate::{
    lang::{
        common::{
            Id, Iter,
            prim::{bool as boolean, num},
            source::Span,
        },
        data::{
            arena::Arena,
            notation::{
                flat::{Mixfix as FlatMixfix, Mixop as FlatMixop, parse::ToMixop},
                items::Item,
                tree::Mixop,
            },
            typ::make as make_typ,
            value::{
                ValueError,
                flat::{Value, make},
            },
        },
        il::ast as il,
        traits::print::Print,
    },
    phrase,
};

/// Converts source IL or AL data into its meta representation.
pub trait Encode {
    /// The encoded value, or an omitted source declaration.
    type Output;

    /// Encodes this data without retaining conversion results.
    fn encode(&self, arena: &mut Arena) -> Result<Self::Output, Error> {
        self.encode_cached(arena, &mut Cache::new(false))
    }

    /// Encodes this data with a cache belonging to the supplied arena.
    fn encode_cached(&self, arena: &mut Arena, cache: &mut Cache) -> Result<Self::Output, Error>;
}

// == Helpers

/// Constructs a typed meta case after its children have been encoded.
pub(in crate::interface) fn case(
    arena: &mut Arena,
    text_shape: &'static str,
    text_typ: &str,
    values_meta: Vec<Value>,
    span: Span,
) -> Result<Value, Error> {
    let mixop_meta = text_shape
        .to_mixop(&mut arena.mixop)
        .map_err(|error| Error::from(error).at(span))?;
    let mixfix_meta = FlatMixfix::new(&arena.mixop, mixop_meta, values_meta)
        .map_err(|error| Error::from(error).at(span))?;
    make::case_flat(
        arena,
        make_typ::var(phrase!(node: text_typ.into(), span: Span::default()), vec![])
            .node
            .into(),
        mixfix_meta,
        span,
    )
    .map_err(|error| Error::from(error).at(span))
}

/// Maps a source list and annotates its meta element type.
pub(in crate::interface) fn list<T: Encode<Output = Value>>(
    arena: &mut Arena,
    cache: &mut Cache,
    text_elem_typ: &str,
    values_source: &[T],
) -> Result<Value, Error> {
    let values_meta = values_source
        .iter()
        .map(|value_source| value_source.encode_cached(arena, cache))
        .collect::<Result<_, _>>()?;
    Ok(make::list(
        arena,
        make_typ::list(make_typ::var(
            phrase!(node: text_elem_typ.into(), span: Span::default()),
            vec![],
        ))
        .node
        .into(),
        values_meta,
        Span::default(),
    )?)
}

/// Maps an optional source value and retains its meta element type.
pub(in crate::interface) fn opt<T: Encode<Output = Value>>(
    arena: &mut Arena,
    cache: &mut Cache,
    text_elem_typ: &str,
    value_source: Option<&T>,
) -> Result<Value, Error> {
    let value_meta = value_source
        .map(|value_source| value_source.encode_cached(arena, cache))
        .transpose()?;
    Ok(make::opt(
        arena,
        make_typ::opt(make_typ::var(
            phrase!(node: text_elem_typ.into(), span: Span::default()),
            vec![],
        ))
        .node
        .into(),
        value_meta,
        Span::default(),
    )?)
}

/// Pairs an encoded expression or path form with its type annotation.
fn typed(
    arena: &mut Arena,
    cache: &mut Cache,
    text_shape: &'static str,
    text_typ: &str,
    value_kind: Value,
    typ_kind: &il::TypKind,
    span: Span,
) -> Result<Value, Error> {
    let typ = phrase!(node: typ_kind.clone(), span: span);
    let value_typ = typ.encode_cached(arena, cache)?;
    case(arena, text_shape, text_typ, vec![value_kind, value_typ], span)
}

// == Encoding

// - References

impl<T: Encode + ?Sized> Encode for &T {
    type Output = T::Output;

    fn encode_cached(&self, arena: &mut Arena, cache: &mut Cache) -> Result<Self::Output, Error> {
        (*self).encode_cached(arena, cache)
    }
}

// - Identifiers

/// Encodes an identifier as text, retaining its span.
impl Encode for Id {
    type Output = Value;

    fn encode_cached(&self, arena: &mut Arena, _cache: &mut Cache) -> Result<Self::Output, Error> {
        Ok(make::text(arena, self.node.to_string(), self.span)?)
    }
}

// - Atoms

/// Encodes a notation atom by its source spelling.
impl Encode for il::Atom {
    type Output = Value;

    fn encode_cached(&self, arena: &mut Arena, _cache: &mut Cache) -> Result<Self::Output, Error> {
        Ok(make::text(arena, self.to_string(), self.span)?)
    }
}

// - Mixops

/// Separates notation atoms at each argument position.
fn atoms_matrix(mixop_al: &Mixop) -> Vec<Vec<&il::Atom>> {
    let mut atoms_matrix_al = vec![vec![]];
    mixop_al.visit(|item| match item {
        Item::Arg(_) => atoms_matrix_al.push(vec![]),
        Item::Atom(atom_al) => atoms_matrix_al.last_mut().unwrap().push(atom_al),
    });
    atoms_matrix_al
}

/// Encodes atoms between argument positions as a nonempty matrix.
impl Encode for Mixop {
    type Output = Value;

    fn encode_cached(&self, arena: &mut Arena, cache: &mut Cache) -> Result<Self::Output, Error> {
        // Split atoms at argument holes, including empty boundary rows
        let atoms_matrix_al = atoms_matrix(self);

        encode_atoms_matrix(arena, cache, &atoms_matrix_al)
    }
}

/// Encodes a flat notation without reconstructing an owned notation tree.
impl Encode for FlatMixop {
    type Output = Value;

    fn encode_cached(&self, arena: &mut Arena, cache: &mut Cache) -> Result<Value, Error> {
        // Reuse encoded notation independently of atom locations
        let id_mixop = arena.mixop.canon_id(*self);
        if let Some(value_meta) = cache.mixops_encode.get(&id_mixop) {
            return Ok(*value_meta);
        }
        // Copy atoms before allocating encoded rows in the same arena
        let mut atoms_matrix = vec![vec![]];
        self.visit(&arena.mixop, |item| match item {
            Item::Arg(_) => atoms_matrix.push(vec![]),
            Item::Atom(atom) => atoms_matrix.last_mut().unwrap().push(atom.clone()),
        });
        let value_meta = encode_atoms_matrix(arena, cache, &atoms_matrix)?;
        cache.mixops_encode.insert(id_mixop, value_meta);
        Ok(value_meta)
    }
}

/// Encodes each atom row before allocating the surrounding matrix.
fn encode_atoms_matrix<T: Encode<Output = Value>>(
    arena: &mut Arena,
    cache: &mut Cache,
    atoms_matrix: &[Vec<T>],
) -> Result<Value, Error> {
    let mut values_meta = Vec::with_capacity(atoms_matrix.len());
    for atoms in atoms_matrix {
        values_meta.push(list(arena, cache, "atom", atoms)?);
    }
    // Empty boundary rows still occupy their original matrix positions
    let typ_matrix = make_typ::list(make_typ::list(make_typ::text()));
    Ok(make::list(arena, typ_matrix.node.into(), values_meta, Span::default())?)
}

// - Iterators

/// Encodes a source iterator.
impl Encode for Iter {
    type Output = Value;

    fn encode_cached(&self, arena: &mut Arena, _cache: &mut Cache) -> Result<Self::Output, Error> {
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

    fn encode_cached(&self, arena: &mut Arena, cache: &mut Cache) -> Result<Self::Output, Error> {
        let value_id = self.id.encode_cached(arena, cache)?;
        let value_typ = self.typ.encode_cached(arena, cache)?;
        let value_iters = list(arena, cache, "iter", &self.iters)?;
        case(arena, m::VARI, "vari", vec![value_id, value_typ, value_iters], self.id.span)
    }
}

// - Types

/// Encodes a source type, including complete function signatures.
impl Encode for il::Typ {
    type Output = Value;

    fn encode_cached(&self, arena: &mut Arena, cache: &mut Cache) -> Result<Self::Output, Error> {
        let typ_al = self;
        let span = typ_al.span;
        match &typ_al.node {
            il::TypKind::Bool => case(arena, m::BOOL_TYP, "typ", vec![], span),
            il::TypKind::Num(num::Typ::Nat) => case(arena, m::NAT_TYP, "typ", vec![], span),
            il::TypKind::Num(num::Typ::Int) => case(arena, m::INT_TYP, "typ", vec![], span),
            il::TypKind::Text => case(arena, m::TEXT_TYP, "typ", vec![], span),
            il::TypKind::Var(id_al, targs_al) => {
                let values_meta =
                    vec![id_al.encode_cached(arena, cache)?, list(arena, cache, "typ", targs_al)?];
                case(arena, m::VAR_TYP, "typ", values_meta, span)
            }
            il::TypKind::Tuple(typs_al) => {
                let value_typs = list(arena, cache, "typ", typs_al)?;
                case(arena, m::TUP_TYP, "typ", vec![value_typs], span)
            }
            il::TypKind::Iter(typ_inner_al, iter_al) => {
                let values_meta = vec![
                    typ_inner_al.encode_cached(arena, cache)?,
                    iter_al.encode_cached(arena, cache)?,
                ];
                case(arena, m::ITER_TYP, "typ", values_meta, span)
            }
            il::TypKind::Func(typ_func_al) => {
                let values_meta = vec![
                    list(arena, cache, "id", &typ_func_al.tparams)?,
                    list(arena, cache, "typ", &typ_func_al.typs_params)?,
                    typ_func_al.typ_ret.encode_cached(arena, cache)?,
                ];
                case(arena, m::FUNC_TYP, "typ", values_meta, span)
            }
        }
    }
}

// - Defined types

/// Encodes an alias, struct, or variant definition.
impl Encode for il::DefTyp {
    type Output = Value;

    fn encode_cached(&self, arena: &mut Arena, cache: &mut Cache) -> Result<Self::Output, Error> {
        let deftyp_al = self;
        let (shape, values_meta) = match &deftyp_al.node {
            il::DefTypKind::Plain(typ_al) => (m::ALIAS, vec![typ_al.encode_cached(arena, cache)?]),
            il::DefTypKind::Struct(typ_fields_al) => {
                (m::STRUCT, vec![list(arena, cache, "typField", typ_fields_al)?])
            }
            il::DefTypKind::Variant(typ_cases_al) => {
                (m::VARIANT, vec![list(arena, cache, "typCase", typ_cases_al)?])
            }
        };
        case(arena, shape, "deftyp", values_meta, deftyp_al.span)
    }
}

/// Encodes one struct type field.
impl Encode for il::TypField {
    type Output = Value;

    fn encode_cached(&self, arena: &mut Arena, cache: &mut Cache) -> Result<Self::Output, Error> {
        let values_meta =
            vec![self.atom.encode_cached(arena, cache)?, self.typ.encode_cached(arena, cache)?];
        case(arena, m::TYPFIELD, "typField", values_meta, self.atom.span)
    }
}

/// Encodes one variant case without its origin or hints.
impl Encode for il::TypCase {
    type Output = Value;

    fn encode_cached(&self, arena: &mut Arena, cache: &mut Cache) -> Result<Self::Output, Error> {
        let values_meta = vec![
            self.not_typ.node.mixop().encode_cached(arena, cache)?,
            list(arena, cache, "typ", self.not_typ.node.args())?,
        ];
        case(arena, m::TYPCASE, "typCase", values_meta, self.not_typ.span)
    }
}

// - Operators

/// Encodes a unary operator.
impl Encode for il::UnOp {
    type Output = Value;

    fn encode_cached(&self, arena: &mut Arena, _cache: &mut Cache) -> Result<Self::Output, Error> {
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

    fn encode_cached(&self, arena: &mut Arena, _cache: &mut Cache) -> Result<Self::Output, Error> {
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

    fn encode_cached(&self, arena: &mut Arena, _cache: &mut Cache) -> Result<Self::Output, Error> {
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

// - Expressions

/// Encodes every source expression form without native optimization data.
impl Encode for il::Exp {
    type Output = Value;

    fn encode_cached(&self, arena: &mut Arena, cache: &mut Cache) -> Result<Self::Output, Error> {
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
            il::ExpKind::Id(id_al) => (m::VAR_EXP, vec![id_al.encode_cached(arena, cache)?]),
            il::ExpKind::Un(op_al, _, exp_al) => (
                m::UN,
                vec![op_al.encode_cached(arena, cache)?, exp_al.encode_cached(arena, cache)?],
            ),
            il::ExpKind::Bin(op_al, _, exp_l_al, exp_r_al) => (
                m::BIN,
                vec![
                    op_al.encode_cached(arena, cache)?,
                    exp_l_al.encode_cached(arena, cache)?,
                    exp_r_al.encode_cached(arena, cache)?,
                ],
            ),
            il::ExpKind::Cmp(op_al, _, exp_l_al, exp_r_al) => (
                m::CMP,
                vec![
                    op_al.encode_cached(arena, cache)?,
                    exp_l_al.encode_cached(arena, cache)?,
                    exp_r_al.encode_cached(arena, cache)?,
                ],
            ),
            il::ExpKind::UpCast(typ_al, exp_al) => (
                m::UPCAST,
                vec![typ_al.encode_cached(arena, cache)?, exp_al.encode_cached(arena, cache)?],
            ),
            il::ExpKind::DownCast(typ_al, exp_al) => (
                m::DOWNCAST,
                vec![typ_al.encode_cached(arena, cache)?, exp_al.encode_cached(arena, cache)?],
            ),
            il::ExpKind::Sub(exp_al, typ_al) => (
                m::SUB_EXP,
                vec![exp_al.encode_cached(arena, cache)?, typ_al.encode_cached(arena, cache)?],
            ),
            il::ExpKind::Match(exp_al, pattern_al) => (
                m::MATCH,
                vec![exp_al.encode_cached(arena, cache)?, pattern_al.encode_cached(arena, cache)?],
            ),
            il::ExpKind::Tuple(exps_al) => (m::TUP, vec![list(arena, cache, "exp", exps_al)?]),
            il::ExpKind::Case(exp_case_al) => {
                let values_case = vec![
                    exp_case_al.mixop().encode_cached(arena, cache)?,
                    list(arena, cache, "exp", exp_case_al.args())?,
                ];
                (m::INJ, vec![case(arena, m::EXPCASE, "expCase", values_case, span)?])
            }
            il::ExpKind::Str(fields_al) => {
                (m::STR, vec![list(arena, cache, "expField", fields_al)?])
            }
            il::ExpKind::Opt(exp_al) => {
                (m::OPT, vec![opt(arena, cache, "exp", exp_al.as_deref())?])
            }
            il::ExpKind::List(exps_al) => (m::LIST, vec![list(arena, cache, "exp", exps_al)?]),
            il::ExpKind::Cons(exp_head_al, exp_tail_al) => (
                m::CONS,
                vec![
                    exp_head_al.encode_cached(arena, cache)?,
                    exp_tail_al.encode_cached(arena, cache)?,
                ],
            ),
            il::ExpKind::Cat(exp_l_al, exp_r_al) => (
                m::CAT,
                vec![exp_l_al.encode_cached(arena, cache)?, exp_r_al.encode_cached(arena, cache)?],
            ),
            il::ExpKind::Mem(exp_elem_al, exp_list_al) => (
                m::MEM,
                vec![
                    exp_elem_al.encode_cached(arena, cache)?,
                    exp_list_al.encode_cached(arena, cache)?,
                ],
            ),
            il::ExpKind::Len(exp_al) => (m::LEN, vec![exp_al.encode_cached(arena, cache)?]),
            il::ExpKind::Dot(exp_al, atom_al) => (
                m::DOT,
                vec![exp_al.encode_cached(arena, cache)?, atom_al.encode_cached(arena, cache)?],
            ),
            il::ExpKind::Idx(exp_al, exp_idx_al) => (
                m::IDX,
                vec![exp_al.encode_cached(arena, cache)?, exp_idx_al.encode_cached(arena, cache)?],
            ),
            il::ExpKind::Slice(exp_al, exp_idx_al, exp_len_al) => (
                m::SLICE,
                vec![
                    exp_al.encode_cached(arena, cache)?,
                    exp_idx_al.encode_cached(arena, cache)?,
                    exp_len_al.encode_cached(arena, cache)?,
                ],
            ),
            il::ExpKind::Upd(exp_al, path_al, exp_upd_al) => (
                m::UPD,
                vec![
                    exp_al.encode_cached(arena, cache)?,
                    path_al.encode_cached(arena, cache)?,
                    exp_upd_al.encode_cached(arena, cache)?,
                ],
            ),
            il::ExpKind::Call(id_al, targs_al, args_al) => (
                m::CALL,
                vec![
                    id_al.encode_cached(arena, cache)?,
                    list(arena, cache, "typ", targs_al)?,
                    list(arena, cache, "arg", args_al)?,
                ],
            ),
            il::ExpKind::Iter(exp_al, exp_iter_al) => (
                m::ITER_EXP,
                vec![exp_al.encode_cached(arena, cache)?, exp_iter_al.encode_cached(arena, cache)?],
            ),
        };
        let value_kind = case(arena, shape, "expKind", values_meta, span)?;
        typed(arena, cache, m::EXP, "exp", value_kind, &self.note, span)
    }
}

/// Encodes one source struct-expression field.
impl Encode for il::ExpField {
    type Output = Value;

    fn encode_cached(&self, arena: &mut Arena, cache: &mut Cache) -> Result<Self::Output, Error> {
        let values_meta =
            vec![self.atom.encode_cached(arena, cache)?, self.exp.encode_cached(arena, cache)?];
        case(arena, m::EXPFIELD, "expField", values_meta, self.atom.span)
    }
}

/// Encodes an expression iterator and its variables.
impl Encode for il::ExpIter {
    type Output = Value;

    fn encode_cached(&self, arena: &mut Arena, cache: &mut Cache) -> Result<Self::Output, Error> {
        let values_meta =
            vec![self.iter.encode_cached(arena, cache)?, list(arena, cache, "vari", &self.vars)?];
        case(arena, m::ITEREXP, "iterExp", values_meta, Span::default())
    }
}

// - Patterns

/// Encodes a case, list, or option shape pattern.
impl Encode for il::Pattern {
    type Output = Value;

    fn encode_cached(&self, arena: &mut Arena, cache: &mut Cache) -> Result<Self::Output, Error> {
        let (shape, values_meta) = match self {
            il::Pattern::Case(mixop_al) => {
                (m::CASE_PATTERN, vec![mixop_al.encode_cached(arena, cache)?])
            }
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
        case(arena, shape, "pat", values_meta, Span::default())
    }
}

// - Paths

/// Encodes a source update path.
impl Encode for il::Path {
    type Output = Value;

    fn encode_cached(&self, arena: &mut Arena, cache: &mut Cache) -> Result<Self::Output, Error> {
        let span = self.span;
        let value_kind = match &self.node {
            il::PathKind::Root => case(arena, m::ROOT, "pathKind", vec![], span),
            il::PathKind::Idx(path_base_al, exp_idx_al) => {
                let values_meta = vec![
                    path_base_al.encode_cached(arena, cache)?,
                    exp_idx_al.encode_cached(arena, cache)?,
                ];
                case(arena, m::IDX, "pathKind", values_meta, span)
            }
            il::PathKind::Slice(path_base_al, exp_idx_al, exp_len_al) => {
                let values_meta = vec![
                    path_base_al.encode_cached(arena, cache)?,
                    exp_idx_al.encode_cached(arena, cache)?,
                    exp_len_al.encode_cached(arena, cache)?,
                ];
                case(arena, m::SLICE, "pathKind", values_meta, span)
            }
            il::PathKind::Dot(path_base_al, atom_al) => {
                let values_meta = vec![
                    path_base_al.encode_cached(arena, cache)?,
                    atom_al.encode_cached(arena, cache)?,
                ];
                case(arena, m::DOT, "pathKind", values_meta, span)
            }
        }?;
        typed(arena, cache, m::PATH, "path", value_kind, &self.note, span)
    }
}

// - Parameters

/// Encodes a value or higher-order function parameter.
impl Encode for il::Param {
    type Output = Value;

    fn encode_cached(&self, arena: &mut Arena, cache: &mut Cache) -> Result<Self::Output, Error> {
        let (shape, values_meta) = match &self.node {
            il::ParamKind::Exp(typ_al) => (m::EXP_PARAM, vec![typ_al.encode_cached(arena, cache)?]),
            il::ParamKind::Def(id_al, tparams_al, params_al, typ_al) => (
                m::FUN_PARAM,
                vec![
                    id_al.encode_cached(arena, cache)?,
                    list(arena, cache, "id", tparams_al)?,
                    list(arena, cache, "param", params_al)?,
                    typ_al.encode_cached(arena, cache)?,
                ],
            ),
        };
        case(arena, shape, "param", values_meta, self.span)
    }
}

// - Arguments

/// Encodes a value or function argument.
impl Encode for il::Arg {
    type Output = Value;

    fn encode_cached(&self, arena: &mut Arena, cache: &mut Cache) -> Result<Self::Output, Error> {
        let (shape, value) = match &self.node {
            il::ArgKind::Exp(exp_al) => (m::EXP_ARG, exp_al.encode_cached(arena, cache)?),
            il::ArgKind::Def(id_al) => (m::FUN_ARG, id_al.encode_cached(arena, cache)?),
        };
        case(arena, shape, "arg", vec![value], self.span)
    }
}
