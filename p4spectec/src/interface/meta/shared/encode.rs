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
pub(crate) fn list<T: Encode<Output = Value>>(
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

/// Encodes a notation tree constructor by constructor.
impl Encode for Mixop {
    type Output = Value;

    fn encode_cached(&self, arena: &mut Arena, cache: &mut Cache) -> Result<Self::Output, Error> {
        match self {
            Mixop::Arg => case(arena, m::MIXOP_ARG, "mixop", vec![], Span::default()),
            Mixop::Atom(atom) => {
                let value_atom = atom.encode_cached(arena, cache)?;
                case(arena, m::MIXOP_ATOM, "mixop", vec![value_atom], Span::default())
            }
            Mixop::Brack(atom_l, mixop_inner, atom_r) => {
                let value_atom_l = atom_l.encode_cached(arena, cache)?;
                let value_mixop_inner = mixop_inner.as_ref().encode_cached(arena, cache)?;
                let value_atom_r = atom_r.encode_cached(arena, cache)?;
                let values_meta = vec![value_atom_l, value_mixop_inner, value_atom_r];
                case(arena, m::MIXOP_BRACK, "mixop", values_meta, Span::default())
            }
            Mixop::Infix(mixop_l, atom, mixop_r) => {
                let value_mixop_l = mixop_l.as_ref().encode_cached(arena, cache)?;
                let value_atom = atom.encode_cached(arena, cache)?;
                let value_mixop_r = mixop_r.as_ref().encode_cached(arena, cache)?;
                let values_meta = vec![value_mixop_l, value_atom, value_mixop_r];
                case(arena, m::MIXOP_INFIX, "mixop", values_meta, Span::default())
            }
            Mixop::Seq(mixops) => {
                let value_mixops = list(arena, cache, "mixop", mixops)?;
                case(arena, m::MIXOP_SEQ, "mixop", vec![value_mixops], Span::default())
            }
        }
    }
}

/// Encodes a flat notation through its tree, reusing earlier encodings.
impl Encode for FlatMixop {
    type Output = Value;

    fn encode_cached(&self, arena: &mut Arena, cache: &mut Cache) -> Result<Value, Error> {
        // Reuse encoded notation independently of atom locations
        let id_mixop = arena.mixop.canon_id(*self);
        if let Some(value_meta) = cache.mixops_encode.get(&id_mixop) {
            return Ok(*value_meta);
        }
        // Copy the notation out of its arena before allocating the encoding
        let mixop = self.into_tree(&arena.mixop);
        let value_meta = mixop.encode_cached(arena, cache)?;
        cache.mixops_encode.insert(id_mixop, value_meta);
        Ok(value_meta)
    }
}

// - Iterators

/// Encodes a source iterator.
impl Encode for Iter {
    type Output = Value;

    fn encode_cached(&self, arena: &mut Arena, _cache: &mut Cache) -> Result<Self::Output, Error> {
        let shape = match self {
            Iter::Opt => m::ITER_QUEST,
            Iter::List => m::ITER_STAR,
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
            il::TypKind::Bool => case(arena, m::TYP_BOOL, "optyp", vec![], span),
            il::TypKind::Num(num::Typ::Nat) => case(arena, m::TYP_NAT, "numtyp", vec![], span),
            il::TypKind::Num(num::Typ::Int) => case(arena, m::TYP_INT, "numtyp", vec![], span),
            il::TypKind::Text => case(arena, m::TYP_TEXT, "optyp", vec![], span),
            il::TypKind::Var(id_al, targs_al) => {
                let values_meta =
                    vec![id_al.encode_cached(arena, cache)?, list(arena, cache, "typ", targs_al)?];
                case(arena, m::TYP_VAR, "typ", values_meta, span)
            }
            il::TypKind::Tuple(typs_al) => {
                let value_typs = list(arena, cache, "typ", typs_al)?;
                case(arena, m::TYP_TUP, "typ", vec![value_typs], span)
            }
            il::TypKind::Iter(typ_inner_al, iter_al) => {
                let values_meta = vec![
                    typ_inner_al.encode_cached(arena, cache)?,
                    iter_al.encode_cached(arena, cache)?,
                ];
                case(arena, m::TYP_ITER, "typ", values_meta, span)
            }
            il::TypKind::Func(typ_func_al) => {
                let values_meta = vec![
                    list(arena, cache, "id", &typ_func_al.tparams)?,
                    list(arena, cache, "typ", &typ_func_al.typs_params)?,
                    typ_func_al.typ_ret.encode_cached(arena, cache)?,
                ];
                case(arena, m::TYP_FUNC, "typ", values_meta, span)
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
            il::DefTypKind::Plain(typ_al) => {
                (m::DEFTYP_ALIAS, vec![typ_al.encode_cached(arena, cache)?])
            }
            il::DefTypKind::Struct(typ_fields_al) => {
                (m::DEFTYP_STRUCT, vec![list(arena, cache, "typField", typ_fields_al)?])
            }
            il::DefTypKind::Variant(typ_cases_al) => {
                (m::DEFTYP_VARIANT, vec![list(arena, cache, "typCase", typ_cases_al)?])
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
        case(arena, m::TYP_FIELD, "typField", values_meta, self.atom.span)
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
        case(arena, m::TYP_CASE, "typCase", values_meta, self.not_typ.span)
    }
}

// - Operators

/// Encodes a unary operator.
impl Encode for il::UnOp {
    type Output = Value;

    fn encode_cached(&self, arena: &mut Arena, _cache: &mut Cache) -> Result<Self::Output, Error> {
        let (shape, typ) = match self {
            il::UnOp::Bool(boolean::UnOp::Not) => (m::UNOP_NOT, "boolUnOp"),
            il::UnOp::Num(num::UnOp::Plus) => (m::UNOP_PLUS, "numUnOp"),
            il::UnOp::Num(num::UnOp::Minus) => (m::UNOP_MINUS, "numUnOp"),
        };
        case(arena, shape, typ, vec![], Span::default())
    }
}

/// Encodes a binary operator.
impl Encode for il::BinOp {
    type Output = Value;

    fn encode_cached(&self, arena: &mut Arena, _cache: &mut Cache) -> Result<Self::Output, Error> {
        let (shape, typ) = match self {
            il::BinOp::Bool(boolean::BinOp::And) => (m::BINOP_AND, "boolBinOp"),
            il::BinOp::Bool(boolean::BinOp::Or) => (m::BINOP_OR, "boolBinOp"),
            il::BinOp::Bool(boolean::BinOp::Impl) => (m::BINOP_IMPL, "boolBinOp"),
            il::BinOp::Bool(boolean::BinOp::Equiv) => (m::BINOP_EQUIV, "boolBinOp"),
            il::BinOp::Num(num::BinOp::Add) => (m::BINOP_ADD, "numBinOp"),
            il::BinOp::Num(num::BinOp::Sub) => (m::BINOP_SUB, "numBinOp"),
            il::BinOp::Num(num::BinOp::Mul) => (m::BINOP_MUL, "numBinOp"),
            il::BinOp::Num(num::BinOp::Div) => (m::BINOP_DIV, "numBinOp"),
            il::BinOp::Num(num::BinOp::Mod) => (m::BINOP_MOD, "numBinOp"),
            il::BinOp::Num(num::BinOp::Pow) => (m::BINOP_POW, "numBinOp"),
        };
        case(arena, shape, typ, vec![], Span::default())
    }
}

/// Encodes a comparison operator.
impl Encode for il::CmpOp {
    type Output = Value;

    fn encode_cached(&self, arena: &mut Arena, _cache: &mut Cache) -> Result<Self::Output, Error> {
        let (shape, typ) = match self {
            il::CmpOp::Bool(boolean::CmpOp::Eq) => (m::CMPOP_EQ, "polyCmpOp"),
            il::CmpOp::Bool(boolean::CmpOp::Ne) => (m::CMPOP_NE, "polyCmpOp"),
            il::CmpOp::Num(num::CmpOp::Lt) => (m::CMPOP_LT, "numCmpOp"),
            il::CmpOp::Num(num::CmpOp::Le) => (m::CMPOP_LE, "numCmpOp"),
            il::CmpOp::Num(num::CmpOp::Gt) => (m::CMPOP_GT, "numCmpOp"),
            il::CmpOp::Num(num::CmpOp::Ge) => (m::CMPOP_GE, "numCmpOp"),
        };
        case(arena, shape, typ, vec![], Span::default())
    }
}

// - Expressions

/// Encodes every source expression form without native optimization data.
impl Encode for il::Exp {
    type Output = Value;

    fn encode_cached(&self, arena: &mut Arena, cache: &mut Cache) -> Result<Self::Output, Error> {
        let span = self.span;
        let (shape, typ_case, values_meta) = match &self.node {
            il::ExpKind::Bool(value_bool) => {
                (m::EXP_BOOL, "literalExp", vec![make::bool(arena, *value_bool, span)?])
            }
            il::ExpKind::Num(num) => {
                let shape = match num {
                    Number::Nat(_) => m::EXP_NAT,
                    Number::Int(_) => m::EXP_INT,
                };
                (shape, "num", vec![make::num(arena, num.clone(), span)?])
            }
            il::ExpKind::Text(text) => {
                (m::EXP_TEXT, "literalExp", vec![make::text(arena, text.clone(), span)?])
            }
            il::ExpKind::Id(id_al) => {
                (m::EXP_VAR, "varExp", vec![id_al.encode_cached(arena, cache)?])
            }
            il::ExpKind::Un(op_al, _, exp_al) => (
                m::EXP_UN,
                "unOpExp",
                vec![op_al.encode_cached(arena, cache)?, exp_al.encode_cached(arena, cache)?],
            ),
            il::ExpKind::Bin(op_al, _, exp_l_al, exp_r_al) => (
                m::EXP_BIN,
                "binOpExp",
                vec![
                    op_al.encode_cached(arena, cache)?,
                    exp_l_al.encode_cached(arena, cache)?,
                    exp_r_al.encode_cached(arena, cache)?,
                ],
            ),
            il::ExpKind::Cmp(op_al, _, exp_l_al, exp_r_al) => (
                m::EXP_CMP,
                "cmpOpExp",
                vec![
                    op_al.encode_cached(arena, cache)?,
                    exp_l_al.encode_cached(arena, cache)?,
                    exp_r_al.encode_cached(arena, cache)?,
                ],
            ),
            il::ExpKind::UpCast(typ_al, exp_al) => (
                m::EXP_UPCAST,
                "upCastExp",
                vec![typ_al.encode_cached(arena, cache)?, exp_al.encode_cached(arena, cache)?],
            ),
            il::ExpKind::DownCast(typ_al, exp_al) => (
                m::EXP_DOWNCAST,
                "downCastExp",
                vec![typ_al.encode_cached(arena, cache)?, exp_al.encode_cached(arena, cache)?],
            ),
            il::ExpKind::Sub(exp_al, typ_al) => (
                m::EXP_SUB,
                "subExp",
                vec![exp_al.encode_cached(arena, cache)?, typ_al.encode_cached(arena, cache)?],
            ),
            il::ExpKind::Match(exp_al, pattern_al) => (
                m::EXP_MATCH,
                "matchExp",
                vec![exp_al.encode_cached(arena, cache)?, pattern_al.encode_cached(arena, cache)?],
            ),
            il::ExpKind::Tuple(exps_al) => {
                (m::EXP_TUP, "tupExp", vec![list(arena, cache, "typedExp", exps_al)?])
            }
            il::ExpKind::Case(exp_case_al) => {
                let values_case = vec![
                    exp_case_al.mixop().encode_cached(arena, cache)?,
                    list(arena, cache, "typedExp", exp_case_al.args())?,
                ];
                (
                    m::EXP_INJ,
                    "injExp",
                    vec![case(arena, m::EXP_CASE, "expCase", values_case, span)?],
                )
            }
            il::ExpKind::Str(fields_al) => {
                (m::EXP_STR, "strExp", vec![list(arena, cache, "expField", fields_al)?])
            }
            il::ExpKind::Opt(exp_al) => {
                (m::EXP_OPT, "optExp", vec![opt(arena, cache, "typedExp", exp_al.as_deref())?])
            }
            il::ExpKind::List(exps_al) => {
                (m::EXP_LIST, "listExp", vec![list(arena, cache, "typedExp", exps_al)?])
            }
            il::ExpKind::Cons(exp_head_al, exp_tail_al) => (
                m::EXP_CONS,
                "consExp",
                vec![
                    exp_head_al.encode_cached(arena, cache)?,
                    exp_tail_al.encode_cached(arena, cache)?,
                ],
            ),
            il::ExpKind::Cat(exp_l_al, exp_r_al) => (
                m::EXP_CAT,
                "catExp",
                vec![exp_l_al.encode_cached(arena, cache)?, exp_r_al.encode_cached(arena, cache)?],
            ),
            il::ExpKind::Mem(exp_elem_al, exp_list_al) => (
                m::EXP_MEM,
                "memExp",
                vec![
                    exp_elem_al.encode_cached(arena, cache)?,
                    exp_list_al.encode_cached(arena, cache)?,
                ],
            ),
            il::ExpKind::Len(exp_al) => {
                (m::EXP_LEN, "lenExp", vec![exp_al.encode_cached(arena, cache)?])
            }
            il::ExpKind::Dot(exp_al, atom_al) => (
                m::EXP_DOT,
                "dotAccExp",
                vec![exp_al.encode_cached(arena, cache)?, atom_al.encode_cached(arena, cache)?],
            ),
            il::ExpKind::Idx(exp_al, exp_idx_al) => (
                m::EXP_IDX,
                "idxAccExp",
                vec![exp_al.encode_cached(arena, cache)?, exp_idx_al.encode_cached(arena, cache)?],
            ),
            il::ExpKind::Slice(exp_al, exp_idx_al, exp_len_al) => (
                m::EXP_SLICE,
                "sliceAccExp",
                vec![
                    exp_al.encode_cached(arena, cache)?,
                    exp_idx_al.encode_cached(arena, cache)?,
                    exp_len_al.encode_cached(arena, cache)?,
                ],
            ),
            il::ExpKind::Upd(exp_al, path_al, exp_upd_al) => (
                m::EXP_UPD,
                "updExp",
                vec![
                    exp_al.encode_cached(arena, cache)?,
                    path_al.encode_cached(arena, cache)?,
                    exp_upd_al.encode_cached(arena, cache)?,
                ],
            ),
            il::ExpKind::Call(id_al, targs_al, args_al) => (
                m::EXP_CALL,
                "callExp",
                vec![
                    id_al.encode_cached(arena, cache)?,
                    list(arena, cache, "typ", targs_al)?,
                    list(arena, cache, "arg", args_al)?,
                ],
            ),
            il::ExpKind::Iter(exp_al, exp_iter_al) => (
                m::EXP_ITER,
                "iterExp",
                vec![exp_al.encode_cached(arena, cache)?, exp_iter_al.encode_cached(arena, cache)?],
            ),
        };
        let value_kind = case(arena, shape, typ_case, values_meta, span)?;
        // The note becomes the type annotation of the typed expression
        let typ = phrase!(node: il::TypKind::clone(&self.note), span: span);
        let value_typ = typ.encode_cached(arena, cache)?;
        case(arena, m::TYPED_EXP, "typedExp", vec![value_kind, value_typ], span)
    }
}

/// Encodes one source struct-expression field.
impl Encode for il::ExpField {
    type Output = Value;

    fn encode_cached(&self, arena: &mut Arena, cache: &mut Cache) -> Result<Self::Output, Error> {
        let values_meta =
            vec![self.atom.encode_cached(arena, cache)?, self.exp.encode_cached(arena, cache)?];
        case(arena, m::EXP_FIELD, "expField", values_meta, self.atom.span)
    }
}

/// Encodes an expression iterator and its variables.
impl Encode for il::ExpIter {
    type Output = Value;

    fn encode_cached(&self, arena: &mut Arena, cache: &mut Cache) -> Result<Self::Output, Error> {
        let values_meta =
            vec![self.iter.encode_cached(arena, cache)?, list(arena, cache, "vari", &self.vars)?];
        case(arena, m::ITER_EXP, "expIter", values_meta, Span::default())
    }
}

// - Patterns

/// Encodes a case, list, or option shape pattern.
impl Encode for il::Pattern {
    type Output = Value;

    fn encode_cached(&self, arena: &mut Arena, cache: &mut Cache) -> Result<Self::Output, Error> {
        let (shape, typ, values_meta) = match self {
            il::Pattern::Case(mixop_al) => {
                (m::PAT_INJ, "pat", vec![mixop_al.encode_cached(arena, cache)?])
            }
            il::Pattern::List(il::ListPattern::Cons) => (m::PAT_CONS, "listPat", vec![]),
            il::Pattern::List(il::ListPattern::Fixed(num_len)) => {
                let nat_meta = u64::try_from(*num_len).map_err(ValueError::from)?.into();
                let value_nat = make::nat(arena, nat_meta, Span::default())?;
                (m::PAT_FIXED, "listPat", vec![value_nat])
            }
            il::Pattern::List(il::ListPattern::Nil) => (m::PAT_NIL, "listPat", vec![]),
            il::Pattern::Opt(il::OptPattern::Some) => (m::PAT_SOME, "optPat", vec![]),
            il::Pattern::Opt(il::OptPattern::None) => (m::PAT_NONE, "optPat", vec![]),
        };
        case(arena, shape, typ, values_meta, Span::default())
    }
}

// - Paths

/// Encodes a source update path.
impl Encode for il::Path {
    type Output = Value;

    fn encode_cached(&self, arena: &mut Arena, cache: &mut Cache) -> Result<Self::Output, Error> {
        let span = self.span;
        let value_kind = match &self.node {
            il::PathKind::Root => case(arena, m::PATH_ROOT, "rootPath", vec![], span),
            il::PathKind::Idx(path_base_al, exp_idx_al) => {
                let values_meta = vec![
                    path_base_al.encode_cached(arena, cache)?,
                    exp_idx_al.encode_cached(arena, cache)?,
                ];
                case(arena, m::PATH_IDX, "idxPath", values_meta, span)
            }
            il::PathKind::Slice(path_base_al, exp_idx_al, exp_len_al) => {
                let values_meta = vec![
                    path_base_al.encode_cached(arena, cache)?,
                    exp_idx_al.encode_cached(arena, cache)?,
                    exp_len_al.encode_cached(arena, cache)?,
                ];
                case(arena, m::PATH_SLICE, "slicePath", values_meta, span)
            }
            il::PathKind::Dot(path_base_al, atom_al) => {
                let values_meta = vec![
                    path_base_al.encode_cached(arena, cache)?,
                    atom_al.encode_cached(arena, cache)?,
                ];
                case(arena, m::PATH_DOT, "dotPath", values_meta, span)
            }
        }?;
        // The note becomes the type annotation of the typed path
        let typ = phrase!(node: il::TypKind::clone(&self.note), span: span);
        let value_typ = typ.encode_cached(arena, cache)?;
        case(arena, m::TYPED_PATH, "typedPath", vec![value_kind, value_typ], span)
    }
}

// - Parameters

/// Encodes a value or higher-order function parameter.
impl Encode for il::Param {
    type Output = Value;

    fn encode_cached(&self, arena: &mut Arena, cache: &mut Cache) -> Result<Self::Output, Error> {
        let (shape, typ, values_meta) = match &self.node {
            il::ParamKind::Exp(typ_al) => {
                (m::PARAM_EXP, "expParam", vec![typ_al.encode_cached(arena, cache)?])
            }
            il::ParamKind::Def(id_al, tparams_al, params_al, typ_al) => (
                m::PARAM_FUN,
                "funParam",
                vec![
                    id_al.encode_cached(arena, cache)?,
                    list(arena, cache, "id", tparams_al)?,
                    list(arena, cache, "param", params_al)?,
                    typ_al.encode_cached(arena, cache)?,
                ],
            ),
        };
        case(arena, shape, typ, values_meta, self.span)
    }
}

// - Arguments

/// Encodes a value or function argument.
impl Encode for il::Arg {
    type Output = Value;

    fn encode_cached(&self, arena: &mut Arena, cache: &mut Cache) -> Result<Self::Output, Error> {
        let (shape, typ, value) = match &self.node {
            il::ArgKind::Exp(exp_al) => (m::ARG_EXP, "expArg", exp_al.encode_cached(arena, cache)?),
            il::ArgKind::Def(id_al) => (m::ARG_FUN, "funArg", id_al.encode_cached(arena, cache)?),
        };
        case(arena, shape, typ, vec![value], self.span)
    }
}
