//! IL syntax with resolved slots and interned mixops
//!
//! These aliases instantiate the shared AST with the prepared stage.
//! AL, SL, and PL reuse these expressions and supporting syntax;
//! preparation operations live under `interp/shared/prepare`.

use super::{ast as source, stage::Stage};

pub use super::ast::{
    Atom, BinOp, CmpOp, DefinedTyp, ExternTyp, FuncTyp, Hint, Id, Iter, ListPattern, Num, NumOp,
    OpTyp, OptPattern, Param, ParamKind, TParam, Targ, TargKind, Text, Typ, TypDef, TypField,
    TypKind, TypOrigin, TypOriginKind, UnOp, Value, ValueCase, ValueField, ValueKind, VarDef,
};
pub use super::stage::Prepared;

// - Variables

/// A variable with its frame slot.
pub type Var = <Prepared as Stage>::Var;

// - Notation

/// An interned notation shape.
pub type Mixop = <Prepared as Stage>::Mixop;

// - Expressions

/// An expression over slot-resolved identifiers.
///
/// Rendering uses `view(arena_mixop)` to obtain [`super::print::ExpRef`].
pub type Exp = source::Exp<Prepared>;
pub type ExpField = source::ExpField<Prepared>;
pub type ExpKind = source::ExpKind<Prepared>;
pub type NotExp = source::NotExp<Prepared>;
pub type ExpIter = source::ExpIter<Var>;

// - Paths

/// A path over slot-resolved identifiers.
pub type Path = source::Path<Prepared>;
pub type PathKind = source::PathKind<Prepared>;

// - Arguments

/// An argument over slot-resolved identifiers.
pub type Arg = source::Arg<Prepared>;
pub type ArgKind = source::ArgKind<Prepared>;

// - Type definitions

/// A notation type over a prepared shape.
pub type NotTyp = source::NotTyp<Prepared>;
pub type NotTypKind = source::NotTypKind<Prepared>;
/// A type definition body whose case notations are shapes.
pub type DefTyp = source::DefTyp<Prepared>;
pub type DefTypKind = source::DefTypKind<Prepared>;
pub type TypCase = source::TypCase<Prepared>;

// - Patterns and subtype checks

/// A pattern over prepared mixops.
pub type Pattern = source::Pattern<Prepared>;
/// A runtime subtype check over prepared mixops.
pub type Subcheck = source::Subcheck<Prepared>;

// - Premises

/// A premise iterator over slot-resolved variables.
pub type PremIter = source::PremIter<Var>;
