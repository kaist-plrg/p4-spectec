//! SL syntax with resolved slots, interned mixops, and staged rule calls
//!
//! These aliases instantiate the shared SL instructions and definitions.
//! Ordinary rules retain their continuation; tail rules carry only inputs.
//! Preparation establishes tail position in `interp/sl/prepare.rs`.

pub use crate::lang::il::prepared::*;

use super::ast as source;

pub use super::ast::{DefinedTyp, ExternTyp, RelSignature, TypDef, VarDef};

// == Prepared syntax

// - Parameters

pub type Param = source::Param<Prepared>;
pub type ParamKind = source::ParamKind<Prepared>;

// - Dangling

pub type Dangle = source::Dangle;

// - Holding conditions

pub type HoldCase = source::HoldCase<Prepared>;

// - Case analysis

pub type Guard = source::Guard<Prepared>;
pub type Case = source::Case<Prepared>;

// - Instructions

pub type Instr = source::Instr<Prepared>;
pub type InstrKind = source::InstrKind<Prepared>;
pub type IfInstr = source::IfInstr<Prepared>;
pub type HoldInstr = source::HoldInstr<Prepared>;
pub type CaseInstr = source::CaseInstr<Prepared>;
pub type GroupInstr = source::GroupInstr<Prepared>;
pub type LetInstr = source::LetInstr<Prepared>;
pub type ResultInstr = source::ResultInstr<Prepared>;
pub type ReturnInstr = source::ReturnInstr<Prepared>;
pub type DebugInstr = source::DebugInstr<Prepared>;
pub type InstrIter = PremIter;

// - Blocks

pub type Block = source::Block<Prepared>;
pub type ElseBlock = source::ElseBlock<Prepared>;

// - Table rows

pub type TableRow = source::TableRow<Prepared>;

// - Relation definitions

pub type RelDef = source::RelDef<Prepared>;
pub type ExternRel = source::ExternRel<Prepared>;
pub type DefinedRel = source::DefinedRel<Prepared>;

// - Meta-function definitions

pub type MetaFuncDef = source::MetaFuncDef<Prepared>;
pub type ExternFunc = source::ExternFunc<Prepared>;
pub type BuiltinFunc = source::BuiltinFunc<Prepared>;
pub type TableFunc = source::TableFunc<Prepared>;
pub type DefinedFunc = source::DefinedFunc<Prepared>;

// - Rule calls

/// A prepared relation call with its execution form.
#[derive(Clone, Debug, PartialEq)]
pub enum RuleInstr {
    /// Binds the relation's outputs and runs its continuation.
    Call(source::RuleInstr<Prepared>),
    /// Returns the relation's outputs through the tail-call dispatch loop.
    Tail(RuleTailInstr),
}

/// A relation call whose outputs conclude the enclosing relation.
#[derive(Clone, Debug, PartialEq)]
pub struct RuleTailInstr {
    pub id: Id,
    pub exps_input: Vec<Exp>,
}
