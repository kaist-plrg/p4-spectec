//! PL syntax instantiated with execution slots
//!
//! Preparation retains PL expression forms and every prose annotation.
//! Identifiers and iterations resolve to the same slots as AL and SL;
//! evaluation removes expression hints at the shared evaluator boundary.

pub use crate::lang::il::prepared::*;

use super::ast as source;

pub use source::{Fallthrough, RelSignature, TierInstr};

// == Prepared syntax

// - Expressions

pub type Exp = source::Exp<Prepared>;
pub type ExpKind = source::ExpKind<Prepared>;
pub type NotExp = source::NotExp<Prepared>;

// - Paths

pub type Path = source::Path<Prepared>;
pub type PathKind = source::PathKind<Prepared>;

// - Arguments

pub type Arg = source::Arg<Prepared>;
pub type ArgKind = source::ArgKind<Prepared>;

// - Parameters

pub type Param = source::Param<Prepared>;
pub type ParamKind = source::ParamKind<Prepared>;

// - Holding conditions

pub type HoldCase<Tier> = source::HoldCase<Tier, Prepared>;

// - Case analysis

pub type Guard = source::Guard<Prepared>;
pub type Case<Tier> = source::Case<Tier, Prepared>;

// - Instructions

pub type Instr<Tier> = source::Instr<Tier, Prepared>;
pub type InstrKind<Tier> = source::InstrKind<Tier, Prepared>;
pub type IfInstr<Tier> = source::IfInstr<Tier, Prepared>;
pub type HoldInstr<Tier> = source::HoldInstr<Tier, Prepared>;
pub type CaseInstr<Tier> = source::CaseInstr<Tier, Prepared>;
pub type LetInstr = source::LetInstr<Prepared>;
pub type DebugInstr = source::DebugInstr<Prepared>;
pub type DestructInstr = source::DestructInstr<Prepared>;
pub type CheckLetSubInstr<Tier> = source::CheckLetSubInstr<Tier, Prepared>;
pub type CheckLetMatchInstr<Tier> = source::CheckLetMatchInstr<Tier, Prepared>;
pub type OptionGetInstr<Tier> = source::OptionGetInstr<Tier, Prepared>;
pub type InstrIter = PremIter;

// - Blocks

pub type Block<Tier> = source::Block<Tier, Prepared>;
pub type GroupBlock = source::GroupBlock<Prepared>;
pub type DispatchBlock = source::DispatchBlock<Prepared>;

// - Group-body tier

pub type GroupInstr = source::GroupInstr<Prepared>;
pub type ResultInstr = source::ResultInstr<Prepared>;
pub type ReturnInstr = source::ReturnInstr<Prepared>;
pub type RuleInstr = source::RuleInstr<Prepared>;
pub type BacktrackInstr = source::BacktrackInstr<Prepared>;

// - Dispatch tier

pub type DispatchInstr = source::DispatchInstr<Prepared>;
pub type RuleGroupInstr = source::RuleGroupInstr<Prepared>;
pub type RouteInstr = source::RouteInstr<Prepared>;

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
