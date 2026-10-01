//! PL syntax instantiated with execution slots
//!
//! Preparation retains PL expression forms and every prose annotation.
//! Identifiers and iterations resolve to the same slots as AL and SL;
//! evaluation removes expression hints at the shared evaluator boundary.

use crate::lang::data::var::{IdSlot, VarSlot};

use crate::lang::pl::ast as source;

pub use crate::interp::shared::prepare::ast::*;

pub use source::{Fallthrough, RelSignature, TierInstr};

// == Prepared syntax

// - Expressions

pub type Exp = source::Exp<IdSlot, VarSlot>;
pub type ExpKind = source::ExpKind<IdSlot, VarSlot>;
pub type NotExp = source::NotExp<IdSlot, VarSlot>;

// - Paths

pub type Path = source::Path<IdSlot, VarSlot>;
pub type PathKind = source::PathKind<IdSlot, VarSlot>;

// - Arguments

pub type Arg = source::Arg<IdSlot, VarSlot>;
pub type ArgKind = source::ArgKind<IdSlot, VarSlot>;

// - Parameters

pub type Param = source::Param<Exp>;
pub type ParamKind = source::ParamKind<Exp>;

// - Holding conditions

pub type HoldCase<Tier> = source::HoldCase<Tier, Exp, VarSlot>;

// - Case analysis

pub type Guard = source::Guard<Exp>;
pub type Case<Tier> = source::Case<Tier, Exp, VarSlot>;

// - Instructions

pub type Instr<Tier> = source::Instr<Tier, Exp, VarSlot>;
pub type InstrKind<Tier> = source::InstrKind<Tier, Exp, VarSlot>;
pub type IfInstr<Tier> = source::IfInstr<Tier, Exp, VarSlot>;
pub type HoldInstr<Tier> = source::HoldInstr<Tier, Exp, VarSlot>;
pub type CaseInstr<Tier> = source::CaseInstr<Tier, Exp, VarSlot>;
pub type LetInstr = source::LetInstr<Exp, VarSlot>;
pub type DebugInstr = source::DebugInstr<Exp>;
pub type DestructInstr = source::DestructInstr<Exp>;
pub type CheckLetSubInstr<Tier> = source::CheckLetSubInstr<Tier, Exp, VarSlot>;
pub type CheckLetMatchInstr<Tier> = source::CheckLetMatchInstr<Tier, Exp, VarSlot>;
pub type OptionGetInstr<Tier> = source::OptionGetInstr<Tier, Exp, VarSlot>;
pub type InstrIter = PremIter;

// - Blocks

pub type Block<Tier> = source::Block<Tier, Exp, VarSlot>;
pub type GroupBlock = source::GroupBlock<Exp, VarSlot>;
pub type DispatchBlock = source::DispatchBlock<Exp, VarSlot>;

// - Group-body tier

pub type GroupInstr = source::GroupInstr<Exp, VarSlot>;
pub type ResultInstr = source::ResultInstr<Exp>;
pub type ReturnInstr = source::ReturnInstr<Exp>;
pub type RuleInstr = source::RuleInstr<Exp, VarSlot>;
pub type BacktrackInstr = source::BacktrackInstr<Exp, VarSlot>;

// - Dispatch tier

pub type DispatchInstr = source::DispatchInstr<Exp, VarSlot>;
pub type RuleGroupInstr = source::RuleGroupInstr<Exp, VarSlot>;
pub type RouteInstr = source::RouteInstr<Exp, VarSlot>;

// - Table rows

pub type TableRow = source::TableRow<Exp, VarSlot>;

// - Relation definitions

pub type RelDef = source::RelDef<Exp, VarSlot>;
pub type ExternRel = source::ExternRel<Exp>;
pub type DefinedRel = source::DefinedRel<Exp, VarSlot>;

// - Meta-function definitions

pub type MetaFuncDef = source::MetaFuncDef<Exp, VarSlot>;
pub type ExternFunc = source::ExternFunc<Exp>;
pub type BuiltinFunc = source::BuiltinFunc<Exp>;
pub type TableFunc = source::TableFunc<Exp, VarSlot>;
pub type DefinedFunc = source::DefinedFunc<Exp, VarSlot>;
