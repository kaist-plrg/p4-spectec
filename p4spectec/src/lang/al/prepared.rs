//! AL syntax with resolved slots and interned mixops
//!
//! These aliases instantiate AL rules, premises, and definitions.
//! Expressions and supporting syntax come from prepared IL;
//! preparation operations live in `interp/al/prepare.rs`.

pub use crate::lang::il::prepared::*;

use super::ast as source;

pub use super::ast::{BuiltinFunc, DefinedTyp, ExternFunc, ExternRel, ExternTyp, TypDef, VarDef};

// == Prepared syntax

// - Premises

pub type Prem = source::Prem<Prepared>;
pub type PremKind = source::PremKind<Prepared>;
pub type RulePrem = source::RulePrem<Prepared>;
pub type IfPrem = source::IfPrem<Prepared>;
pub type IfHoldPrem = source::IfHoldPrem<Prepared>;
pub type IfNotHoldPrem = source::IfNotHoldPrem<Prepared>;
pub type LetPrem = source::LetPrem<Prepared>;
pub type IterPrem = source::IterPrem<Prepared>;
pub type DebugPrem = source::DebugPrem<Prepared>;

// - Rules

pub type RuleGroup = source::RuleGroup<Prepared>;
pub type RuleGroupKind = source::RuleGroupKind<Prepared>;
pub type ElseGroup = source::ElseGroup<Prepared>;
pub type ElseGroupKind = source::ElseGroupKind<Prepared>;
pub type RuleMatch = source::RuleMatch<Prepared>;
pub type RulePath = source::RulePath<Prepared>;

// - Clauses

pub type Clause = source::Clause<Prepared>;
pub type ClauseKind = source::ClauseKind<Prepared>;
pub type ElseClause = source::ElseClause<Prepared>;
pub type ElseClauseKind = source::ElseClauseKind<Prepared>;

// - Table rows

pub type TableRow = source::TableRow<Prepared>;
pub type TableRowKind = source::TableRowKind<Prepared>;

// - Relation definitions

pub type RelDef = source::RelDef<Prepared>;
pub type DefinedRel = source::DefinedRel<Prepared>;

// - Meta-function definitions

pub type MetaFuncDef = source::MetaFuncDef<Prepared>;
pub type TableFunc = source::TableFunc<Prepared>;
pub type DefinedFunc = source::DefinedFunc<Prepared>;

// - Definitions

pub type Def = source::Def<Prepared>;
pub type DefKind = source::DefKind<Prepared>;
pub type Spec = Vec<Def>;
