//! Optimization language model
//!
//! OL instructions mirror SL instructions
//! but keep `Hold` as two blocks and `Case` with a `total` flag
//! instead of SL's hold cases and dangle marks;
//! expressions, guards, and iterators are shared with SL.

use crate::lang::{common::source::Phrase, hints::input::InputHint};

pub use crate::lang::sl::ast::{Exp, ExpIter, Guard, Id, InstrIter, NotExp, RelSignature};

// == Syntax

// - Instructions

/// An instruction with its source span.
pub type Instr = Phrase<InstrKind>;

/// The instruction kinds a block may contain.
#[derive(Clone, Debug, PartialEq)]
#[allow(clippy::large_enum_variant)]
pub enum InstrKind {
    If(IfInstr),
    Hold(HoldInstr),
    Case(CaseInstr),
    Group(GroupInstr),
    Let(LetInstr),
    Rule(RuleInstr),
    Result(ResultInstr),
    Return(ReturnInstr),
    Debug(DebugInstr),
}

/// Runs `block` when `exp` holds under the iterations.
#[derive(Clone, Debug, PartialEq)]
pub struct IfInstr {
    pub exp: Exp,
    pub iter_exps: Vec<ExpIter>,
    pub block: Block,
}

/// Branches on whether relation `id` holds for `not_exp`.
#[derive(Clone, Debug, PartialEq)]
pub struct HoldInstr {
    pub id: Id,
    pub not_exp: NotExp,
    pub iter_exps: Vec<ExpIter>,
    pub block_hold: Block,
    pub block_not_hold: Block,
}

/// Case analysis on `exp`.
#[derive(Clone, Debug, PartialEq)]
pub struct CaseInstr {
    pub exp: Exp,
    pub cases: Vec<Case>,
    /// Whether the guards cover every constructor of the scrutinee type.
    pub total: bool,
}

/// A rule group matching `exps` against the relation signature.
#[derive(Clone, Debug, PartialEq)]
pub struct GroupInstr {
    pub id: Id,
    pub rel_signature: RelSignature,
    pub exps: Vec<Exp>,
    pub block: Block,
}

/// Binds `exp_l` to `exp_r`, then runs `block`.
#[derive(Clone, Debug, PartialEq)]
pub struct LetInstr {
    pub exp_l: Exp,
    pub exp_r: Exp,
    pub iter_instrs: Vec<InstrIter>,
    pub block: Block,
}

/// Calls relation `id`, binding its outputs, then runs `block`.
#[derive(Clone, Debug, PartialEq)]
pub struct RuleInstr {
    pub id: Id,
    pub not_exp: NotExp,
    pub input_hint: InputHint,
    pub iter_instrs: Vec<InstrIter>,
    pub block: Block,
}

/// Yields the outputs of a relation.
#[derive(Clone, Debug, PartialEq)]
pub struct ResultInstr {
    pub rel_signature: RelSignature,
    pub exps: Vec<Exp>,
}

/// Returns the value of a function.
#[derive(Clone, Debug, PartialEq)]
pub struct ReturnInstr {
    pub exp: Exp,
}

/// Prints `exp` before running `instr`.
#[derive(Clone, Debug, PartialEq)]
pub struct DebugInstr {
    pub exp: Exp,
    pub instr: Box<Instr>,
}

// - Case analysis

/// One guarded branch of a case analysis.
#[derive(Clone, Debug, PartialEq)]
pub struct Case {
    pub guard: Guard,
    pub block: Block,
}

// - Blocks

/// An ordered sequence of instructions.
pub type Block = Vec<Instr>;
/// The fallback block run when the main block falls through.
pub type ElseBlock = Vec<Instr>;
