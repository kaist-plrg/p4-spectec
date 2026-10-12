//! Structured language model
//!
//! Types, values, and expressions are re-exported from IL;
//! SL adds parameters with patterns, guards, and the instruction forms.
//! The stage parameter `P` (`stage::Stage`) lets the interpreter
//! instantiate names with slots and select the rule-call representation.

use crate::lang::{common::source::Phrase, hints::input::InputHint};

use crate::lang::el;

use crate::lang::il;

pub use super::stage::{Source, Stage};

// Numbers

pub type Num = il::ast::Num;

// Texts

pub type Text = il::ast::Text;

// Identifiers

pub type Id = il::ast::Id;

// Atoms

pub type Atom = il::ast::Atom;

// Mixfix operators

pub type Mixop = il::ast::Mixop;

// Iterators

pub type Iter = il::ast::Iter;

// Variables

pub type Var = il::ast::Var;

// Types

pub type Typ = il::ast::Typ;
pub type TypKind = il::ast::TypKind;

pub type NotTyp = il::ast::NotTyp;
pub type NotTypKind = il::ast::NotTypKind;

pub type DefTyp = il::ast::DefTyp;
pub type DefTypKind = il::ast::DefTypKind;

pub type TypField = il::ast::TypField;
pub type TypCase = il::ast::TypCase;

// Values

pub type Value = il::ast::Value;
pub type ValueKind = il::ast::ValueKind;

pub type ValueField = il::ast::ValueField;
pub type ValueCase = il::ast::ValueCase;

// Operators

pub type NumOp = il::ast::NumOp;
pub type UnOp = il::ast::UnOp;
pub type BinOp = il::ast::BinOp;
pub type CmpOp = il::ast::CmpOp;
pub type OpTyp = il::ast::OpTyp;

// Subtype checks

pub type Subcheck<P = Source> = il::ast::Subcheck<P>;

// Expressions

pub type Exp<P = Source> = il::ast::Exp<P>;
pub type ExpKind<P = Source> = il::ast::ExpKind<P>;

pub type NotExp<P = Source> = il::ast::NotExp<P>;
pub type ExpIter<V = Var> = il::ast::ExpIter<V>;

// Patterns

pub type Pattern<P = Source> = il::ast::Pattern<P>;

// Path

pub type Path<P = Source> = il::ast::Path<P>;
pub type PathKind<P = Source> = il::ast::PathKind<P>;

// Type parameters

pub type TParam = il::ast::TParam;

// Parameters

/// A function parameter with its span.
pub type Param<P = Source> = Phrase<ParamKind<P>>;

/// A parameter: a typed pattern, or a function with its own signature.
#[derive(Clone, Debug, PartialEq)]
pub enum ParamKind<P: Stage = Source> {
    /// A value parameter: its type and the pattern it binds.
    Exp(Typ, Box<Exp<P>>),
    /// A function parameter with its signature.
    Def(Id, Vec<TParam>, Vec<Param<P>>, Typ),
}

// Type arguments

pub type Targ = il::ast::Targ;
pub type TargKind = il::ast::TargKind;

// Arguments

pub type Arg<P = Source> = il::ast::Arg<P>;
pub type ArgKind<P = Source> = il::ast::ArgKind<P>;

// Dangling

/// Whether a branch with no otherwise block may fall through.
pub type Dangle = bool;

// Holding conditions

/// Which branches a hold instruction has: both, or one that may dangle.
#[derive(Clone, Debug, PartialEq)]
pub enum HoldCase<P: Stage = Source> {
    /// A block for holds and one for does not hold.
    Both(Block<P>, Block<P>),
    /// Only the holds block.
    Hold(Block<P>, Dangle),
    /// Only the does-not-hold block.
    NotHold(Block<P>, Dangle),
}

// Case analysis

/// One arm of a case analysis: a guard and its block.
#[derive(Clone, Debug, PartialEq)]
pub struct Case<P: Stage = Source> {
    pub guard: Guard<P>,
    pub block: Block<P>,
}

/// A test on the case scrutinee.
#[derive(Clone, Debug, PartialEq)]
pub enum Guard<P: Stage = Source> {
    /// The scrutinee is this boolean.
    Bool(bool),
    /// The scrutinee compares so with the expression.
    Cmp(CmpOp, OpTyp, Exp<P>),
    /// The scrutinee has the type, by the given runtime check.
    Sub(Typ, Box<il::ast::Subcheck<P>>),
    /// The scrutinee matches the pattern.
    Match(Pattern<P>),
    /// The scrutinee is an element of the list.
    Mem(Exp<P>),
}

// Instructions

/// An instruction with its span.
pub type Instr<P = Source> = Phrase<InstrKind<P>>;

/// The forms of an instruction.
#[derive(Clone, Debug, PartialEq)]
#[allow(clippy::large_enum_variant)]
pub enum InstrKind<P: Stage = Source> {
    /// Run the block if a condition holds.
    If(IfInstr<P>),
    /// Run a branch by whether a relation applies.
    Hold(HoldInstr<P>),
    /// Run the first arm whose guard accepts.
    Case(CaseInstr<P>),
    /// Run a rule group's block against the inputs.
    Group(GroupInstr<P>),
    /// Bind a pattern, then run the block.
    Let(LetInstr<P>),
    /// Call a relation using the stage's rule-call representation.
    Rule(P::RuleInstr),
    /// Conclude the relation with outputs.
    Result(ResultInstr<P>),
    /// Conclude the function with a value.
    Return(ReturnInstr<P>),
    /// Print an expression, then run the wrapped instruction.
    Debug(DebugInstr<P>),
}

/// Run the block when the condition holds under its iterations.
#[derive(Clone, Debug, PartialEq)]
pub struct IfInstr<P: Stage = Source> {
    pub exp: Exp<P>,
    pub iter_exps: Vec<ExpIter<P::Var>>,
    pub block: Block<P>,
    pub dangle: Dangle,
}
/// Run a branch by whether the relation applies under its iterations.
#[derive(Clone, Debug, PartialEq)]
pub struct HoldInstr<P: Stage = Source> {
    pub id: Id,
    pub not_exp: NotExp<P>,
    pub iter_exps: Vec<ExpIter<P::Var>>,
    pub hold_case: HoldCase<P>,
}
/// Case analysis on an expression.
#[derive(Clone, Debug, PartialEq)]
pub struct CaseInstr<P: Stage = Source> {
    pub exp: Exp<P>,
    pub cases: Vec<Case<P>>,
    pub dangle: Dangle,
}
/// A rule group's block, entered when the inputs match `exps`.
#[derive(Clone, Debug, PartialEq)]
pub struct GroupInstr<P: Stage = Source> {
    pub id: Id,
    pub rel_signature: RelSignature,
    pub exps: Vec<Exp<P>>,
    pub block: Block<P>,
}
/// Bind `exp_l` to `exp_r` under the iterations, then run the block.
#[derive(Clone, Debug, PartialEq)]
pub struct LetInstr<P: Stage = Source> {
    pub exp_l: Exp<P>,
    pub exp_r: Exp<P>,
    pub iter_instrs: Vec<InstrIter<P::Var>>,
    pub block: Block<P>,
}
/// Call the relation, bind its outputs under the iterations, then the block.
#[derive(Clone, Debug, PartialEq)]
pub struct RuleInstr<P: Stage = Source> {
    pub id: Id,
    pub not_exp: NotExp<P>,
    pub input_hint: InputHint,
    pub iter_instrs: Vec<InstrIter<P::Var>>,
    pub block: Block<P>,
}
/// The relation's outputs.
#[derive(Clone, Debug, PartialEq)]
pub struct ResultInstr<P: Stage = Source> {
    pub rel_signature: RelSignature,
    pub exps: Vec<Exp<P>>,
}
/// The function's result.
#[derive(Clone, Debug, PartialEq)]
pub struct ReturnInstr<P: Stage = Source> {
    pub exp: Exp<P>,
}
/// Print the expression, then run the instruction.
#[derive(Clone, Debug, PartialEq)]
pub struct DebugInstr<P: Stage = Source> {
    pub exp: Exp<P>,
    pub instr: Box<Instr<P>>,
}

/// Instructions run in order.
pub type Block<P = Source> = Vec<Instr<P>>;
/// The otherwise block, run when the main block falls through.
pub type ElseBlock<P = Source> = Vec<Instr<P>>;
/// An iteration over a binding instruction, as over an IL premise.
pub type InstrIter<V = Var> = il::ast::PremIter<V>;

// Hints

/// A `hint(id exp)` annotation, unchanged from EL.
pub type Hint = el::ast::Hint;

// Type definitions

/// A type definition: extern or defined.
#[derive(Clone, Debug, PartialEq)]
pub enum TypDef {
    /// `extern syntax id hint*`
    Extern(ExternTyp),
    /// `syntax id <` list(tparam, `,`) `> : typ hint*`
    Defined(Box<DefinedTyp>),
}

/// A type defined outside the specification.
#[derive(Clone, Debug, PartialEq)]
pub struct ExternTyp {
    pub id: Id,
    pub hints: Vec<Hint>,
}

/// A type with parameters and a body.
#[derive(Clone, Debug, PartialEq)]
pub struct DefinedTyp {
    pub id: Id,
    pub tparams: Vec<TParam>,
    pub def_typ: DefTyp,
    pub hints: Vec<Hint>,
}

// Meta-variables

/// A meta-variable naming a type.
#[derive(Clone, Debug, PartialEq)]
pub struct VarDef {
    pub id: Id,
    pub typ: Typ,
    pub hints: Vec<Hint>,
}

// Relations

/// A relation definition: extern or defined.
#[derive(Clone, Debug, PartialEq)]
pub enum RelDef<P: Stage = Source> {
    /// `extern relation id : not_typ hint(input %int*) hint*`
    Extern(ExternRel<P>),
    /// `relation id : not_typ hint(input %int*) rulegroup* hint*`
    Defined(DefinedRel<P>),
}

/// A relation's notation type and input hint, `not_typ hint(input %int*)`.
#[derive(Clone, Debug, PartialEq)]
pub struct RelSignature {
    pub not_typ: NotTyp,
    pub input_hint: InputHint,
}

/// A relation provided by the host, `id : rel_signature exp* hint*`.
#[derive(Clone, Debug, PartialEq)]
pub struct ExternRel<P: Stage = Source> {
    pub id: Id,
    pub rel_signature: RelSignature,
    pub exps_input: Vec<Exp<P>>,
    pub hints: Vec<Hint>,
}

/// A relation as a block with an optional otherwise block,
/// `id : rel_signature exp* block elseblock? hint*`.
#[derive(Clone, Debug, PartialEq)]
pub struct DefinedRel<P: Stage = Source> {
    pub id: Id,
    pub rel_signature: RelSignature,
    pub exps_input: Vec<Exp<P>>,
    pub block: Block<P>,
    pub block_else: Option<ElseBlock<P>>,
    pub hints: Vec<Hint>,
}

// Meta-functions

/// A function definition: extern, builtin, table, or defined.
#[derive(Clone, Debug, PartialEq)]
pub enum MetaFuncDef<P: Stage = Source> {
    /// `extern dec id <` list(tparam, `,`) `> list(param, `,`) : typ hint*`
    Extern(ExternFunc<P>),
    /// `builtin dec id <` list(tparam, `,`) `> list(param, `,`) : typ hint*`
    Builtin(BuiltinFunc<P>),
    /// `table dec id list(param, `,`) : typ hint*`
    Table(TableFunc<P>),
    /// `dec id <` list(tparam, `,`) `> list(param, `,`) : typ clause* hint*`
    Defined(DefinedFunc<P>),
}

/// A function provided by the host, `id<tparams>(params) : typ hint*`.
#[derive(Clone, Debug, PartialEq)]
pub struct ExternFunc<P: Stage = Source> {
    pub id: Id,
    pub tparams: Vec<TParam>,
    pub params: Vec<Param<P>>,
    pub typ: Typ,
    pub hints: Vec<Hint>,
}

/// A function provided by the interpreter, `id<tparams>(params) : typ hint*`.
#[derive(Clone, Debug, PartialEq)]
pub struct BuiltinFunc<P: Stage = Source> {
    pub id: Id,
    pub tparams: Vec<TParam>,
    pub params: Vec<Param<P>>,
    pub typ: Typ,
    pub hints: Vec<Hint>,
}

/// One row, `(exps) -> exp block`: input patterns, matched expression, block.
#[derive(Clone, Debug, PartialEq)]
pub struct TableRow<P: Stage = Source> {
    pub exps_input: Vec<Exp<P>>,
    pub exp: Exp<P>,
    pub block: Block<P>,
}

/// A function defined by table rows, `id(params) : typ tablerow* hint*`.
#[derive(Clone, Debug, PartialEq)]
pub struct TableFunc<P: Stage = Source> {
    pub id: Id,
    pub params: Vec<Param<P>>,
    pub typ: Typ,
    pub table_rows: Vec<TableRow<P>>,
    pub hints: Vec<Hint>,
}

/// A function as a block with an optional otherwise block,
/// `id<tparams>(params) : typ block elseblock? hint*`.
#[derive(Clone, Debug, PartialEq)]
pub struct DefinedFunc<P: Stage = Source> {
    pub id: Id,
    pub tparams: Vec<TParam>,
    pub params: Vec<Param<P>>,
    pub typ: Typ,
    pub block: Block<P>,
    pub block_else: Option<ElseBlock<P>>,
    pub hints: Vec<Hint>,
}

// Definitions

/// A top-level definition with its span.
pub type Def<P = Source> = Phrase<DefKind<P>>;

/// The forms of a definition.
#[derive(Clone, Debug, PartialEq)]
pub enum DefKind<P: Stage = Source> {
    /// A type definition.
    Typ(TypDef),
    /// A meta-variable, `var id : typ hint*`.
    Var(VarDef),
    /// A relation definition.
    Rel(RelDef<P>),
    /// A function definition.
    MetaFunc(MetaFuncDef<P>),
}

// Spec

/// A whole specification: its definitions in source order.
pub type Spec<P = Source> = Vec<Def<P>>;
