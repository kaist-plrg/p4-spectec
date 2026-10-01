//! Structured language model
//!
//! Types, values, and expressions are re-exported from IL;
//! SL adds parameters with patterns, guards, and the instruction forms.
//! The `I` and `V` parameters let the interpreter instantiate names with slots.

use crate::lang::{common::source::Phrase, hints::input::InputHint};

use crate::lang::el;

use crate::lang::il;

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

pub type Subcheck = il::ast::Subcheck;

// Expressions

pub type Exp<I = Id, V = Var> = il::ast::Exp<I, V>;
pub type ExpKind<I = Id, V = Var> = il::ast::ExpKind<I, V>;

pub type NotExp<I = Id, V = Var> = il::ast::NotExp<I, V>;
pub type ExpIter<V = Var> = il::ast::ExpIter<V>;

// Patterns

pub type Pattern = il::ast::Pattern;

// Path

pub type Path<I = Id, V = Var> = il::ast::Path<I, V>;
pub type PathKind<I = Id, V = Var> = il::ast::PathKind<I, V>;

// Type parameters

pub type TParam = il::ast::TParam;

// Parameters

/// A function parameter with its span.
pub type Param<I = Id, V = Var> = Phrase<ParamKind<I, V>>;

/// A parameter: a typed pattern, or a function with its own signature.
#[derive(Clone, Debug, PartialEq)]
pub enum ParamKind<I = Id, V = Var> {
    /// A value parameter: its type and the pattern it binds.
    Exp(Typ, Box<Exp<I, V>>),
    /// A function parameter with its signature.
    Def(Id, Vec<TParam>, Vec<Param<I, V>>, Typ),
}

// Type arguments

pub type Targ = il::ast::Targ;
pub type TargKind = il::ast::TargKind;

// Arguments

pub type Arg<I = Id, V = Var> = il::ast::Arg<I, V>;
pub type ArgKind<I = Id, V = Var> = il::ast::ArgKind<I, V>;

// Dangling

/// Whether a branch with no otherwise block may fall through.
pub type Dangle = bool;

// Holding conditions

/// Which branches a hold instruction has: both, or one that may dangle.
#[derive(Clone, Debug, PartialEq)]
pub enum HoldCase<I = Id, V = Var> {
    /// A block for holds and one for does not hold.
    Both(Block<I, V>, Block<I, V>),
    /// Only the holds block.
    Hold(Block<I, V>, Dangle),
    /// Only the does-not-hold block.
    NotHold(Block<I, V>, Dangle),
}

// Case analysis

/// One arm of a case analysis: a guard and its block.
#[derive(Clone, Debug, PartialEq)]
pub struct Case<I = Id, V = Var> {
    pub guard: Guard<I, V>,
    pub block: Block<I, V>,
}

/// A test on the case scrutinee.
#[derive(Clone, Debug, PartialEq)]
pub enum Guard<I = Id, V = Var> {
    /// The scrutinee is this boolean.
    Bool(bool),
    /// The scrutinee compares so with the expression.
    Cmp(CmpOp, OpTyp, Exp<I, V>),
    /// The scrutinee has the type, by the given runtime check.
    Sub(Typ, Box<il::ast::Subcheck>),
    /// The scrutinee matches the pattern.
    Match(Pattern),
    /// The scrutinee is an element of the list.
    Mem(Exp<I, V>),
}

// Instructions

/// An instruction with its span.
pub type Instr<I = Id, V = Var> = Phrase<InstrKind<I, V>>;

/// The forms of an instruction.
#[derive(Clone, Debug, PartialEq)]
#[allow(clippy::large_enum_variant)]
pub enum InstrKind<I = Id, V = Var> {
    /// Run the block if a condition holds.
    If(IfInstr<I, V>),
    /// Run a branch by whether a relation applies.
    Hold(HoldInstr<I, V>),
    /// Run the first arm whose guard accepts.
    Case(CaseInstr<I, V>),
    /// Run a rule group's block against the inputs.
    Group(GroupInstr<I, V>),
    /// Bind a pattern, then run the block.
    Let(LetInstr<I, V>),
    /// Call a relation, bind its outputs, then run the block.
    Rule(RuleInstr<I, V>),
    /// Conclude the relation with outputs.
    Result(ResultInstr<I, V>),
    /// Conclude the function with a value.
    Return(ReturnInstr<I, V>),
    /// Print an expression, then run the wrapped instruction.
    Debug(DebugInstr<I, V>),
}

/// Run the block when the condition holds under its iterations.
#[derive(Clone, Debug, PartialEq)]
pub struct IfInstr<I = Id, V = Var> {
    pub exp: Exp<I, V>,
    pub iter_exps: Vec<ExpIter<V>>,
    pub block: Block<I, V>,
    pub dangle: Dangle,
}
/// Run a branch by whether the relation applies under its iterations.
#[derive(Clone, Debug, PartialEq)]
pub struct HoldInstr<I = Id, V = Var> {
    pub id: Id,
    pub not_exp: NotExp<I, V>,
    pub iter_exps: Vec<ExpIter<V>>,
    pub hold_case: HoldCase<I, V>,
}
/// Case analysis on an expression.
#[derive(Clone, Debug, PartialEq)]
pub struct CaseInstr<I = Id, V = Var> {
    pub exp: Exp<I, V>,
    pub cases: Vec<Case<I, V>>,
    pub dangle: Dangle,
}
/// A rule group's block, entered when the inputs match `exps`.
#[derive(Clone, Debug, PartialEq)]
pub struct GroupInstr<I = Id, V = Var> {
    pub id: Id,
    pub rel_signature: RelSignature,
    pub exps: Vec<Exp<I, V>>,
    pub block: Block<I, V>,
}
/// Bind `exp_l` to `exp_r` under the iterations, then run the block.
#[derive(Clone, Debug, PartialEq)]
pub struct LetInstr<I = Id, V = Var> {
    pub exp_l: Exp<I, V>,
    pub exp_r: Exp<I, V>,
    pub iter_instrs: Vec<InstrIter<V>>,
    pub block: Block<I, V>,
}
/// Call the relation, bind its outputs under the iterations, then the block.
#[derive(Clone, Debug, PartialEq)]
pub struct RuleInstr<I = Id, V = Var> {
    pub id: Id,
    pub not_exp: NotExp<I, V>,
    pub input_hint: InputHint,
    pub iter_instrs: Vec<InstrIter<V>>,
    pub block: Block<I, V>,
}
/// The relation's outputs.
#[derive(Clone, Debug, PartialEq)]
pub struct ResultInstr<I = Id, V = Var> {
    pub rel_signature: RelSignature,
    pub exps: Vec<Exp<I, V>>,
}
/// The function's result.
#[derive(Clone, Debug, PartialEq)]
pub struct ReturnInstr<I = Id, V = Var> {
    pub exp: Exp<I, V>,
}
/// Print the expression, then run the instruction.
#[derive(Clone, Debug, PartialEq)]
pub struct DebugInstr<I = Id, V = Var> {
    pub exp: Exp<I, V>,
    pub instr: Box<Instr<I, V>>,
}

/// Instructions run in order.
pub type Block<I = Id, V = Var> = Vec<Instr<I, V>>;
/// The otherwise block, run when the main block falls through.
pub type ElseBlock<I = Id, V = Var> = Vec<Instr<I, V>>;
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
pub enum RelDef<I = Id, V = Var> {
    /// `extern relation id : not_typ hint(input %int*) hint*`
    Extern(ExternRel<I, V>),
    /// `relation id : not_typ hint(input %int*) rulegroup* hint*`
    Defined(DefinedRel<I, V>),
}

/// A relation's notation type and input hint, `not_typ hint(input %int*)`.
#[derive(Clone, Debug, PartialEq)]
pub struct RelSignature {
    pub not_typ: NotTyp,
    pub input_hint: InputHint,
}

/// A relation provided by the host, `id : rel_signature exp* hint*`.
#[derive(Clone, Debug, PartialEq)]
pub struct ExternRel<I = Id, V = Var> {
    pub id: Id,
    pub rel_signature: RelSignature,
    pub exps_input: Vec<Exp<I, V>>,
    pub hints: Vec<Hint>,
}

/// A relation as a block with an optional otherwise block,
/// `id : rel_signature exp* block elseblock? hint*`.
#[derive(Clone, Debug, PartialEq)]
pub struct DefinedRel<I = Id, V = Var> {
    pub id: Id,
    pub rel_signature: RelSignature,
    pub exps_input: Vec<Exp<I, V>>,
    pub block: Block<I, V>,
    pub block_else: Option<ElseBlock<I, V>>,
    pub hints: Vec<Hint>,
}

// Meta-functions

/// A function definition: extern, builtin, table, or defined.
#[derive(Clone, Debug, PartialEq)]
pub enum MetaFuncDef<I = Id, V = Var> {
    /// `extern dec id <` list(tparam, `,`) `> list(param, `,`) : typ hint*`
    Extern(ExternFunc<I, V>),
    /// `builtin dec id <` list(tparam, `,`) `> list(param, `,`) : typ hint*`
    Builtin(BuiltinFunc<I, V>),
    /// `table dec id list(param, `,`) : typ hint*`
    Table(TableFunc<I, V>),
    /// `dec id <` list(tparam, `,`) `> list(param, `,`) : typ clause* hint*`
    Defined(DefinedFunc<I, V>),
}

/// A function provided by the host, `id<tparams>(params) : typ hint*`.
#[derive(Clone, Debug, PartialEq)]
pub struct ExternFunc<I = Id, V = Var> {
    pub id: Id,
    pub tparams: Vec<TParam>,
    pub params: Vec<Param<I, V>>,
    pub typ: Typ,
    pub hints: Vec<Hint>,
}

/// A function provided by the interpreter, `id<tparams>(params) : typ hint*`.
#[derive(Clone, Debug, PartialEq)]
pub struct BuiltinFunc<I = Id, V = Var> {
    pub id: Id,
    pub tparams: Vec<TParam>,
    pub params: Vec<Param<I, V>>,
    pub typ: Typ,
    pub hints: Vec<Hint>,
}

/// One row, `(exps) -> exp block`: input patterns, matched expression, block.
#[derive(Clone, Debug, PartialEq)]
pub struct TableRow<I = Id, V = Var> {
    pub exps_input: Vec<Exp<I, V>>,
    pub exp: Exp<I, V>,
    pub block: Block<I, V>,
}

/// A function defined by table rows, `id(params) : typ tablerow* hint*`.
#[derive(Clone, Debug, PartialEq)]
pub struct TableFunc<I = Id, V = Var> {
    pub id: Id,
    pub params: Vec<Param<I, V>>,
    pub typ: Typ,
    pub table_rows: Vec<TableRow<I, V>>,
    pub hints: Vec<Hint>,
}

/// A function as a block with an optional otherwise block,
/// `id<tparams>(params) : typ block elseblock? hint*`.
#[derive(Clone, Debug, PartialEq)]
pub struct DefinedFunc<I = Id, V = Var> {
    pub id: Id,
    pub tparams: Vec<TParam>,
    pub params: Vec<Param<I, V>>,
    pub typ: Typ,
    pub block: Block<I, V>,
    pub block_else: Option<ElseBlock<I, V>>,
    pub hints: Vec<Hint>,
}

// Definitions

/// A top-level definition with its span.
pub type Def<I = Id, V = Var> = Phrase<DefKind<I, V>>;

/// The forms of a definition.
#[derive(Clone, Debug, PartialEq)]
pub enum DefKind<I = Id, V = Var> {
    /// A type definition.
    Typ(TypDef),
    /// A meta-variable, `var id : typ hint*`.
    Var(VarDef),
    /// A relation definition.
    Rel(RelDef<I, V>),
    /// A function definition.
    MetaFunc(MetaFuncDef<I, V>),
}

// Spec

/// A whole specification: its definitions in source order.
pub type Spec<I = Id, V = Var> = Vec<Def<I, V>>;
