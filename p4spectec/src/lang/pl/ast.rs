//! Prose language model
//!
//! Types, values, operators, and patterns are re-exported from SL.
//! Expressions, instructions, and definitions carry prose hints.
//! Instructions are generic over a `Tier`, the instruction kind it alone has:
//! `DispatchInstr` selects a rule group, `GroupInstr` runs its body.
//! The stage parameter `P` (`stage::Stage`) resolves identifiers,
//! variables, and mixops for the interpreter.
//! Expressions and their containing syntax share the same stage.

use std::rc::Rc;

use crate::lang::{
    common::source::{NotePhrase, Phrase},
    data::notation::Mixfix,
};

use crate::lang::sl;

use crate::lang::pl::annot;

pub use super::stage::{Source, Stage};

// Numbers

pub type Num = sl::ast::Num;

// Texts

pub type Text = sl::ast::Text;

// Identifiers

pub type Id = sl::ast::Id;

// Atoms

pub type Atom = sl::ast::Atom;

// Mixfix operators

pub type Mixop = sl::ast::Mixop;

// Iterators

pub type Iter = sl::ast::Iter;

// Variables

pub type Var = sl::ast::Var;

// Types

pub type Typ = sl::ast::Typ;
pub type TypKind = sl::ast::TypKind;
pub type NotTyp = sl::ast::NotTyp;
pub type NotTypKind = sl::ast::NotTypKind;
pub type DefTyp = sl::ast::DefTyp;
pub type DefTypKind = sl::ast::DefTypKind;
pub type TypField = sl::ast::TypField;
pub type TypCase = sl::ast::TypCase;

// Values

pub type Value = sl::ast::Value;

// Operators

pub type UnOp = sl::ast::UnOp;
pub type BinOp = sl::ast::BinOp;
pub type CmpOp = sl::ast::CmpOp;
pub type OpTyp = sl::ast::OpTyp;

// Expressions

/// A typed expression before annotation, sharing its SL type.
pub type ExpNode<P = Source> = NotePhrase<ExpKind<P>, Rc<TypKind>>;
/// A typed expression with prose hints.
pub type Exp<P = Source> = annot::Annotated<ExpNode<P>>;
#[derive(Clone, Debug, PartialEq)]
/// The forms of an expression, as in SL.
pub enum ExpKind<P: Stage = Source> {
    Bool(bool),
    Num(Num),
    Text(Text),
    Id(P::Id),
    Un(UnOp, OpTyp, Box<Exp<P>>),
    Bin(BinOp, OpTyp, Box<Exp<P>>, Box<Exp<P>>),
    Cmp(CmpOp, OpTyp, Box<Exp<P>>, Box<Exp<P>>),
    UpCast(Typ, Box<Exp<P>>),
    DownCast(Typ, Box<Exp<P>>),
    Sub(Box<Exp<P>>, P::SubTarget),
    Match(Box<Exp<P>>, Pattern<P>),
    Tuple(Vec<Exp<P>>),
    Case(Box<NotExp<P>>),
    Str(Vec<(Atom, Exp<P>)>),
    Opt(Option<Box<Exp<P>>>),
    List(Vec<Exp<P>>),
    Cons(Box<Exp<P>>, Box<Exp<P>>),
    Cat(Box<Exp<P>>, Box<Exp<P>>),
    Mem(Box<Exp<P>>, Box<Exp<P>>),
    Len(Box<Exp<P>>),
    Dot(Box<Exp<P>>, Atom),
    Idx(Box<Exp<P>>, Box<Exp<P>>),
    Slice(Box<Exp<P>>, Box<Exp<P>>, Box<Exp<P>>),
    Upd(Box<Exp<P>>, Box<Path<P>>, Box<Exp<P>>),
    Call(Id, Vec<Targ>, Vec<Arg<P>>),
    Iter(Box<Exp<P>>, ExpIter<P::Var>),
}
/// A notation expression: a mixfix skeleton with expressions as arguments.
pub type NotExp<P = Source> = Mixfix<<P as Stage>::Mixop, Exp<P>>;
pub type ExpIter<V = Var> = sl::ast::ExpIter<V>;

// Patterns

pub type Pattern<P = Source> = sl::ast::Pattern<P>;

// Path

/// A typed path into a value, for updates, sharing its SL type.
pub type Path<P = Source> = NotePhrase<PathKind<P>, Rc<TypKind>>;
#[derive(Clone, Debug, PartialEq)]
/// The steps of a path, from the root outward.
pub enum PathKind<P: Stage = Source> {
    Root,
    Idx(Box<Path<P>>, Box<Exp<P>>),
    Slice(Box<Path<P>>, Box<Exp<P>>, Box<Exp<P>>),
    Dot(Box<Path<P>>, Atom),
}

// Type parameters

pub type TParam = sl::ast::TParam;

// Parameters

/// A function parameter with its span.
pub type Param<P = Source> = Phrase<ParamKind<P>>;
#[derive(Clone, Debug, PartialEq)]
/// A parameter: a typed pattern, or a function with its own signature.
pub enum ParamKind<P: Stage = Source> {
    Exp(Typ, Box<Exp<P>>),
    Def(Id, Vec<TParam>, Vec<Param<P>>, Typ),
}

// Type arguments

pub type Targ = sl::ast::Targ;

// Arguments

/// A call argument with its span.
pub type Arg<P = Source> = Phrase<ArgKind<P>>;
#[derive(Clone, Debug, PartialEq)]
/// An argument: a value expression or a function name.
pub enum ArgKind<P: Stage = Source> {
    Exp(Box<Exp<P>>),
    Def(Id),
}

// Dangling

pub type Dangle = sl::ast::Dangle;

// Holding conditions

#[derive(Clone, Debug, PartialEq)]
/// Which branches a hold instruction has: both, or one that may dangle.
pub enum HoldCase<Tier, P: Stage = Source> {
    /// The holds branch, then the does-not-hold branch.
    Both(Block<Tier, P>, Block<Tier, P>),
    /// Only the holds branch.
    Hold(Block<Tier, P>, Dangle),
    /// Only the does-not-hold branch.
    NotHold(Block<Tier, P>, Dangle),
}

// Case analysis

pub type Totality = sl::ast::Totality;

#[derive(Clone, Debug, PartialEq)]
/// One arm of a case analysis: a guard and its block.
pub struct Case<Tier, P: Stage = Source> {
    pub guard: Guard<P>,
    pub block: Block<Tier, P>,
}

#[derive(Clone, Debug, PartialEq)]
/// A case test with an optional binding encoded by its guard kind.
pub enum Guard<P: Stage = Source> {
    /// The scrutinee is this boolean.
    Bool(bool),
    /// The scrutinee compares so with the expression.
    Cmp(CmpOp, OpTyp, Exp<P>),
    /// The scrutinee has the type.
    Sub(P::SubTarget),
    /// The scrutinee has the type and binds the expression.
    LetSub(P::SubBind, Exp<P>),
    /// The scrutinee matches the pattern.
    Match(Pattern<P>),
    /// The scrutinee matches the pattern and binds the expression.
    LetMatch(Pattern<P>, Exp<P>),
    /// The scrutinee is an element of the list.
    Mem(Exp<P>),
}

// Instructions

/// An instruction before prose annotation.
pub type InstrNode<Tier, P = Source> = Phrase<InstrKind<Tier, P>>;
/// An instruction with prose hints.
pub type Instr<Tier, P = Source> = annot::Annotated<InstrNode<Tier, P>>;

#[derive(Clone, Debug, PartialEq)]
/// The control flow shared by both tiers, plus the tier's own instruction.
pub enum InstrKind<Tier, P: Stage = Source> {
    /// Run the block if a condition holds.
    If(IfInstr<Tier, P>),
    /// Test a subtype, bind the value, then run the block.
    IfLetSub(IfLetSubInstr<Tier, P>),
    /// Test a pattern, bind the value, then run the block.
    IfLetMatch(IfLetMatchInstr<Tier, P>),
    /// Require a condition when no continuation can recover from its failure.
    Check(CheckInstr<Tier, P>),
    /// Run a branch by whether a relation applies.
    Hold(HoldInstr<Tier, P>),
    /// Run the first arm whose guard accepts.
    Case(CaseInstr<Tier, P>),
    /// Bind a pattern.
    Let(LetInstr<P>),
    /// Print an expression.
    Debug(DebugInstr<P>),
    /// Shorthand: bind several fields of one value at once.
    Destruct(DestructInstr<P>),
    /// The tier's own instruction.
    Tier(TierInstr<Tier>),
}

#[derive(Clone, Debug, PartialEq)]
/// Run the block when the condition holds under its iterations.
pub struct IfInstr<Tier, P: Stage = Source> {
    pub exp: Exp<P>,
    pub iter_exps: Vec<ExpIter<P::Var>>,
    pub block: Block<Tier, P>,
    pub dangle: Dangle,
}
#[derive(Clone, Debug, PartialEq)]
/// Bind a value with the required subtype and run the block.
pub struct IfLetSubInstr<Tier, P: Stage = Source> {
    pub target: P::SubBind,
    pub exp_l: Exp<P>,
    pub exp_r: Exp<P>,
    pub block: Block<Tier, P>,
    pub dangle: Dangle,
}
#[derive(Clone, Debug, PartialEq)]
/// Bind a value matching the pattern and run the block.
pub struct IfLetMatchInstr<Tier, P: Stage = Source> {
    pub pattern: Pattern<P>,
    pub exp_l: Exp<P>,
    pub exp_r: Exp<P>,
    pub block: Block<Tier, P>,
    pub dangle: Dangle,
}
#[derive(Clone, Debug, PartialEq)]
/// A condition required to reach the conclusion, with bindings scoped to its block.
pub struct CheckInstr<Tier, P: Stage = Source> {
    pub exp: Exp<P>,
    pub iter_exps: Vec<ExpIter<P::Var>>,
    pub block: Block<Tier, P>,
}
#[derive(Clone, Debug, PartialEq)]
/// Run a branch by whether the relation applies under its iterations.
pub struct HoldInstr<Tier, P: Stage = Source> {
    pub id: Id,
    pub not_exp: NotExp<P>,
    pub iter_exps: Vec<ExpIter<P::Var>>,
    pub hold_case: HoldCase<Tier, P>,
}
#[derive(Clone, Debug, PartialEq)]
/// Case analysis on an expression.
pub struct CaseInstr<Tier, P: Stage = Source> {
    pub exp: Exp<P>,
    pub cases: Vec<Case<Tier, P>>,
    pub dangle: Dangle,
    pub totality: Totality,
}
#[derive(Clone, Debug, PartialEq)]
/// Bind `exp_l` to `exp_r` under the iterations.
pub struct LetInstr<P: Stage = Source> {
    pub exp_l: Exp<P>,
    pub exp_r: Exp<P>,
    pub iter_instrs: Vec<InstrIter<P::Var>>,
}
#[derive(Clone, Debug, PartialEq)]
/// Print the expression.
pub struct DebugInstr<P: Stage = Source> {
    pub exp: Exp<P>,
}
#[derive(Clone, Debug, PartialEq)]
/// Bind each field expression, named when shown, from `exp`.
pub struct DestructInstr<P: Stage = Source> {
    pub bindings: Vec<(Option<String>, Exp<P>)>,
    pub exp: Exp<P>,
}
#[derive(Clone, Debug, PartialEq)]
/// The tier-specific instruction.
pub struct TierInstr<Tier> {
    pub tier: Tier,
}

/// Instructions run in order.
#[derive(Clone, Debug, PartialEq)]
pub struct Block<Tier, P: Stage = Source> {
    pub instrs: Vec<Instr<Tier, P>>,
}
pub type InstrIter<V = Var> = sl::ast::InstrIter<V>;

// Relations

pub type RelSignature = sl::ast::RelSignature;

// Group-body tier

#[derive(Clone, Debug, PartialEq)]
/// Instructions of a rule group's body.
pub enum GroupInstr<P: Stage = Source> {
    /// Conclude the relation with outputs.
    Result(ResultInstr<P>),
    /// Conclude the function with a value.
    Return(ReturnInstr<P>),
    /// Call a relation and bind its outputs.
    Rule(RuleInstr<P>),
    /// Try the arms in order until one concludes.
    Backtrack(BacktrackInstr<P>),
}

#[derive(Clone, Debug, PartialEq)]
/// The relation's outputs.
pub struct ResultInstr<P: Stage = Source> {
    pub rel_signature: RelSignature,
    pub exps_output: Vec<Exp<P>>,
}
#[derive(Clone, Debug, PartialEq)]
/// The function's result.
pub struct ReturnInstr<P: Stage = Source> {
    pub exp: Exp<P>,
}
#[derive(Clone, Debug, PartialEq)]
/// A relation call under its iterations; the hint marks the input arguments.
pub struct RuleInstr<P: Stage = Source> {
    pub id: Id,
    pub not_exp: NotExp<P>,
    pub input_hint: crate::lang::hints::input::InputHint,
    pub iter_instrs: Vec<InstrIter<P::Var>>,
}
#[derive(Clone, Debug, PartialEq)]
/// Alternative blocks; the first that concludes wins.
pub struct BacktrackInstr<P: Stage = Source> {
    pub blocks: Vec<GroupBlock<P>>,
}

/// A block of the group-body tier.
pub type GroupBlock<P = Source> = Block<GroupInstr<P>, P>;

// Dispatch tier

#[derive(Clone, Debug, PartialEq)]
#[allow(clippy::large_enum_variant)]
/// Instructions of a relation's dispatch: which group runs.
pub enum DispatchInstr<P: Stage = Source> {
    /// Match the inputs against a rule group and run its body.
    Group(RuleGroupInstr<P>),
    /// Try alternative dispatch blocks in order.
    Route(RouteInstr<P>),
}

#[derive(Clone, Debug, PartialEq)]
/// One rule group: its relation, name, input patterns, and body.
pub struct RuleGroupInstr<P: Stage = Source> {
    pub id_rel: Id,
    pub id_group: Id,
    pub rel_signature: RelSignature,
    pub exps_input: Vec<Exp<P>>,
    pub block: GroupBlock<P>,
}
#[derive(Clone, Debug, PartialEq)]
/// Alternative dispatch blocks; the first that concludes wins.
pub struct RouteInstr<P: Stage = Source> {
    pub blocks: Vec<DispatchBlock<P>>,
}

/// A block of the dispatch tier.
pub type DispatchBlock<P = Source> = Block<DispatchInstr<P>, P>;

// Type definitions

#[derive(Clone, Debug, PartialEq)]
/// A type definition: extern or defined.
pub enum TypDef {
    /// `extern syntax id hint*`
    Extern(ExternTyp),
    /// `syntax id <` list(tparam, `,`) `> : typ hint*`
    Defined(Box<DefinedTyp>),
}

#[derive(Clone, Debug, PartialEq)]
/// A type defined outside the specification.
pub struct ExternTyp {
    pub id: Id,
}

#[derive(Clone, Debug, PartialEq)]
/// A type with parameters and a body.
pub struct DefinedTyp {
    pub id: Id,
    pub tparams: Vec<TParam>,
    pub def_typ: DefTyp,
}

// Meta-variables

#[derive(Clone, Debug, PartialEq)]
/// A meta-variable naming a type.
pub struct VarDef {
    pub id: Id,
    pub typ: Typ,
}

// Relations

#[derive(Clone, Debug, PartialEq)]
/// A relation definition: extern or defined.
pub enum RelDef<P: Stage = Source> {
    /// `extern relation id : not_typ hint(input %int*) hint*`
    Extern(ExternRel<P>),
    /// `relation id : not_typ hint(input %int*) rulegroup* hint*`
    Defined(P::Framed<DefinedRel<P>>),
}

#[derive(Clone, Debug, PartialEq)]
/// A relation provided by the host.
pub struct ExternRel<P: Stage = Source> {
    pub id: Id,
    pub rel_signature: RelSignature,
    pub exps_input: Vec<Exp<P>>,
}

#[derive(Clone, Debug, PartialEq)]
/// A relation as a dispatch block with an optional otherwise block.
pub struct DefinedRel<P: Stage = Source> {
    pub id: Id,
    pub rel_signature: RelSignature,
    pub exps_input: Vec<Exp<P>>,
    pub block: DispatchBlock<P>,
    pub block_else_opt: Option<DispatchBlock<P>>,
}

// Meta-functions

#[derive(Clone, Debug, PartialEq)]
/// A function definition: extern, builtin, table, or defined.
pub enum MetaFuncDef<P: Stage = Source> {
    /// `extern dec id <` list(tparam, `,`) `> list(param, `,`) : typ hint*`
    Extern(ExternFunc<P>),
    /// `builtin dec id <` list(tparam, `,`) `> list(param, `,`) : typ hint*`
    Builtin(BuiltinFunc<P>),
    /// `table dec id list(param, `,`) : typ hint*`
    Table(P::Framed<TableFunc<P>>),
    /// `dec id <` list(tparam, `,`) `> list(param, `,`) : typ clause* hint*`
    Defined(P::Framed<DefinedFunc<P>>),
}

#[derive(Clone, Debug, PartialEq)]
/// A function provided by the host.
pub struct ExternFunc<P: Stage = Source> {
    pub id: Id,
    pub tparams: Vec<TParam>,
    pub params: Vec<Param<P>>,
    pub typ: Typ,
}

#[derive(Clone, Debug, PartialEq)]
/// A function provided by the interpreter.
pub struct BuiltinFunc<P: Stage = Source> {
    pub id: Id,
    pub tparams: Vec<TParam>,
    pub params: Vec<Param<P>>,
    pub typ: Typ,
}

#[derive(Clone, Debug, PartialEq)]
/// One row: input patterns, the matched expression, and a group-body block.
pub struct TableRow<P: Stage = Source> {
    pub exps_input: Vec<Exp<P>>,
    pub exp: Exp<P>,
    pub block: GroupBlock<P>,
}

#[derive(Clone, Debug, PartialEq)]
/// A function defined by table rows.
pub struct TableFunc<P: Stage = Source> {
    pub id: Id,
    pub params: Vec<Param<P>>,
    pub typ: Typ,
    pub rows: Vec<TableRow<P>>,
}

#[derive(Clone, Debug, PartialEq)]
/// A function as a group-body block with an optional otherwise block.
pub struct DefinedFunc<P: Stage = Source> {
    pub id: Id,
    pub tparams: Vec<TParam>,
    pub params: Vec<Param<P>>,
    pub typ: Typ,
    pub block: GroupBlock<P>,
    pub block_else_opt: Option<GroupBlock<P>>,
}

// Definitions

/// A definition before annotation.
pub type DefNode<P = Source> = Phrase<DefKind<P>>;
/// A definition with prose hints.
pub type Def<P = Source> = annot::Annotated<DefNode<P>>;

#[derive(Clone, Debug, PartialEq)]
/// The forms of a definition.
pub enum DefKind<P: Stage = Source> {
    Typ(TypDef),
    Var(VarDef),
    Rel(RelDef<P>),
    MetaFunc(MetaFuncDef<P>),
}

// Spec<P>

/// A whole specification: its definitions in source order.
pub type Spec<P = Source> = Vec<Def<P>>;
