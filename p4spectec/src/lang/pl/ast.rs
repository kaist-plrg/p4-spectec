//! Prose language model
//!
//! Types, values, operators, and patterns are re-exported from SL.
//! Expressions and definitions are `Annotated` so prose hints can attach;
//! instructions carry an optional `Fallthrough` note saying where control goes
//! when they do not conclude.
//! Instructions are generic over a `Tier`, the instruction kind it alone has:
//! `DispatchInstr` selects a rule group, `GroupInstr` runs its body.
//! Expression parameters `I` and `V` resolve identifiers and variables to slots.
//! Instruction parameter `E` selects the expression representation.

use crate::lang::common::{
    notation::mixfix::Mixfix,
    source::{NotePhrase, Phrase},
};

use crate::lang::sl;

use crate::lang::pl::annot;

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

// Subtype checks

pub type Subcheck = sl::ast::Subcheck;

// Expressions

/// A typed expression before annotation.
pub type ExpNode<I = Id, V = Var> = NotePhrase<ExpKind<I, V>, TypKind>;
/// A typed expression with prose hints.
pub type Exp<I = Id, V = Var> = annot::Annotated<ExpNode<I, V>>;
#[derive(Clone, Debug, PartialEq)]
/// The forms of an expression, as in SL.
pub enum ExpKind<I = Id, V = Var> {
    Bool(bool),
    Num(Num),
    Text(Text),
    Id(I),
    Un(UnOp, OpTyp, Box<Exp<I, V>>),
    Bin(BinOp, OpTyp, Box<Exp<I, V>>, Box<Exp<I, V>>),
    Cmp(CmpOp, OpTyp, Box<Exp<I, V>>, Box<Exp<I, V>>),
    UpCast(Typ, Box<Exp<I, V>>),
    DownCast(Typ, Box<Exp<I, V>>),
    Sub(Box<Exp<I, V>>, Typ, Box<Subcheck>),
    Match(Box<Exp<I, V>>, Pattern),
    Tuple(Vec<Exp<I, V>>),
    Case(Box<NotExp<I, V>>),
    Str(Vec<(Atom, Exp<I, V>)>),
    Opt(Option<Box<Exp<I, V>>>),
    List(Vec<Exp<I, V>>),
    Cons(Box<Exp<I, V>>, Box<Exp<I, V>>),
    Cat(Box<Exp<I, V>>, Box<Exp<I, V>>),
    Mem(Box<Exp<I, V>>, Box<Exp<I, V>>),
    Len(Box<Exp<I, V>>),
    Dot(Box<Exp<I, V>>, Atom),
    Idx(Box<Exp<I, V>>, Box<Exp<I, V>>),
    Slice(Box<Exp<I, V>>, Box<Exp<I, V>>, Box<Exp<I, V>>),
    Upd(Box<Exp<I, V>>, Box<Path<I, V>>, Box<Exp<I, V>>),
    Call(Id, Vec<Targ>, Vec<Arg<I, V>>),
    Iter(Box<Exp<I, V>>, ExpIter<V>),
}
/// A notation expression: a mixfix skeleton with expressions as arguments.
pub type NotExp<I = Id, V = Var> = Mixfix<Exp<I, V>>;
pub type ExpIter<V = Var> = sl::ast::ExpIter<V>;

// Patterns

pub type Pattern = sl::ast::Pattern;

// Path

/// A typed path into a value, for updates.
pub type Path<I = Id, V = Var> = NotePhrase<PathKind<I, V>, TypKind>;
#[derive(Clone, Debug, PartialEq)]
/// The steps of a path, from the root outward.
pub enum PathKind<I = Id, V = Var> {
    Root,
    Idx(Box<Path<I, V>>, Box<Exp<I, V>>),
    Slice(Box<Path<I, V>>, Box<Exp<I, V>>, Box<Exp<I, V>>),
    Dot(Box<Path<I, V>>, Atom),
}

// Type parameters

pub type TParam = sl::ast::TParam;

// Parameters

/// A function parameter with its span.
pub type Param<E = Exp> = Phrase<ParamKind<E>>;
#[derive(Clone, Debug, PartialEq)]
/// A parameter: a typed pattern, or a function with its own signature.
pub enum ParamKind<E = Exp> {
    Exp(Typ, Box<E>),
    Def(Id, Vec<TParam>, Vec<Param<E>>, Typ),
}

// Type arguments

pub type Targ = sl::ast::Targ;

// Arguments

/// A call argument with its span.
pub type Arg<I = Id, V = Var> = Phrase<ArgKind<I, V>>;
#[derive(Clone, Debug, PartialEq)]
/// An argument: a value expression or a function name.
pub enum ArgKind<I = Id, V = Var> {
    Exp(Box<Exp<I, V>>),
    Def(Id),
}

// Dangling

pub type Dangle = sl::ast::Dangle;

// Holding conditions

#[derive(Clone, Debug, PartialEq)]
/// Which branches a hold instruction has: both, or one that may dangle.
pub enum HoldCase<Tier, E = Exp, V = Var> {
    /// The holds branch, then the does-not-hold branch.
    Both(Block<Tier, E, V>, Block<Tier, E, V>),
    /// Only the holds branch.
    Hold(Block<Tier, E, V>, Dangle),
    /// Only the does-not-hold branch.
    NotHold(Block<Tier, E, V>, Dangle),
}

// Case analysis

#[derive(Clone, Debug, PartialEq)]
/// One arm of a case analysis: a guard and its block.
pub struct Case<Tier, E = Exp, V = Var> {
    pub guard: Guard<E>,
    pub block: Block<Tier, E, V>,
}

#[derive(Clone, Debug, PartialEq)]
/// A test on the case scrutinee; the shorthands also bind it.
pub enum Guard<E = Exp> {
    /// The scrutinee is this boolean.
    Bool(bool),
    /// The scrutinee compares so with the expression.
    Cmp(CmpOp, OpTyp, E),
    /// The scrutinee has the type.
    Sub(Typ, Box<Subcheck>),
    /// The scrutinee matches the pattern.
    Match(Pattern),
    /// The scrutinee is an element of the list.
    Mem(E),
    // Shorthands
    /// Let the expression be the scrutinee, which has the type.
    CheckLetSub(Typ, Box<Subcheck>, E),
    /// Let the expression be the scrutinee, which matches the pattern.
    CheckLetMatch(Pattern, E),
}

// Instructions

#[derive(Clone, Debug, PartialEq)]
/// Where control goes when an instruction does not conclude.
pub enum Fallthrough {
    /// To the named rule group.
    Group(Id),
    /// To the next alternative.
    Next,
    /// To the otherwise block.
    Else,
    /// Nowhere: evaluation fails.
    Fail,
}

/// An instruction with its fall-through note, before annotation.
pub type InstrNode<Tier, E = Exp, V = Var> = NotePhrase<InstrKind<Tier, E, V>, Option<Fallthrough>>;
/// An instruction with prose hints.
pub type Instr<Tier, E = Exp, V = Var> = annot::Annotated<InstrNode<Tier, E, V>>;

#[derive(Clone, Debug, PartialEq)]
/// The control flow shared by both tiers, plus the tier's own instruction.
pub enum InstrKind<Tier, E = Exp, V = Var> {
    /// Run the block if a condition holds.
    If(IfInstr<Tier, E, V>),
    /// Run a branch by whether a relation applies.
    Hold(HoldInstr<Tier, E, V>),
    /// Run the first arm whose guard accepts.
    Case(CaseInstr<Tier, E, V>),
    /// Bind a pattern.
    Let(LetInstr<E, V>),
    /// Print an expression.
    Debug(DebugInstr<E>),
    /// Shorthand: bind several fields of one value at once.
    Destruct(DestructInstr<E>),
    /// Shorthand: bind after a subtype check, then run the block.
    CheckLetSub(CheckLetSubInstr<Tier, E, V>),
    /// Shorthand: bind after a pattern match, then run the block.
    CheckLetMatch(CheckLetMatchInstr<Tier, E, V>),
    /// Shorthand: bind the content of a present option, then run the block.
    OptionGet(OptionGetInstr<Tier, E, V>),
    /// The tier's own instruction.
    Tier(TierInstr<Tier>),
}

#[derive(Clone, Debug, PartialEq)]
/// Run the block when the condition holds under its iterations.
pub struct IfInstr<Tier, E = Exp, V = Var> {
    pub exp: E,
    pub iter_exps: Vec<ExpIter<V>>,
    pub block: Block<Tier, E, V>,
    pub dangle: Dangle,
}
#[derive(Clone, Debug, PartialEq)]
/// Run a branch by whether the relation applies under its iterations.
pub struct HoldInstr<Tier, E = Exp, V = Var> {
    pub id: Id,
    pub not_exp: Mixfix<E>,
    pub iter_exps: Vec<ExpIter<V>>,
    pub hold_case: HoldCase<Tier, E, V>,
}
#[derive(Clone, Debug, PartialEq)]
/// Case analysis on an expression.
pub struct CaseInstr<Tier, E = Exp, V = Var> {
    pub exp: E,
    pub cases: Vec<Case<Tier, E, V>>,
    pub dangle: Dangle,
}
#[derive(Clone, Debug, PartialEq)]
/// Bind `exp_l` to `exp_r` under the iterations.
pub struct LetInstr<E = Exp, V = Var> {
    pub exp_l: E,
    pub exp_r: E,
    pub iter_instrs: Vec<InstrIter<V>>,
}
#[derive(Clone, Debug, PartialEq)]
/// Print the expression.
pub struct DebugInstr<E = Exp> {
    pub exp: E,
}
#[derive(Clone, Debug, PartialEq)]
/// Bind each field expression, named when shown, from `exp`.
pub struct DestructInstr<E = Exp> {
    pub bindings: Vec<(Option<String>, E)>,
    pub exp: E,
}
#[derive(Clone, Debug, PartialEq)]
/// Bind `exp_l` to `exp_r` once it passes the subtype check, then the block.
pub struct CheckLetSubInstr<Tier, E = Exp, V = Var> {
    pub typ: Typ,
    pub subcheck: Box<Subcheck>,
    pub exp_l: E,
    pub exp_r: E,
    pub block: Block<Tier, E, V>,
}
#[derive(Clone, Debug, PartialEq)]
/// Bind `exp_l` to `exp_r` once it matches the pattern, then run the block.
pub struct CheckLetMatchInstr<Tier, E = Exp, V = Var> {
    pub pattern: Pattern,
    pub exp_l: E,
    pub exp_r: E,
    pub block: Block<Tier, E, V>,
}
#[derive(Clone, Debug, PartialEq)]
/// Bind `exp_l` to the content of the option `exp_r`, then run the block.
pub struct OptionGetInstr<Tier, E = Exp, V = Var> {
    pub exp_l: E,
    pub exp_r: E,
    pub block: Block<Tier, E, V>,
}
#[derive(Clone, Debug, PartialEq)]
/// The tier-specific instruction.
pub struct TierInstr<Tier> {
    pub tier: Tier,
}

/// Instructions run in order.
pub type Block<Tier, E = Exp, V = Var> = Vec<Instr<Tier, E, V>>;
pub type InstrIter<V = Var> = sl::ast::InstrIter<V>;

// Relations

pub type RelSignature = sl::ast::RelSignature;

// Group-body tier

#[derive(Clone, Debug, PartialEq)]
/// Instructions of a rule group's body.
pub enum GroupInstr<E = Exp, V = Var> {
    /// Conclude the relation with outputs.
    Result(ResultInstr<E>),
    /// Conclude the function with a value.
    Return(ReturnInstr<E>),
    /// Call a relation and bind its outputs.
    Rule(RuleInstr<E, V>),
    /// Try the arms in order until one concludes.
    Backtrack(BacktrackInstr<E, V>),
}

#[derive(Clone, Debug, PartialEq)]
/// The relation's outputs.
pub struct ResultInstr<E = Exp> {
    pub rel_signature: RelSignature,
    pub exps_output: Vec<E>,
}
#[derive(Clone, Debug, PartialEq)]
/// The function's result.
pub struct ReturnInstr<E = Exp> {
    pub exp: E,
}
#[derive(Clone, Debug, PartialEq)]
/// A relation call under its iterations; the hint marks the input arguments.
pub struct RuleInstr<E = Exp, V = Var> {
    pub id: Id,
    pub not_exp: Mixfix<E>,
    pub input_hint: crate::lang::hints::input::InputHint,
    pub iter_instrs: Vec<InstrIter<V>>,
}
#[derive(Clone, Debug, PartialEq)]
/// Alternative blocks; the first that concludes wins.
pub struct BacktrackInstr<E = Exp, V = Var> {
    pub blocks: Vec<GroupBlock<E, V>>,
}

/// A block of the group-body tier.
pub type GroupBlock<E = Exp, V = Var> = Block<GroupInstr<E, V>, E, V>;

// Dispatch tier

#[derive(Clone, Debug, PartialEq)]
#[allow(clippy::large_enum_variant)]
/// Instructions of a relation's dispatch: which group runs.
pub enum DispatchInstr<E = Exp, V = Var> {
    /// Match the inputs against a rule group and run its body.
    Group(RuleGroupInstr<E, V>),
    /// Try alternative dispatch blocks in order.
    Route(RouteInstr<E, V>),
}

#[derive(Clone, Debug, PartialEq)]
/// One rule group: its relation, name, input patterns, and body.
pub struct RuleGroupInstr<E = Exp, V = Var> {
    pub id_rel: Id,
    pub id_group: Id,
    pub rel_signature: RelSignature,
    pub exps_input: Vec<E>,
    pub block: GroupBlock<E, V>,
}
#[derive(Clone, Debug, PartialEq)]
/// Alternative dispatch blocks; the first that concludes wins.
pub struct RouteInstr<E = Exp, V = Var> {
    pub blocks: Vec<DispatchBlock<E, V>>,
}

/// A block of the dispatch tier.
pub type DispatchBlock<E = Exp, V = Var> = Block<DispatchInstr<E, V>, E, V>;

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
pub enum RelDef<E = Exp, V = Var> {
    /// `extern relation id : not_typ hint(input %int*) hint*`
    Extern(ExternRel<E>),
    /// `relation id : not_typ hint(input %int*) rulegroup* hint*`
    Defined(DefinedRel<E, V>),
}

#[derive(Clone, Debug, PartialEq)]
/// A relation provided by the host.
pub struct ExternRel<E = Exp> {
    pub id: Id,
    pub rel_signature: RelSignature,
    pub exps_input: Vec<E>,
}

#[derive(Clone, Debug, PartialEq)]
/// A relation as a dispatch block with an optional otherwise block.
pub struct DefinedRel<E = Exp, V = Var> {
    pub id: Id,
    pub rel_signature: RelSignature,
    pub exps_input: Vec<E>,
    pub block: DispatchBlock<E, V>,
    pub block_else_opt: Option<DispatchBlock<E, V>>,
}

// Meta-functions

#[derive(Clone, Debug, PartialEq)]
/// A function definition: extern, builtin, table, or defined.
pub enum MetaFuncDef<E = Exp, V = Var> {
    /// `extern dec id <` list(tparam, `,`) `> list(param, `,`) : typ hint*`
    Extern(ExternFunc<E>),
    /// `builtin dec id <` list(tparam, `,`) `> list(param, `,`) : typ hint*`
    Builtin(BuiltinFunc<E>),
    /// `table dec id list(param, `,`) : typ hint*`
    Table(TableFunc<E, V>),
    /// `dec id <` list(tparam, `,`) `> list(param, `,`) : typ clause* hint*`
    Defined(DefinedFunc<E, V>),
}

#[derive(Clone, Debug, PartialEq)]
/// A function provided by the host.
pub struct ExternFunc<E = Exp> {
    pub id: Id,
    pub tparams: Vec<TParam>,
    pub params: Vec<Param<E>>,
    pub typ: Typ,
}

#[derive(Clone, Debug, PartialEq)]
/// A function provided by the interpreter.
pub struct BuiltinFunc<E = Exp> {
    pub id: Id,
    pub tparams: Vec<TParam>,
    pub params: Vec<Param<E>>,
    pub typ: Typ,
}

#[derive(Clone, Debug, PartialEq)]
/// One row: input patterns, the matched expression, and a group-body block.
pub struct TableRow<E = Exp, V = Var> {
    pub exps_input: Vec<E>,
    pub exp: E,
    pub block: GroupBlock<E, V>,
}

#[derive(Clone, Debug, PartialEq)]
/// A function defined by table rows.
pub struct TableFunc<E = Exp, V = Var> {
    pub id: Id,
    pub params: Vec<Param<E>>,
    pub typ: Typ,
    pub rows: Vec<TableRow<E, V>>,
}

#[derive(Clone, Debug, PartialEq)]
/// A function as a group-body block with an optional otherwise block.
pub struct DefinedFunc<E = Exp, V = Var> {
    pub id: Id,
    pub tparams: Vec<TParam>,
    pub params: Vec<Param<E>>,
    pub typ: Typ,
    pub block: GroupBlock<E, V>,
    pub block_else_opt: Option<GroupBlock<E, V>>,
}

// Definitions

/// A definition before annotation.
pub type DefNode<E = Exp, V = Var> = Phrase<DefKind<E, V>>;
/// A definition with prose hints.
pub type Def<E = Exp, V = Var> = annot::Annotated<DefNode<E, V>>;

#[derive(Clone, Debug, PartialEq)]
/// The forms of a definition.
pub enum DefKind<E = Exp, V = Var> {
    Typ(TypDef),
    Var(VarDef),
    Rel(RelDef<E, V>),
    MetaFunc(MetaFuncDef<E, V>),
}

// Spec<E, V>

/// A whole specification: its definitions in source order.
pub type Spec<E = Exp, V = Var> = Vec<Def<E, V>>;
