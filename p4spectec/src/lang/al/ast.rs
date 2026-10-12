//! Algorithmic language model
//!
//! Everything below the premises is re-exported from IL;
//! AL adds `let` premises, rule matches and paths, and clause and table forms
//! whose arguments are patterns.
//! The stage parameter `P` (`stage::Stage`) lets the interpreter
//! instantiate names with slots.

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
pub type ExpField<P = Source> = il::ast::ExpField<P>;
pub type ExpKind<P = Source> = il::ast::ExpKind<P>;
pub type NotExp<P = Source> = il::ast::NotExp<P>;
pub type ExpIter<V = Var> = il::ast::ExpIter<V>;

// Patterns

pub type Pattern<P = Source> = il::ast::Pattern<P>;

// Path

pub type Path<P = Source> = il::ast::Path<P>;
pub type PathKind<P = Source> = il::ast::PathKind<P>;

// Parameters

pub type Param = il::ast::Param;
pub type ParamKind = il::ast::ParamKind;

// Type parameters

pub type TParam = il::ast::TParam;

// Arguments

pub type Arg<P = Source> = il::ast::Arg<P>;
pub type ArgKind<P = Source> = il::ast::ArgKind<P>;

// Type arguments

pub type Targ = il::ast::Targ;
pub type TargKind = il::ast::TargKind;

// Premises

/// A premise with its span.
pub type Prem<P = Source> = Phrase<PremKind<P>>;

/// The relation `id` derives `not_exp`; the hint marks the input arguments.
#[derive(Clone, Debug, PartialEq)]
pub struct RulePrem<P: Stage = Source> {
    pub id: Id,
    pub not_exp: NotExp<P>,
    pub input_hint: InputHint,
}

/// A boolean side condition.
#[derive(Clone, Debug, PartialEq)]
pub struct IfPrem<P: Stage = Source> {
    pub exp: Exp<P>,
}

/// The relation applies to `not_exp`, without binding its outputs.
#[derive(Clone, Debug, PartialEq)]
pub struct IfHoldPrem<P: Stage = Source> {
    pub id: Id,
    pub not_exp: NotExp<P>,
}

/// The relation does not apply to `not_exp`.
#[derive(Clone, Debug, PartialEq)]
pub struct IfNotHoldPrem<P: Stage = Source> {
    pub id: Id,
    pub not_exp: NotExp<P>,
}

/// Binds the pattern `exp_l` to the value of `exp_r`.
#[derive(Clone, Debug, PartialEq)]
pub struct LetPrem<P: Stage = Source> {
    pub exp_l: Exp<P>,
    pub exp_r: Exp<P>,
}

/// A premise repeated under an iteration.
#[derive(Clone, Debug, PartialEq)]
pub struct IterPrem<P: Stage = Source> {
    pub prem: Box<Prem<P>>,
    pub prem_iter: PremIter<P::Var>,
}

/// Prints the expression when evaluated.
#[derive(Clone, Debug, PartialEq)]
pub struct DebugPrem<P: Stage = Source> {
    pub exp: Exp<P>,
}

/// The forms of a premise.
#[derive(Clone, Debug, PartialEq)]
#[allow(clippy::large_enum_variant)]
pub enum PremKind<P: Stage = Source> {
    /// `id : notexp`
    Rule(RulePrem<P>),
    /// `if exp`
    If(IfPrem<P>),
    /// `if id : notexp holds`
    IfHold(IfHoldPrem<P>),
    /// `if id : notexp does not hold`
    IfNotHold(IfNotHoldPrem<P>),
    /// `let exp = exp`
    Let(LetPrem<P>),
    /// `prem iterprem`
    Iter(IterPrem<P>),
    /// `debug exp`
    Debug(DebugPrem<P>),
}

/// An iteration over a premise, as in IL.
pub type PremIter<V = Var> = il::ast::PremIter<V>;

// Rules

/// The part of a rule group shared by its paths: inputs and common premises.
#[derive(Clone, Debug, PartialEq)]
pub struct RuleMatch<P: Stage = Source> {
    /// The notation arguments as written, for printing.
    pub exps_signature: Vec<Exp<P>>,
    /// Patterns the inputs are matched against.
    pub exps_input: Vec<Exp<P>>,
    /// Premises every path must pass first.
    pub prems: Vec<Prem<P>>,
}

/// One way a rule group can conclude: its own premises and outputs.
#[derive(Clone, Debug, PartialEq)]
pub struct RulePath<P: Stage = Source> {
    pub id: Id,
    pub prems: Vec<Prem<P>>,
    pub exps_output: Vec<Exp<P>>,
}

/// A rule group with its span.
pub type RuleGroup<P = Source> = Phrase<RuleGroupKind<P>>;
/// The rules sharing one name, as a match and its paths.
#[derive(Clone, Debug, PartialEq)]
pub struct RuleGroupKind<P: Stage = Source> {
    pub id: Id,
    pub rule_match: RuleMatch<P>,
    pub rule_paths: Vec<RulePath<P>>,
}

/// An otherwise group with its span.
pub type ElseGroup<P = Source> = Phrase<ElseGroupKind<P>>;
/// The otherwise rule of a relation: a match with a single path.
#[derive(Clone, Debug, PartialEq)]
pub struct ElseGroupKind<P: Stage = Source> {
    pub id: Id,
    pub rule_match: RuleMatch<P>,
    pub rule_path: RulePath<P>,
}

// Clauses

/// A function clause with its span.
pub type Clause<P = Source> = Phrase<ClauseKind<P>>;

/// One clause: argument patterns, body, and premises.
#[derive(Clone, Debug, PartialEq)]
pub struct ClauseKind<P: Stage = Source> {
    pub args: Vec<Arg<P>>,
    pub exp: Exp<P>,
    pub prems: Vec<Prem<P>>,
}

/// The otherwise clause of a function.
pub type ElseClause<P = Source> = Clause<P>;
/// The form of an otherwise clause.
pub type ElseClauseKind<P = Source> = ClauseKind<P>;

// Table rows

/// A table row with its span.
pub type TableRow<P = Source> = Phrase<TableRowKind<P>>;
/// One row: its signature as written, argument patterns, body, and premises.
#[derive(Clone, Debug, PartialEq)]
pub struct TableRowKind<P: Stage = Source> {
    pub exps_signature: Vec<Exp<P>>,
    pub args: Vec<Arg<P>>,
    pub exp: Exp<P>,
    pub prems: Vec<Prem<P>>,
}

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
    Extern(Box<ExternRel>),
    /// `relation id : not_typ hint(input %int*) rulegroup* hint*`
    Defined(Box<DefinedRel<P>>),
}

/// A relation provided by the host.
#[derive(Clone, Debug, PartialEq)]
pub struct ExternRel {
    pub id: Id,
    pub not_typ: NotTyp,
    pub input_hint: InputHint,
    pub hints: Vec<Hint>,
}

/// A relation with its rule groups and optional otherwise group.
#[derive(Clone, Debug, PartialEq)]
pub struct DefinedRel<P: Stage = Source> {
    pub id: Id,
    pub not_typ: NotTyp,
    pub input_hint: InputHint,
    pub rule_groups: Vec<RuleGroup<P>>,
    pub else_group: Option<ElseGroup<P>>,
    pub hints: Vec<Hint>,
}

// Meta-functions

/// A function definition: extern, builtin, table, or defined.
#[derive(Clone, Debug, PartialEq)]
pub enum MetaFuncDef<P: Stage = Source> {
    /// `extern dec id <` list(tparam, `,`) `> list(param, `,`) : typ hint*`
    Extern(ExternFunc),
    /// `builtin dec id <` list(tparam, `,`) `> list(param, `,`) : typ hint*`
    Builtin(BuiltinFunc),
    /// `table dec id list(param, `,`) : typ hint*`
    Table(TableFunc<P>),
    /// `dec id <` list(tparam, `,`) `> list(param, `,`) : typ clause* hint*`
    Defined(Box<DefinedFunc<P>>),
}

/// A function provided by the host.
#[derive(Clone, Debug, PartialEq)]
pub struct ExternFunc {
    pub id: Id,
    pub tparams: Vec<TParam>,
    pub params: Vec<Param>,
    pub typ: Typ,
    pub hints: Vec<Hint>,
}

/// A function provided by the interpreter.
#[derive(Clone, Debug, PartialEq)]
pub struct BuiltinFunc {
    pub id: Id,
    pub tparams: Vec<TParam>,
    pub params: Vec<Param>,
    pub typ: Typ,
    pub hints: Vec<Hint>,
}

/// A function defined by table rows.
#[derive(Clone, Debug, PartialEq)]
pub struct TableFunc<P: Stage = Source> {
    pub id: Id,
    pub params: Vec<Param>,
    pub typ: Typ,
    pub table_rows: Vec<TableRow<P>>,
    pub hints: Vec<Hint>,
}

/// A function with clauses and an optional otherwise clause.
#[derive(Clone, Debug, PartialEq)]
pub struct DefinedFunc<P: Stage = Source> {
    pub id: Id,
    pub tparams: Vec<TParam>,
    pub params: Vec<Param>,
    pub typ: Typ,
    pub clauses: Vec<Clause<P>>,
    pub else_clause: Option<ElseClause<P>>,
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
    /// `var id : typ hint*`
    Var(VarDef),
    /// A relation definition.
    Rel(RelDef<P>),
    /// A function definition.
    MetaFunc(MetaFuncDef<P>),
}

// Spec

/// A whole specification: its definitions in source order.
pub type Spec<P = Source> = Vec<Def<P>>;
