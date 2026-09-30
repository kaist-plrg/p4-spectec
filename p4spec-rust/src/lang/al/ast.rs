//! Algorithmic language model
//!
//! Everything below the premises is re-exported from IL;
//! AL adds `let` premises, rule matches and paths, and clause and table forms
//! whose arguments are patterns.
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
pub type ExpField<I = Id, V = Var> = il::ast::ExpField<I, V>;
pub type ExpKind<I = Id, V = Var> = il::ast::ExpKind<I, V>;
pub type NotExp<I = Id, V = Var> = il::ast::NotExp<I, V>;
pub type ExpIter<V = Var> = il::ast::ExpIter<V>;

// Patterns

pub type Pattern = il::ast::Pattern;

// Path

pub type Path<I = Id, V = Var> = il::ast::Path<I, V>;
pub type PathKind<I = Id, V = Var> = il::ast::PathKind<I, V>;

// Parameters

pub type Param = il::ast::Param;
pub type ParamKind = il::ast::ParamKind;

// Type parameters

pub type TParam = il::ast::TParam;

// Arguments

pub type Arg<I = Id, V = Var> = il::ast::Arg<I, V>;
pub type ArgKind<I = Id, V = Var> = il::ast::ArgKind<I, V>;

// Type arguments

pub type Targ = il::ast::Targ;
pub type TargKind = il::ast::TargKind;

// Premises

/// A premise with its span.
pub type Prem<I = Id, V = Var> = Phrase<PremKind<I, V>>;

/// The relation `id` derives `not_exp`; the hint marks the input arguments.
#[derive(Clone, Debug, PartialEq)]
pub struct RulePrem<I = Id, V = Var> {
    pub id: Id,
    pub not_exp: NotExp<I, V>,
    pub input_hint: InputHint,
}

/// A boolean side condition.
#[derive(Clone, Debug, PartialEq)]
pub struct IfPrem<I = Id, V = Var> {
    pub exp: Exp<I, V>,
}

/// The relation applies to `not_exp`, without binding its outputs.
#[derive(Clone, Debug, PartialEq)]
pub struct IfHoldPrem<I = Id, V = Var> {
    pub id: Id,
    pub not_exp: NotExp<I, V>,
}

/// The relation does not apply to `not_exp`.
#[derive(Clone, Debug, PartialEq)]
pub struct IfNotHoldPrem<I = Id, V = Var> {
    pub id: Id,
    pub not_exp: NotExp<I, V>,
}

/// Binds the pattern `exp_l` to the value of `exp_r`.
#[derive(Clone, Debug, PartialEq)]
pub struct LetPrem<I = Id, V = Var> {
    pub exp_l: Exp<I, V>,
    pub exp_r: Exp<I, V>,
}

/// A premise repeated under an iteration.
#[derive(Clone, Debug, PartialEq)]
pub struct IterPrem<I = Id, V = Var> {
    pub prem: Box<Prem<I, V>>,
    pub prem_iter: PremIter<V>,
}

/// Prints the expression when evaluated.
#[derive(Clone, Debug, PartialEq)]
pub struct DebugPrem<I = Id, V = Var> {
    pub exp: Exp<I, V>,
}

/// The forms of a premise.
#[derive(Clone, Debug, PartialEq)]
#[allow(clippy::large_enum_variant)]
pub enum PremKind<I = Id, V = Var> {
    /// `id : notexp`
    Rule(RulePrem<I, V>),
    /// `if exp`
    If(IfPrem<I, V>),
    /// `if id : notexp holds`
    IfHold(IfHoldPrem<I, V>),
    /// `if id : notexp does not hold`
    IfNotHold(IfNotHoldPrem<I, V>),
    /// `let exp = exp`
    Let(LetPrem<I, V>),
    /// `prem iterprem`
    Iter(IterPrem<I, V>),
    /// `debug exp`
    Debug(DebugPrem<I, V>),
}

/// An iteration over a premise, as in IL.
pub type PremIter<V = Var> = il::ast::PremIter<V>;

// Rules

/// The part of a rule group shared by its paths: inputs and common premises.
#[derive(Clone, Debug, PartialEq)]
pub struct RuleMatch<I = Id, V = Var> {
    /// The notation arguments as written, for printing.
    pub exps_signature: Vec<Exp<I, V>>,
    /// Patterns the inputs are matched against.
    pub exps_input: Vec<Exp<I, V>>,
    /// Premises every path must pass first.
    pub prems: Vec<Prem<I, V>>,
}

/// One way a rule group can conclude: its own premises and outputs.
#[derive(Clone, Debug, PartialEq)]
pub struct RulePath<I = Id, V = Var> {
    pub id: Id,
    pub prems: Vec<Prem<I, V>>,
    pub exps_output: Vec<Exp<I, V>>,
}

/// A rule group with its span.
pub type RuleGroup<I = Id, V = Var> = Phrase<RuleGroupKind<I, V>>;
/// The rules sharing one name, as a match and its paths.
#[derive(Clone, Debug, PartialEq)]
pub struct RuleGroupKind<I = Id, V = Var> {
    pub id: Id,
    pub rule_match: RuleMatch<I, V>,
    pub rule_paths: Vec<RulePath<I, V>>,
}

/// An otherwise group with its span.
pub type ElseGroup<I = Id, V = Var> = Phrase<ElseGroupKind<I, V>>;
/// The otherwise rule of a relation: a match with a single path.
#[derive(Clone, Debug, PartialEq)]
pub struct ElseGroupKind<I = Id, V = Var> {
    pub id: Id,
    pub rule_match: RuleMatch<I, V>,
    pub rule_path: RulePath<I, V>,
}

// Clauses

/// A function clause with its span.
pub type Clause<I = Id, V = Var> = Phrase<ClauseKind<I, V>>;

/// One clause: argument patterns, body, and premises.
#[derive(Clone, Debug, PartialEq)]
pub struct ClauseKind<I = Id, V = Var> {
    pub args: Vec<Arg<I, V>>,
    pub exp: Exp<I, V>,
    pub prems: Vec<Prem<I, V>>,
}

/// The otherwise clause of a function.
pub type ElseClause<I = Id, V = Var> = Clause<I, V>;
/// The form of an otherwise clause.
pub type ElseClauseKind<I = Id, V = Var> = ClauseKind<I, V>;

// Table rows

/// A table row with its span.
pub type TableRow<I = Id, V = Var> = Phrase<TableRowKind<I, V>>;
/// One row: its signature as written, argument patterns, body, and premises.
#[derive(Clone, Debug, PartialEq)]
pub struct TableRowKind<I = Id, V = Var> {
    pub exps_signature: Vec<Exp<I, V>>,
    pub args: Vec<Arg<I, V>>,
    pub exp: Exp<I, V>,
    pub prems: Vec<Prem<I, V>>,
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
pub enum RelDef<I = Id, V = Var> {
    /// `extern relation id : not_typ hint(input %int*) hint*`
    Extern(Box<ExternRel>),
    /// `relation id : not_typ hint(input %int*) rulegroup* hint*`
    Defined(Box<DefinedRel<I, V>>),
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
pub struct DefinedRel<I = Id, V = Var> {
    pub id: Id,
    pub not_typ: NotTyp,
    pub input_hint: InputHint,
    pub rule_groups: Vec<RuleGroup<I, V>>,
    pub else_group: Option<ElseGroup<I, V>>,
    pub hints: Vec<Hint>,
}

// Meta-functions

/// A function definition: extern, builtin, table, or defined.
#[derive(Clone, Debug, PartialEq)]
pub enum MetaFuncDef<I = Id, V = Var> {
    /// `extern dec id <` list(tparam, `,`) `> list(param, `,`) : typ hint*`
    Extern(ExternFunc),
    /// `builtin dec id <` list(tparam, `,`) `> list(param, `,`) : typ hint*`
    Builtin(BuiltinFunc),
    /// `table dec id list(param, `,`) : typ hint*`
    Table(TableFunc<I, V>),
    /// `dec id <` list(tparam, `,`) `> list(param, `,`) : typ clause* hint*`
    Defined(Box<DefinedFunc<I, V>>),
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
pub struct TableFunc<I = Id, V = Var> {
    pub id: Id,
    pub params: Vec<Param>,
    pub typ: Typ,
    pub table_rows: Vec<TableRow<I, V>>,
    pub hints: Vec<Hint>,
}

/// A function with clauses and an optional otherwise clause.
#[derive(Clone, Debug, PartialEq)]
pub struct DefinedFunc<I = Id, V = Var> {
    pub id: Id,
    pub tparams: Vec<TParam>,
    pub params: Vec<Param>,
    pub typ: Typ,
    pub clauses: Vec<Clause<I, V>>,
    pub else_clause: Option<ElseClause<I, V>>,
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
    /// `var id : typ hint*`
    Var(VarDef),
    /// A relation definition.
    Rel(RelDef<I, V>),
    /// A function definition.
    MetaFunc(MetaFuncDef<I, V>),
}

// Spec

/// A whole specification: its definitions in source order.
pub type Spec<I = Id, V = Var> = Vec<Def<I, V>>;
