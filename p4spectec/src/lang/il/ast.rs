//! Internal language model
//!
//! Expressions and paths are `NotePhrase`s whose note is the type.
//! `ExpKind` and friends take identifier and variable parameters `I` and `V`
//! so the interpreters can instantiate them with frame slots;
//! the defaults are the plain `Id` and `Var`.

use std::rc::Rc;

use crate::lang::{
    common::{
        self,
        notation::{atom, mixfix::Mixfix, mixop},
        prim::num,
        source::{NotePhrase, Phrase},
    },
    data,
    hints::input::InputHint,
};

use crate::lang::el;

// Numbers

/// A numeric literal value.
pub type Num = num::Number;

// Texts

/// A text literal value.
pub type Text = String;

// Identifiers

/// An identifier with its span.
pub type Id = common::Id;

// Atoms

/// A notation atom with its span.
pub type Atom = Phrase<atom::Atom>;

// Mixfix operators

/// The atom skeleton of a notation form, without its arguments.
pub type Mixop = mixop::Mixop;

// Iterators

/// An iteration marker, `?` or `*`.
pub type Iter = common::Iter;

// Variables

/// A variable: identifier, type, and iteration path.
pub type Var = crate::lang::data::var::Var;

// Types

/// A type with its span.
pub type Typ = data::typ::Typ;
/// The forms of a type.
pub type TypKind = data::typ::TypKind;
/// The type of a function value.
pub type FuncTyp = data::typ::FuncTyp;

// Subtype checks

/// The runtime part of a subtype check, after static subtyping is decided.
#[derive(Clone, Debug, PartialEq)]
pub enum Subcheck {
    /// Statically a subtype: nothing to check.
    Skip,
    /// A variant value: its case must be one of these.
    Mixop(Vec<Mixop>),
    /// A tuple: check each component.
    Tuple(Vec<Subcheck>),
    /// An option or list: check each element.
    Iter(Iter, Box<Subcheck>),
    /// Fall back to full membership in this type.
    Recurse(Typ),
}

// Defined types

/// A notation type with its span.
pub type NotTyp = Phrase<NotTypKind>;
/// A notation type: a mixfix skeleton with types as arguments.
pub type NotTypKind = Mixfix<Typ>;

/// The body of a type definition.
pub type DefTyp = Phrase<DefTypKind>;

/// An alias, a struct of fields, or a variant of cases.
#[derive(Clone, Debug, PartialEq)]
pub enum DefTypKind {
    /// An alias for another type.
    Plain(Typ),
    /// A struct with named fields.
    Struct(Vec<TypField>),
    /// A variant with notation cases.
    Variant(Vec<TypCase>),
}

/// One field of a struct type.
#[derive(Clone, Debug, PartialEq)]
pub struct TypField {
    pub atom: Atom,
    pub typ: Typ,
}

/// The type a variant case was inherited from.
pub type TypOrigin = Phrase<TypOriginKind>;
/// A type name with its arguments, naming where a case came from.
#[derive(Clone, Debug, PartialEq)]
pub struct TypOriginKind {
    pub id: Id,
    pub targs: Vec<Targ>,
}

/// One case of a variant type, with the type that introduced it.
#[derive(Clone, Debug, PartialEq)]
pub struct TypCase {
    pub not_typ: NotTyp,
    pub typ_origin: TypOrigin,
    pub hints: Vec<Hint>,
}

// == Values

/// A runtime value handle.
pub type Value = data::value::Value;
/// The forms of a runtime value.
pub type ValueKind = data::value::ValueKind;
/// One field of a struct value.
pub type ValueField = data::value::ValueField;
/// A variant value: a mixfix skeleton with values as arguments.
pub type ValueCase = data::value::ValueCase;

// Operators

/// How a numeric literal was written.
pub type NumOp = el::ast::NumOp;
/// A unary operator.
pub type UnOp = el::ast::UnOp;
/// A binary operator.
pub type BinOp = el::ast::BinOp;
/// A comparison operator.
pub type CmpOp = el::ast::CmpOp;

/// The operand type an operator was resolved to.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum OpTyp {
    Bool,
    Nat,
    Int,
}

// Expressions

/// A typed expression: its form, its span, and its type as the note.
pub type Exp<I = Id, V = Var> = NotePhrase<ExpKind<I, V>, Rc<TypKind>>;

/// The forms of a typed expression.
#[derive(Clone, Debug, PartialEq)]
pub enum ExpKind<I = Id, V = Var> {
    /// `bool`
    Bool(bool),
    /// `num`
    Num(Num),
    /// `text`
    Text(Text),
    /// `varid`
    Id(I),
    /// `unop exp`
    Un(UnOp, OpTyp, Box<Exp<I, V>>),
    /// `exp binop exp`
    Bin(BinOp, OpTyp, Box<Exp<I, V>>, Box<Exp<I, V>>),
    /// `exp cmpop exp`
    Cmp(CmpOp, OpTyp, Box<Exp<I, V>>, Box<Exp<I, V>>),
    /// `exp as typ`
    UpCast(Box<Typ>, Box<Exp<I, V>>),
    /// `exp as typ`
    DownCast(Box<Typ>, Box<Exp<I, V>>),
    /// `exp <: typ`
    Sub(Box<Exp<I, V>>, Box<Typ>, Box<Subcheck>),
    /// `exp matches pattern`
    Match(Box<Exp<I, V>>, Pattern),
    /// `(` exp* `)`
    Tuple(Vec<Exp<I, V>>),
    /// `notexp`
    Case(Box<NotExp<I, V>>),
    /// `{` expfield* `}`
    Str(Vec<ExpField<I, V>>),
    /// `exp?`
    Opt(Option<Box<Exp<I, V>>>),
    /// `[` exp* `]`
    List(Vec<Exp<I, V>>),
    /// `exp :: exp`
    Cons(Box<Exp<I, V>>, Box<Exp<I, V>>),
    /// `exp ++ exp`
    Cat(Box<Exp<I, V>>, Box<Exp<I, V>>),
    /// `exp <- exp`
    Mem(Box<Exp<I, V>>, Box<Exp<I, V>>),
    /// `|` exp `|`
    Len(Box<Exp<I, V>>),
    /// `exp.atom`
    Dot(Box<Exp<I, V>>, Atom),
    /// `exp [` exp `]`
    Idx(Box<Exp<I, V>>, Box<Exp<I, V>>),
    /// `exp [` exp `:` exp `]`
    Slice(Box<Exp<I, V>>, Box<Exp<I, V>>, Box<Exp<I, V>>),
    /// `exp [` path `=` exp `]`
    Upd(Box<Exp<I, V>>, Box<Path<I, V>>, Box<Exp<I, V>>),
    /// `$id<` targ* `>(` arg* `)`
    Call(Id, Vec<Targ>, Vec<Arg<I, V>>),
    /// `exp iterexp`
    Iter(Box<Exp<I, V>>, ExpIter<V>),
}

/// A notation expression: a mixfix skeleton with expressions as arguments.
pub type NotExp<I = Id, V = Var> = Mixfix<Exp<I, V>>;

/// One field of a struct expression.
#[derive(Clone, Debug, PartialEq)]
pub struct ExpField<I = Id, V = Var> {
    pub atom: Atom,
    pub exp: Exp<I, V>,
}

/// An iteration over an expression and the variables it iterates.
#[derive(Clone, Debug, PartialEq)]
pub struct ExpIter<V = Var> {
    /// `?` or `*`.
    pub iter: Iter,
    /// Variables whose bound dimension this iteration consumes.
    pub vars: Vec<V>,
}

// Patterns

/// A shape an expression is matched against, without binding.
#[derive(Clone, Debug, PartialEq)]
pub enum Pattern {
    /// A variant case with this skeleton.
    Case(Box<Mixop>),
    /// A list of some shape.
    List(ListPattern),
    /// An option, present or absent.
    Opt(OptPattern),
}

/// The shape of a list.
#[derive(Clone, Debug, PartialEq)]
pub enum ListPattern {
    /// Non-empty.
    Cons,
    /// Exactly this many elements.
    Fixed(usize),
    /// Empty.
    Nil,
}

/// Whether an option is present.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum OptPattern {
    Some,
    None,
}

// Paths

/// A typed path into a value, for updates.
pub type Path<I = Id, V = Var> = NotePhrase<PathKind<I, V>, Rc<TypKind>>;

/// The steps of a path, from the root outward.
#[derive(Clone, Debug, PartialEq)]
pub enum PathKind<I = Id, V = Var> {
    /// The value itself.
    Root,
    /// `path [` exp `]`
    Idx(Box<Path<I, V>>, Box<Exp<I, V>>),
    /// `path [` exp `:` exp `]`
    Slice(Box<Path<I, V>>, Box<Exp<I, V>>, Box<Exp<I, V>>),
    /// `path . atom`
    Dot(Box<Path<I, V>>, Atom),
}

// Parameters

/// A function parameter with its span.
pub type Param = Phrase<ParamKind>;

/// A parameter: a value of a type, or a function with its own signature.
#[derive(Clone, Debug, PartialEq)]
pub enum ParamKind {
    /// `typ`
    Exp(Typ),
    /// `def $id (<` list(tparam, `,`) `>)? (` list(param, `,`) `)? : typ`
    Def(Id, Vec<TParam>, Vec<Param>, Typ),
}

// Type parameters

/// A type parameter name.
pub type TParam = common::TId;

// Arguments

/// A call argument with its span.
pub type Arg<I = Id, V = Var> = Phrase<ArgKind<I, V>>;

/// An argument: a value expression or a function name.
#[derive(Clone, Debug, PartialEq)]
pub enum ArgKind<I = Id, V = Var> {
    /// `exp`
    Exp(Box<Exp<I, V>>),
    /// `$id`
    Def(Id),
}

// Type arguments

/// A type argument.
pub type Targ = Typ;
/// The forms of a type argument.
pub type TargKind = TypKind;

// Premises

/// A premise with its span.
pub type Prem = Phrase<PremKind>;

/// The relation `id` derives `not_exp`; the hint marks the input arguments.
#[derive(Clone, Debug, PartialEq)]
pub struct RulePrem {
    pub id: Id,
    pub not_exp: NotExp,
    pub input_hint: InputHint,
}

/// A boolean side condition.
#[derive(Clone, Debug, PartialEq)]
pub struct IfPrem {
    pub exp: Exp,
}

/// The relation applies to `not_exp`, without binding its outputs.
#[derive(Clone, Debug, PartialEq)]
pub struct IfHoldPrem {
    pub id: Id,
    pub not_exp: NotExp,
}

/// The relation does not apply to `not_exp`.
#[derive(Clone, Debug, PartialEq)]
pub struct IfNotHoldPrem {
    pub id: Id,
    pub not_exp: NotExp,
}

/// A premise repeated under an iteration.
#[derive(Clone, Debug, PartialEq)]
pub struct IterPrem {
    pub prem: Box<Prem>,
    pub prem_iter: PremIter,
}

/// Prints the expression when evaluated.
#[derive(Clone, Debug, PartialEq)]
pub struct DebugPrem {
    pub exp: Exp,
}

/// The forms of a premise.
#[derive(Clone, Debug, PartialEq)]
#[allow(clippy::large_enum_variant)]
pub enum PremKind {
    /// `id : notexp`
    Rule(RulePrem),
    /// `if exp`
    If(IfPrem),
    /// `if id : notexp holds`
    IfHold(IfHoldPrem),
    /// `if id : notexp does not hold`
    IfNotHold(IfNotHoldPrem),
    /// `prem iterprem`
    Iter(IterPrem),
    /// `debug exp`
    Debug(DebugPrem),
}

/// An iteration over a premise, split into the variables it reads and binds.
#[derive(Clone, Debug, PartialEq)]
pub struct PremIter<V = Var> {
    /// `?` or `*`.
    pub iter: Iter,
    /// Variables already bound outside, iterated over.
    pub vars_bound: Vec<V>,
    /// Variables the premise binds, collected one dimension up.
    pub vars_bind: Vec<V>,
}

// Otherwise markers

/// An otherwise keyword with its source annotation.
pub type Otherwise = Phrase<OtherwiseKind>;

/// The keyword that marks a fallback rule or clause.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct OtherwiseKind;

// Rules

/// A rule with its span.
pub type Rule = Phrase<RuleKind>;

/// One rule: its name, conclusion, and premises.
#[derive(Clone, Debug, PartialEq)]
pub struct RuleKind {
    pub id: Id,
    pub not_exp: NotExp,
    pub prems: Vec<Prem>,
}

/// A rule group with its span.
pub type RuleGroup = Phrase<RuleGroupKind>;

/// The rules that share one name of a relation.
#[derive(Clone, Debug, PartialEq)]
pub struct RuleGroupKind {
    pub id: Id,
    pub rules: Vec<Rule>,
}

/// An otherwise group with its span.
pub type ElseGroup = Phrase<ElseGroupKind>;

/// The single otherwise rule of a relation, tried when no group matches.
#[derive(Clone, Debug, PartialEq)]
pub struct ElseGroupKind {
    pub id: Id,
    pub rule: Rule,
    /// The otherwise keyword preserved during elaboration.
    pub otherwise: Otherwise,
}

// Clauses

/// A function clause with its span.
pub type Clause = Phrase<ClauseKind>;

/// One clause: argument patterns, body, and premises.
#[derive(Clone, Debug, PartialEq)]
pub struct ClauseKind {
    pub args: Vec<Arg>,
    pub exp: Exp,
    pub prems: Vec<Prem>,
    /// The source keyword, absent in ordinary or synthesized clauses.
    pub otherwise_opt: Option<Otherwise>,
}

/// The otherwise clause of a function, tried when no clause matches.
pub type ElseClause = Clause;
/// The form of an otherwise clause.
pub type ElseClauseKind = ClauseKind;

// Table rows

/// A table row with its span.
pub type TableRow = Phrase<TableRowKind>;

/// One row: argument patterns and the body.
#[derive(Clone, Debug, PartialEq)]
pub struct TableRowKind {
    pub args: Vec<Arg>,
    pub exp: Exp,
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
pub enum RelDef {
    /// `extern relation id : not_typ hint(input %int*) hint*`
    Extern(Box<ExternRel>),
    /// `relation id : not_typ hint(input %int*) rulegroup* hint*`
    Defined(Box<DefinedRel>),
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
pub struct DefinedRel {
    pub id: Id,
    pub not_typ: NotTyp,
    pub input_hint: InputHint,
    pub rule_groups: Vec<RuleGroup>,
    pub else_group: Option<ElseGroup>,
    pub hints: Vec<Hint>,
}

// Meta-functions

/// A function definition: extern, builtin, table, or defined.
#[derive(Clone, Debug, PartialEq)]
pub enum MetaFuncDef {
    /// `extern dec id <` list(tparam, `,`) `> list(param, `,`) : typ hint*`
    Extern(ExternFunc),
    /// `builtin dec id <` list(tparam, `,`) `> list(param, `,`) : typ hint*`
    Builtin(BuiltinFunc),
    /// `table dec id list(param, `,`) : typ hint*`
    Table(TableFunc),
    /// `dec id <` list(tparam, `,`) `> list(param, `,`) : typ clause* hint*`
    Defined(Box<DefinedFunc>),
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
pub struct TableFunc {
    pub id: Id,
    pub params: Vec<Param>,
    pub typ: Typ,
    pub rows: Vec<TableRow>,
    pub hints: Vec<Hint>,
}

/// A function with clauses and an optional otherwise clause.
#[derive(Clone, Debug, PartialEq)]
pub struct DefinedFunc {
    pub id: Id,
    pub tparams: Vec<TParam>,
    pub params: Vec<Param>,
    pub typ: Typ,
    pub clauses: Vec<Clause>,
    pub else_clause: Option<ElseClause>,
    pub hints: Vec<Hint>,
}

// Definitions

/// A top-level definition with its span.
pub type Def = Phrase<DefKind>;

/// The forms of a definition.
#[derive(Clone, Debug, PartialEq)]
#[allow(clippy::large_enum_variant)]
pub enum DefKind {
    /// A type definition.
    Typ(TypDef),
    /// `var id : typ hint*`
    Var(VarDef),
    /// A relation definition.
    Rel(RelDef),
    /// A function definition.
    MetaFunc(MetaFuncDef),
}

// Spec

/// A whole specification: its definitions in source order.
pub type Spec = Vec<Def>;
