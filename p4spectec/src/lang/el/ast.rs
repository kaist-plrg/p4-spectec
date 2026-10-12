//! Elaboration language model
//!
//! Every node is a `Phrase` carrying its source span.
//! Variant docs give the concrete syntax each form is parsed from.

use crate::lang::common::{self, notation::atom, prim::num, source::Phrase};

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

// Iterators

/// An iteration marker, `?` or `*`.
pub type Iter = common::Iter;

// Types

/// A type usable for variables and parameters.
pub type PlainTyp = Phrase<PlainTypKind>;

/// The forms of a plain type.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum PlainTypKind {
    /// `bool`
    Bool,
    /// `numtyp`
    Num(num::Typ),
    /// `text`
    Text,
    /// `id (`<` list(targ, `,`) `>`)?`
    Var(Id, Vec<Targ>),
    /// `(` plain_typ `)`
    Paren(Box<PlainTyp>),
    /// `(` list(plain_typ, `,`) `)`
    Tuple(Vec<PlainTyp>),
    /// `plain_typ iter`
    Iter(Box<PlainTyp>, Iter),
}

// Operators

/// How a numeric literal was written.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum NumOp {
    /// Decimal, `42`.
    Dec,
    /// Hexadecimal, `0x2A`.
    Hex,
}

/// A unary operator, on booleans or numbers.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum UnOp {
    Bool(crate::lang::common::prim::bool::UnOp),
    Num(num::UnOp),
}

/// A binary operator, on booleans or numbers.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum BinOp {
    Bool(crate::lang::common::prim::bool::BinOp),
    Num(num::BinOp),
}

/// A comparison operator, on booleans or numbers.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum CmpOp {
    Bool(crate::lang::common::prim::bool::CmpOp),
    Num(num::CmpOp),
}

/// A fuse operator with its span.
pub type FuseOp = Phrase<FuseOpKind>;

/// The form of a fuse operator.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum FuseOpKind {
    /// `#`
    Fuse,
}

// Expressions

/// An expression with its span.
pub type Exp = Phrase<ExpKind>;

/// The forms of an expression, including notation and hint-only forms.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ExpKind {
    /// `bool`
    Bool(bool),
    /// `num`
    Num(NumOp, Num),
    /// `text`
    Text(Text),
    /// `id`
    Id(Id),
    /// `unop exp`
    Un(UnOp, Box<Exp>),
    /// `exp binop exp`
    Bin(Box<Exp>, BinOp, Box<Exp>),
    /// `exp cmpop exp`
    Cmp(Box<Exp>, CmpOp, Box<Exp>),
    /// `$(` exp `)`
    Arith(Box<Exp>),
    /// `eps`
    Eps,
    /// `[` list(exp, `,`) `]`
    List(Vec<Exp>),
    /// `exp :: exp`
    Cons(Box<Exp>, Box<Exp>),
    /// `exp ++ exp`
    Cat(Box<Exp>, Box<Exp>),
    /// `exp [` exp `]`
    Idx(Box<Exp>, Box<Exp>),
    /// `exp [` exp `:` exp `]`
    Slice(Box<Exp>, Box<Exp>, Box<Exp>),
    /// `|` exp `|`
    Len(Box<Exp>),
    /// `exp <- exp`
    Mem(Box<Exp>, Box<Exp>),
    /// `{` list(atom exp, `,`) `}`
    Str(Vec<(Atom, Exp)>),
    /// `exp . atom`
    Dot(Box<Exp>, Atom),
    /// `exp [` path `=` exp `]`
    Upd(Box<Exp>, Path, Box<Exp>),
    /// `(` exp `)`
    Paren(Box<Exp>),
    /// `(` list2(exp, `,`) `)`
    Tuple(Vec<Exp>),
    /// `$defid (<targ, ...>)? ((arg, ...))?`
    Call(Id, Vec<Targ>, Vec<Arg>),
    /// `exp iter`
    Iter(Box<Exp>, Iter),
    /// `exp <: typ`
    Sub(Box<Exp>, PlainTyp),
    // Notation expressions
    /// `atom`
    Atom(Atom),
    /// `list(exp, ` `)`
    Seq(Vec<Exp>),
    /// `exp atom exp`
    Infix(Box<Exp>, Atom, Box<Exp>),
    /// ``[({` exp `})]``
    Brack(Atom, Box<Exp>, Atom),
    // Hint expressions
    /// `%N` or `%` or `%%` or `!%`
    Hole(Hole),
    /// `exp # exp`
    Fuse(Box<Exp>, FuseOp, Box<Exp>),
    /// `## exp`
    Unparen(Box<Exp>),
    /// `latex (` `"..."`* `)`
    Latex(String),
}

/// A placeholder in a hint expression.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Hole {
    /// `%N`, the N-th argument.
    Num(usize),
    /// `%`, the next argument.
    Next,
    /// `%%`, all remaining arguments.
    Rest,
    /// `!%`, no argument.
    None,
}

// Paths

/// A path into a value, for updates.
pub type Path = Phrase<PathKind>;

/// The steps of a path, from the root outward.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum PathKind {
    /// The value itself.
    Root,
    /// `path [` exp `]`
    Idx(Box<Path>, Box<Exp>),
    /// `path [` exp `:` exp `]`
    Slice(Box<Path>, Box<Exp>, Box<Exp>),
    /// `path . atom`
    Dot(Box<Path>, Atom),
}

// Arguments

/// A call argument with its span.
pub type Arg = Phrase<ArgKind>;

/// An argument: a value expression or a function name.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ArgKind {
    /// `exp`
    Exp(Box<Exp>),
    /// `$id`
    Def(Id),
}

// Type arguments

/// A type argument.
pub type Targ = Phrase<TargKind>;
/// Type arguments are plain types.
pub type TargKind = PlainTypKind;

// Hints

/// A `hint(id exp)` annotation on a definition.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Hint {
    /// The hint's name, such as `input` or `show`.
    pub id: Id,
    /// The hint's body, read by the pass that owns the hint.
    pub exp: Exp,
}

// Notation types

/// A type in a definition: plain, or a notation with atoms.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Typ {
    /// A plain type.
    Plain(PlainTyp),
    /// A notation type, such as `C |- e : t`.
    Notation(NotTyp),
}

/// A notation type with its span.
pub type NotTyp = Phrase<NotTypKind>;

/// The forms of a notation type, mirroring notation expressions.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum NotTypKind {
    /// A bare atom.
    Atom(Atom),
    /// A sequence of types.
    Seq(Vec<Typ>),
    /// Two types around an atom.
    Infix(Box<Typ>, Atom, Box<Typ>),
    /// A type between bracket atoms.
    Brack(Atom, Box<Typ>, Atom),
}

/// The body of a type definition.
pub type DefTyp = Phrase<DefTypKind>;

/// An alias, a struct of fields, or a variant of cases.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum DefTypKind {
    /// `syntax t = u`, an alias.
    Plain(PlainTyp),
    /// `syntax t = { a u, ... }`, a struct.
    Struct(Vec<TypField>),
    /// `syntax t = | c1 | c2 ...`, a variant.
    Variant(Vec<TypCase>),
}

/// One field of a struct type.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypField {
    pub atom: Atom,
    pub typ: PlainTyp,
    pub hints: Vec<Hint>,
}

/// One case of a variant type.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypCase {
    pub typ: Typ,
    pub hints: Vec<Hint>,
}

// Parameters and premises

/// A function parameter with its span.
pub type Param = Phrase<ParamKind>;

/// A parameter: a value of a type, or a function with its own signature.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ParamKind {
    /// `typ`, a value parameter.
    Exp(PlainTyp),
    /// `def $id<tparams>(params) : typ`, a function parameter.
    Def(Id, Vec<TParam>, Vec<Param>, PlainTyp),
}

/// A type parameter name.
pub type TParam = Id;

/// A premise with its span.
pub type Prem = Phrase<PremKind>;

/// `var id : typ`, declares a rule-local variable.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct VarPrem {
    pub id: Id,
    pub plain_typ: PlainTyp,
}

/// `id: exp`, the relation `id` derives `exp`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RulePrem {
    pub id: Id,
    pub exp: Exp,
}

/// `id:/ exp`, the relation `id` does not derive `exp`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RuleNotPrem {
    pub id: Id,
    pub exp: Exp,
}

/// `if exp`, a boolean side condition.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct IfPrem {
    pub exp: Exp,
}

/// `(prem)*` or `(prem)?`, a premise under an iteration.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct IterPrem {
    pub prem: Box<Prem>,
    pub iter: Iter,
}

/// `debug exp`, prints the expression when evaluated.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DebugPrem {
    pub exp: Exp,
}

/// The forms of a premise; `Else` marks an otherwise rule.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum PremKind {
    Var(VarPrem),
    Rule(RulePrem),
    RuleNot(RuleNotPrem),
    If(IfPrem),
    Else,
    Iter(IterPrem),
    Debug(DebugPrem),
}

// Rules and tables

/// A rule with its span.
pub type Rule = Phrase<RuleKind>;

/// `rule rel/rule: exp -- prems`, one rule of a relation.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RuleKind {
    pub id_rel: Id,
    pub id_rule: Id,
    pub exp: Exp,
    pub prems: Vec<Prem>,
}

/// A table row with its span.
pub type TableRow = Phrase<TableRowKind>;

/// `pattern => body`, one row of a table function.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TableRowKind {
    pub exp_pattern: Exp,
    pub exp_body: Exp,
}

// Definitions

/// A top-level definition with its span.
pub type Def = Phrase<DefKind>;

/// `extern syntax id`, a type defined outside the specification.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ExternSyntaxDef {
    pub id: Id,
    pub hints: Vec<Hint>,
}

/// `syntax id1, id2, ...`, forward declarations of types.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SyntaxDef {
    pub entries: Vec<SyntaxDefEntry>,
}

/// One declared type name with its type parameters.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SyntaxDefEntry {
    pub id: Id,
    pub tparams: Vec<TParam>,
}

/// `syntax id<tparams> = deftyp`, a type definition.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypDef {
    pub id: Id,
    pub tparams: Vec<TParam>,
    pub def_typ: DefTyp,
    pub hints: Vec<Hint>,
}

/// `var id : typ`, a meta-variable naming a type.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct VarDef {
    pub id: Id,
    pub plain_typ: PlainTyp,
    pub hints: Vec<Hint>,
}

/// `extern relation id : nottyp`, a relation provided by the host.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ExternRelDef {
    pub id: Id,
    pub not_typ: NotTyp,
    pub hints: Vec<Hint>,
}

/// `relation id : nottyp`, declares a relation's notation type.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RelDef {
    pub id: Id,
    pub not_typ: NotTyp,
    pub hints: Vec<Hint>,
}

/// `rule rel/group ...`, the rules of one group of a relation.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RuleGroupDef {
    pub relid: Id,
    pub groupid: Id,
    pub rules: Vec<Rule>,
}

/// `extern dec $id ... : typ`, a function provided by the host.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ExternDecDef {
    pub id: Id,
    pub tparams: Vec<TParam>,
    pub params: Vec<Param>,
    pub plain_typ: PlainTyp,
    pub hints: Vec<Hint>,
}

/// `builtin dec $id ... : typ`, a function provided by the interpreter.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BuiltinDecDef {
    pub id: Id,
    pub tparams: Vec<TParam>,
    pub params: Vec<Param>,
    pub plain_typ: PlainTyp,
    pub hints: Vec<Hint>,
}

/// `table dec $id(params) : typ`, declares a table function.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TableDecDef {
    pub id: Id,
    pub params: Vec<Param>,
    pub plain_typ: PlainTyp,
    pub hints: Vec<Hint>,
}

/// `dec $id<tparams>(params) : typ`, declares a function.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FuncDecDef {
    pub id: Id,
    pub tparams: Vec<TParam>,
    pub params: Vec<Param>,
    pub plain_typ: PlainTyp,
    pub hints: Vec<Hint>,
}

/// `table $id ...`, the rows of a table function.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TableDef {
    pub id: Id,
    pub rows: Vec<TableRow>,
}

/// `def $id<tparams>(args) = exp -- prems`, one clause of a function.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FuncDef {
    pub id: Id,
    pub tparams: Vec<TParam>,
    pub args: Vec<Arg>,
    pub exp: Exp,
    pub prems: Vec<Prem>,
}

/// The forms of a definition; `Sep` is a blank line kept for printing.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum DefKind {
    ExternSyntax(ExternSyntaxDef),
    Syntax(SyntaxDef),
    Typ(TypDef),
    Var(VarDef),
    ExternRel(ExternRelDef),
    Rel(RelDef),
    RuleGroup(RuleGroupDef),
    ExternDec(ExternDecDef),
    BuiltinDec(BuiltinDecDef),
    TableDec(TableDecDef),
    FuncDec(FuncDecDef),
    TableDef(TableDef),
    FuncDef(FuncDef),
    Sep,
}

/// A whole specification: its definitions in source order.
pub type Spec = Vec<Def>;
