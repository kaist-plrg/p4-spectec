//! Parenthesization of nested EL expressions
//!
//! Identifiers below are shown without their `\mathsf` wrapper:
//!
//! ```text
//! Bin(Bin(a, Add, b), Mul, c)   -> \left(a + b\right) \cdot c
//! Bin(a, Add, Bin(b, Mul, c))   -> a + b \cdot c
//! Bin(a, Sub, Bin(b, Sub, c))   -> a - \left(b - c\right)
//! Bin(Bin(a, Sub, b), Sub, c)   -> a - b - c
//! ```

use crate::lang::{
    common::{
        notation::atom::Atom,
        prim::{bool, num},
    },
    el::ast::{BinOp, CmpOp},
};

// == Precedence model
//
//   Bin(a, Add, b)   -> Prec { category: Additive, assoc: Left }

/// Binding strength of an expression, from weakest to strongest.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub(super) enum Category {
    Implication,
    Disjunction,
    Conjunction,
    Turnstile,
    Tilesturn,
    SquigglyArrow,
    Colon,
    Comparison,
    Cons,
    Arrow,
    Semicolon,
    Dot,
    Additive,
    Multiplicative,
    Unary,
    Sequence,
    Power,
    Postfix,
    Atomic,
}

/// Grouping of adjacent operators in the same category.
///
/// ```text
/// Left    a - b - c     groups as  (a - b) - c
/// Right   a => b => c   groups as  a => (b => c)
/// Non     a |- b |- c   parenthesizes either side
/// ```
#[derive(Clone, Copy)]
pub(super) enum Assoc {
    Left,
    Right,
    Non,
}

/// Category and associativity of an operator.
#[derive(Clone, Copy)]
pub(super) struct Prec {
    pub(super) category: Category,
    pub(super) assoc: Assoc,
}

impl Prec {
    const fn new(category: Category, assoc: Assoc) -> Self {
        Self { category, assoc }
    }
}

// == Parenthesization
//
//   needs_parentheses(of_binop(Sub), Right, Additive)   -> true    a - (b - c)
//   needs_parentheses(of_binop(Sub), Left, Additive)    -> false   a - b - c
//   needs_parentheses(of_binop(Mul), Left, Additive)    -> true    (a + b) * c

/// Operand position under a parent operator.
#[derive(Clone, Copy)]
pub(super) enum Side {
    Left,
    Right,
}

/// Determines whether an operand needs parentheses under its parent operator.
pub(super) fn needs_parentheses(prec_parent: Prec, side: Side, category_child: Category) -> bool {
    // A weaker operand always needs parentheses
    if category_child < prec_parent.category {
        return true;
    }
    // A stronger operand never needs them
    if category_child > prec_parent.category {
        return false;
    }
    // An equal operand needs them only against the associativity
    matches!(
        (prec_parent.assoc, side),
        (Assoc::Left, Side::Right) | (Assoc::Right, Side::Left) | (Assoc::Non, _)
    )
}

// == Operator precedence

// - Notation atoms
//
//   of_infix(Turnstile)   -> Prec { category: Turnstile, assoc: Non }
//   of_infix(Arrow)       -> Prec { category: Arrow, assoc: Right }

/// Returns the precedence of a notation atom used as an infix operator.
pub(super) fn of_infix(atom: &Atom) -> Prec {
    use Assoc as A;
    use Category as C;
    match atom {
        Atom::DoubleArrowSub | Atom::DoubleArrowLong => Prec::new(C::Implication, A::Right),
        Atom::Turnstile => Prec::new(C::Turnstile, A::Non),
        Atom::Tilesturn => Prec::new(C::Tilesturn, A::Non),
        Atom::SqArrow | Atom::SqArrowStar => Prec::new(C::SquigglyArrow, A::Right),
        Atom::Sub | Atom::Sup | Atom::Colon | Atom::ColonEq | Atom::Tilde2 => {
            Prec::new(C::Colon, A::Left)
        }
        Atom::Arrow | Atom::ArrowSub => Prec::new(C::Arrow, A::Right),
        Atom::Semicolon => Prec::new(C::Semicolon, A::Left),
        Atom::Dot | Atom::Dot2 | Atom::Dot3 => Prec::new(C::Dot, A::Left),
        Atom::Backslash => Prec::new(C::Multiplicative, A::Left),
        Atom::Keyword(_)
        | Atom::Tag(_)
        | Atom::Operator(_)
        | Atom::LAngle
        | Atom::RAngle
        | Atom::LParen
        | Atom::RParen
        | Atom::LBrack
        | Atom::RBrack
        | Atom::LBrace
        | Atom::RBrace => Prec::new(C::Colon, A::Non),
    }
}

// - Operators
//
//   of_binop(Add)   -> Prec { category: Additive, assoc: Left }
//   of_cmpop(Lt)    -> COMPARISON

/// Returns the precedence of a binary operator.
pub(super) fn of_binop(op: BinOp) -> Prec {
    use Assoc as A;
    use Category as C;
    match op {
        BinOp::Bool(bool::BinOp::Impl | bool::BinOp::Equiv) => Prec::new(C::Implication, A::Right),
        BinOp::Bool(bool::BinOp::Or) => Prec::new(C::Disjunction, A::Left),
        BinOp::Bool(bool::BinOp::And) => Prec::new(C::Conjunction, A::Left),
        BinOp::Num(num::BinOp::Add | num::BinOp::Sub) => Prec::new(C::Additive, A::Left),
        BinOp::Num(num::BinOp::Mul | num::BinOp::Div | num::BinOp::Mod) => {
            Prec::new(C::Multiplicative, A::Left)
        }
        BinOp::Num(num::BinOp::Pow) => Prec::new(C::Power, A::Left),
    }
}

/// Returns the precedence shared by every comparison operator.
pub(super) fn of_cmpop(_op: CmpOp) -> Prec {
    COMPARISON
}

// - Expression forms
//
//   x :: xs          -> CONS
//   xs[i], x.a, x*   -> POSTFIX

/// Comparisons and membership tests.
pub(super) const COMPARISON: Prec = Prec::new(Category::Comparison, Assoc::Right);
/// List construction, `x :: xs`.
pub(super) const CONS: Prec = Prec::new(Category::Cons, Assoc::Right);
/// List concatenation, `xs ++ ys`.
pub(super) const CAT: Prec = Prec::new(Category::Additive, Assoc::Left);
/// Subtype tests, `e <: t`.
pub(super) const SUBTYPE: Prec = Prec::new(Category::Colon, Assoc::Left);
/// Prefix operators and negated relation premises.
pub(super) const UNARY: Prec = Prec::new(Category::Unary, Assoc::Right);
/// Juxtaposed notation terms.
pub(super) const SEQUENCE: Prec = Prec::new(Category::Sequence, Assoc::Left);
/// Indexing, slicing, field access, updates, and iterations.
pub(super) const POSTFIX: Prec = Prec::new(Category::Postfix, Assoc::Left);
