//! Numeric values and operations
//!
//! Numbers are naturals or integers over `BigInt`;
//! operations require both operands of the same kind
//! and stay in it, except that subtracting naturals yields an integer.
//! Naturals sort before integers and `nat <: int`.

use std::{cmp::Ordering, fmt};

use num_bigint::BigInt;
use num_traits::{Signed, Zero};
use thiserror::Error;

use crate::lang::traits::print::{Print, Printer};

// Numbers: natural numbers and integers

/// A non-negative arbitrary-precision integer
///
/// Construct with `TryFrom<BigInt>`;
/// negative inputs return `NumericError::NaturalNegative`.
#[derive(Clone, Debug, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
#[serde(try_from = "BigInt")]
pub struct Natural(BigInt);

/// A natural number or a signed integer.
#[derive(Clone, Debug, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub enum Number {
    /// A natural number.
    Nat(Natural),
    /// A signed integer.
    Int(BigInt),
}

/// The numeric types.
#[derive(
    Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize,
)]
pub enum Typ {
    Nat,
    Int,
}

/// The numeric type of a number.
pub fn to_typ(num: &Number) -> Typ {
    match num {
        Number::Nat(_) => Typ::Nat,
        Number::Int(_) => Typ::Int,
    }
}

/// Views any number as a signed integer.
pub fn to_int(num: &Number) -> &BigInt {
    match num {
        Number::Nat(nat) => nat.as_bigint(),
        Number::Int(int) => int,
    }
}

// Operations

/// Sign operators, `+` and `-`.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum UnOp {
    Plus,
    Minus,
}

/// Arithmetic, `+ - * / \ ^`; `\` is modulo.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
}

/// Order comparisons, `< > <= >=`.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum CmpOp {
    Lt,
    Gt,
    Le,
    Ge,
}

/// Errors from checked numeric construction and operations.
#[derive(Clone, Debug, Error, PartialEq, Eq)]
pub enum NumericError {
    /// A negative value was given where a natural was needed.
    #[error("natural number cannot be negative: {0}")]
    NaturalNegative(BigInt),

    /// An operation mixed a natural and an integer.
    #[error("numeric operands have mismatched kinds: {typ_l:?} and {typ_r:?}")]
    KindMismatch { typ_l: Typ, typ_r: Typ },

    /// Division or modulo by zero.
    #[error("numeric operation {0:?} has a zero divisor")]
    DivisorZero(BinOp),

    /// An operator with no implementation, currently `^`.
    #[error("unsupported numeric binary operation: {0:?}")]
    BinaryOperationUnsupported(BinOp),
}

impl Natural {
    /// Borrows the validated integer payload.
    pub fn as_bigint(&self) -> &BigInt {
        &self.0
    }
}

impl TryFrom<BigInt> for Natural {
    type Error = NumericError;

    fn try_from(int: BigInt) -> Result<Self, Self::Error> {
        if int.is_negative() { Err(NumericError::NaturalNegative(int)) } else { Ok(Self(int)) }
    }
}

impl From<u64> for Natural {
    fn from(int: u64) -> Self {
        Self(int.into())
    }
}

impl fmt::Display for Natural {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(formatter)
    }
}

// Comparison

/// Compares number kind before numeric value;
/// every natural number sorts before every signed integer.
pub fn compare(num_l: &Number, num_r: &Number) -> Ordering {
    match (num_l, num_r) {
        (Number::Nat(nat_l), Number::Nat(nat_r)) => nat_l.0.cmp(&nat_r.0),
        (Number::Int(int_l), Number::Int(int_r)) => int_l.cmp(int_r),
        (Number::Nat(_), Number::Int(_)) => Ordering::Less,
        (Number::Int(_), Number::Nat(_)) => Ordering::Greater,
    }
}

/// Orders the numeric types, naturals first.
pub fn compare_typ(typ_l: Typ, typ_r: Typ) -> Ordering {
    match (typ_l, typ_r) {
        (Typ::Nat, Typ::Nat) | (Typ::Int, Typ::Int) => Ordering::Equal,
        (Typ::Nat, Typ::Int) => Ordering::Less,
        (Typ::Int, Typ::Nat) => Ordering::Greater,
    }
}

// Equality

/// Equality of kind and value; `Nat(1)` differs from `Int(1)`.
pub fn eq(num_l: &Number, num_r: &Number) -> bool {
    compare(num_l, num_r) == Ordering::Equal
}

// Subtyping

/// Type equivalence: the same numeric type.
pub fn equiv(typ_l: Typ, typ_r: Typ) -> bool {
    typ_l == typ_r
}

/// Subtyping: `nat <: int`, plus equivalence.
pub fn sub(typ_l: Typ, typ_r: Typ) -> bool {
    matches!((typ_l, typ_r), (Typ::Nat, Typ::Int)) || equiv(typ_l, typ_r)
}

// Stringifiers

impl Print for Number {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        match self {
            Self::Nat(nat) => printer.write_fmt(format_args!("{nat}")),
            // Integers always carry a sign: `+1` is an integer, `1` a natural
            Self::Int(int) => {
                let sign = if int.is_negative() { "-" } else { "+" };
                printer.write_fmt(format_args!("{sign}{}", int.abs()))
            }
        }
    }
}

impl Print for Typ {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        printer.write(match self {
            Self::Nat => "nat",
            Self::Int => "int",
        })
    }
}

impl Print for UnOp {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        printer.write(match self {
            Self::Plus => "+",
            Self::Minus => "-",
        })
    }
}

impl Print for BinOp {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        printer.write(match self {
            Self::Add => "+",
            Self::Sub => "-",
            Self::Mul => "*",
            Self::Div => "/",
            Self::Mod => "\\",
            Self::Pow => "^",
        })
    }
}

impl Print for CmpOp {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        printer.write(match self {
            Self::Lt => "<",
            Self::Gt => ">",
            Self::Le => "<=",
            Self::Ge => ">=",
        })
    }
}

// Unary

/// Applies a sign; negation always yields an integer.
pub fn un(unop: UnOp, num: &Number) -> Number {
    match unop {
        UnOp::Plus => num.clone(),
        UnOp::Minus => Number::Int(-to_int(num)),
    }
}

// Binary

/// Applies a checked binary operation
///
/// Returns an error for mismatched kinds;
/// returns an error for zero division or modulo.
pub fn bin(binop: BinOp, number_l: &Number, number_r: &Number) -> Result<Number, NumericError> {
    match (binop, number_l, number_r) {
        // Addition and multiplication stay within the kind
        (BinOp::Add, Number::Nat(natural_l), Number::Nat(natural_r)) => {
            Ok(Number::Nat(Natural(&natural_l.0 + &natural_r.0)))
        }
        (BinOp::Add, Number::Int(integer_l), Number::Int(integer_r)) => {
            Ok(Number::Int(integer_l + integer_r))
        }
        // Subtracting naturals may go negative, so the result is an integer
        (BinOp::Sub, Number::Nat(natural_l), Number::Nat(natural_r)) => {
            Ok(Number::Int(&natural_l.0 - &natural_r.0))
        }
        // Integer subtraction stays an integer
        (BinOp::Sub, Number::Int(integer_l), Number::Int(integer_r)) => {
            Ok(Number::Int(integer_l - integer_r))
        }
        (BinOp::Mul, Number::Nat(natural_l), Number::Nat(natural_r)) => {
            Ok(Number::Nat(Natural(&natural_l.0 * &natural_r.0)))
        }
        (BinOp::Mul, Number::Int(integer_l), Number::Int(integer_r)) => {
            Ok(Number::Int(integer_l * integer_r))
        }
        // A zero divisor is an error, checked before dividing
        (binop @ (BinOp::Div | BinOp::Mod), Number::Nat(_), Number::Nat(natural_r))
            if natural_r.0.is_zero() =>
        {
            Err(NumericError::DivisorZero(binop))
        }
        (binop @ (BinOp::Div | BinOp::Mod), Number::Int(_), Number::Int(integer_r))
            if integer_r.is_zero() =>
        {
            Err(NumericError::DivisorZero(binop))
        }
        // Division and modulo stay within the kind
        (BinOp::Div, Number::Nat(natural_l), Number::Nat(natural_r)) => {
            Ok(Number::Nat(Natural(&natural_l.0 / &natural_r.0)))
        }
        (BinOp::Div, Number::Int(integer_l), Number::Int(integer_r)) => {
            Ok(Number::Int(integer_l / integer_r))
        }
        (BinOp::Mod, Number::Nat(natural_l), Number::Nat(natural_r)) => {
            Ok(Number::Nat(Natural(&natural_l.0 % &natural_r.0)))
        }
        (BinOp::Mod, Number::Int(integer_l), Number::Int(integer_r)) => {
            Ok(Number::Int(integer_l % integer_r))
        }
        // Exponentiation is not implemented
        (BinOp::Pow, Number::Nat(_), Number::Nat(_))
        | (BinOp::Pow, Number::Int(_), Number::Int(_)) => {
            Err(NumericError::BinaryOperationUnsupported(binop))
        }
        // Mixed kinds are an error
        (_, number_l, number_r) => {
            Err(NumericError::KindMismatch { typ_l: to_typ(number_l), typ_r: to_typ(number_r) })
        }
    }
}

// Comparison

/// Applies a checked comparison
///
/// Returns an error for mismatched number kinds.
pub fn cmp(cmpop: CmpOp, number_l: &Number, number_r: &Number) -> Result<bool, NumericError> {
    match (cmpop, number_l, number_r) {
        // Same-kind operands compare on their values
        (CmpOp::Lt, Number::Nat(natural_l), Number::Nat(natural_r)) => {
            Ok(natural_l.0 < natural_r.0)
        }
        (CmpOp::Lt, Number::Int(integer_l), Number::Int(integer_r)) => Ok(integer_l < integer_r),
        (CmpOp::Gt, Number::Nat(natural_l), Number::Nat(natural_r)) => {
            Ok(natural_l.0 > natural_r.0)
        }
        (CmpOp::Gt, Number::Int(integer_l), Number::Int(integer_r)) => Ok(integer_l > integer_r),
        (CmpOp::Le, Number::Nat(natural_l), Number::Nat(natural_r)) => {
            Ok(natural_l.0 <= natural_r.0)
        }
        (CmpOp::Le, Number::Int(integer_l), Number::Int(integer_r)) => Ok(integer_l <= integer_r),
        (CmpOp::Ge, Number::Nat(natural_l), Number::Nat(natural_r)) => {
            Ok(natural_l.0 >= natural_r.0)
        }
        (CmpOp::Ge, Number::Int(integer_l), Number::Int(integer_r)) => Ok(integer_l >= integer_r),
        // Mixed kinds are an error
        (_, number_l, number_r) => {
            Err(NumericError::KindMismatch { typ_l: to_typ(number_l), typ_r: to_typ(number_r) })
        }
    }
}
