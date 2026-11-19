//! Booleans
//!
//! The boolean type and its operators, printed in the specification's spelling.

use std::fmt;

use crate::lang::traits::print::{Print, Printer};

/// The boolean type.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Typ {
    Bool,
}

// Operations

/// Negation, `~`.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum UnOp {
    Not,
}

/// Connectives: `/\`, `\/`, `=>`, `<=>`.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum BinOp {
    And,
    Or,
    Impl,
    Equiv,
}

/// Equality, `=` and `=/=`.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum CmpOp {
    Eq,
    Ne,
}

// Stringifiers

impl Print for Typ {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        printer.write("bool")
    }
}

impl Print for UnOp {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        printer.write(match self {
            Self::Not => "~",
        })
    }
}

impl Print for BinOp {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        printer.write(match self {
            Self::And => "/\\",
            Self::Or => "\\/",
            Self::Impl => "=>",
            Self::Equiv => "<=>",
        })
    }
}

impl Print for CmpOp {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        printer.write(match self {
            Self::Eq => "=",
            Self::Ne => "=/=",
        })
    }
}
