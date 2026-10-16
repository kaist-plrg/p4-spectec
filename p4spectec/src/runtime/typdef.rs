//! Runtime representation of internal-language type definitions
//!
//! A `TypeDef` is what a type name resolves to in a type environment:
//! a type parameter, an extern type,
//! or a defined type with its parameters and, once checked, its body.

use crate::lang::il::ast::{DefTyp, Source, Stage, TParam};

/// Runtime representation of a type definition.
///
/// Its body is in a syntax stage `P`: source for the passes,
/// prepared, with case shapes, for the interpreters.
#[derive(Clone, Debug, PartialEq)]
pub enum TypeDef<P: Stage = Source> {
    /// A type parameter.
    Parameter,
    /// An extern type.
    Extern,
    /// A type being defined, but not yet fully checked.
    Defining(Vec<TParam>),
    /// A fully checked defined type.
    Defined(Vec<TParam>, Box<DefTyp<P>>),
}

impl<P: Stage> TypeDef<P> {
    /// Returns the type definition's type parameters.
    pub fn tparams(&self) -> &[TParam] {
        match self {
            Self::Parameter | Self::Extern => &[],
            Self::Defining(tparams) | Self::Defined(tparams, _) => tparams,
        }
    }
}
