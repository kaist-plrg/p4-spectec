//! Stages of IL syntax: what differs between source and prepared forms
//!
//! A `Stage` chooses identifier and variable occurrences,
//! how a notation's mixop is held,
//! and whether a subtype test holds its type or its runtime check.
//! `Source` keeps names and shares mixop trees, as elaboration and the passes
//! produce and rewrite them;
//! the interpreters prepare syntax into a stage with frame slots and
//! interned shapes.

use std::{fmt, rc::Rc};

use crate::lang::{
    common::Id,
    data::{
        notation::{flat, tree},
        typ::Typ,
        var::{IdSlot, Var, VarSlot},
    },
};

use super::ast::Subcheck;

/// The parts of syntax that differ between source and prepared forms.
pub trait Stage: Clone + fmt::Debug + PartialEq + 'static {
    /// Identifier occurrences.
    type Id: Clone + fmt::Debug + PartialEq;
    /// Variable occurrences.
    type Var: Clone + fmt::Debug + PartialEq;
    /// How a notation's mixop is held.
    type Mixop: Clone + fmt::Debug + PartialEq;
    /// The target of a subtype test: a type, or the runtime check it leaves.
    type SubTarget: Clone + fmt::Debug + PartialEq;
}

/// Syntax as elaboration and the passes produce it.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Source;

impl Stage for Source {
    type Id = Id;
    type Var = Var;
    type Mixop = Rc<tree::Mixop>;
    type SubTarget = Typ;
}

/// Syntax whose names are resolved to slots and mixops to arena handles.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Prepared;

impl Stage for Prepared {
    type Id = IdSlot;
    type Var = VarSlot;
    type Mixop = flat::Mixop;
    type SubTarget = Subcheck<Prepared>;
}
