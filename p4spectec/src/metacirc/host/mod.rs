//! The levels above a tower's native base
//!
//! A level above the base is a host: the level below interprets it, so it
//! only answers the builtin and host calls relayed from below. Each meta
//! level's extern holds the host of the level above it (`relay`), relaying
//! calls in the meta representation (`codec`); the AL or IL program on top
//! answers only its builtins. A host's extern runs with an interpreter lent
//! by the tower, which supports no reentry (`reentry`).

mod codec;
mod reentry;
mod relay;

pub(super) use relay::{MetaExtern, RelayExtern, TerminalExtern};

use crate::{
    interface::{meta::MetaInterface, p4::P4Interface},
    lang::{
        data::{arena::Arena, value::flat::Value},
        il::ast::{Id, Typ},
    },
    runner::{Eval, Host},
    sim_plugin::dummy::Dummy,
};

use reentry::NoReentry;

/// The host of a level above the base, which the level below interprets.
pub(in crate::metacirc) enum HostLayer {
    /// A P4 specification, answering the builtins and externs of its program.
    P4Spec(Host<P4Interface, Dummy>),
    /// A meta specification, hosting the script above it.
    MetaSpec(Host<MetaInterface, MetaExtern>),
}

impl HostLayer {
    /// Calls a decoded builtin at this layer.
    fn eval_builtin(
        &mut self,
        arena: &mut Arena,
        id: &Id,
        targs: &[Typ],
        values: &[Value],
    ) -> Eval<Value> {
        match self {
            Self::P4Spec(host) => host.eval_builtin(arena, id, targs, values),
            Self::MetaSpec(host) => host.eval_builtin(arena, id, targs, values),
        }
        .map_err(Into::into)
    }

    /// Calls a decoded host relation at this layer; `interp` answers its reentry.
    fn eval_extern_rel(
        &mut self,
        arena: &mut Arena,
        interp: &mut NoReentry,
        name: &str,
        values: &[Value],
    ) -> Eval<Vec<Value>> {
        match self {
            Self::P4Spec(host) => host.eval_extern_rel(arena, interp, name, values),
            Self::MetaSpec(host) => host.eval_extern_rel(arena, interp, name, values),
        }
    }

    /// Calls a decoded host function at this layer; `interp` answers its reentry.
    fn eval_extern_func(
        &mut self,
        arena: &mut Arena,
        interp: &mut NoReentry,
        name: &str,
        targs: &[Typ],
        values: &[Value],
    ) -> Eval<Value> {
        match self {
            Self::P4Spec(host) => host.eval_extern_func(arena, interp, name, targs, values),
            Self::MetaSpec(host) => host.eval_extern_func(arena, interp, name, targs, values),
        }
    }

    /// Resets this layer and all layers owned by its extern implementation.
    fn reset(&mut self) {
        match self {
            Self::P4Spec(host) => host.reset(),
            Self::MetaSpec(host) => host.reset(),
        }
    }
}
