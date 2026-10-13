//! Runtime-sized chain of native runners
//!
//! Meta runners use the AL input interface and recursively own another runner.
//! The terminal runner supplies P4 parsing and target builtin/extern calls.
//! Every method borrows the caller's single arena without replacing it.

use super::externs::TowerExtern;
use crate::{
    interface::{al::AlInterface, p4::P4Interface},
    lang::{
        data::{arena::Arena, value::flat::Value},
        il::ast::Typ,
    },
    runner::{Eval, InterfaceError, Runner},
    sim_plugin::dummy::Dummy,
};
use std::path::{Path, PathBuf};

/// A finite native dispatch chain with a P4 leaf.
pub(super) enum Tower {
    Target(Runner<P4Interface, Dummy>),
    Meta(Runner<AlInterface, TowerExtern>),
}

impl Tower {
    /// Parses the P4 input through the terminal runner's interface.
    pub fn parse_program(
        &mut self,
        arena: &mut Arena,
        includes: &[PathBuf],
        path: &Path,
    ) -> Result<Value, InterfaceError> {
        match self {
            Self::Target(runner) => runner.parse_program(arena, includes, &[path.to_path_buf()]),
            Self::Meta(runner) => runner
                .external
                .runner_next
                .parse_program(arena, includes, path),
        }
    }

    /// Enters the outer specification with its encoded input script.
    pub fn eval_program(
        &mut self,
        arena: &mut Arena,
        name: &str,
        program: Value,
    ) -> Eval<Vec<Value>> {
        match self {
            Self::Target(runner) => runner.eval_program(arena, name, program),
            Self::Meta(runner) => runner.eval_program(arena, name, program),
        }
    }

    /// Dispatches a decoded relation at this layer.
    pub fn eval_rel(
        &mut self,
        arena: &mut Arena,
        name: &str,
        values: &[Value],
    ) -> Eval<Vec<Value>> {
        match self {
            Self::Target(runner) => runner.eval_rel(arena, name, values),
            Self::Meta(runner) => runner.eval_rel(arena, name, values),
        }
    }

    /// Dispatches a decoded function at this layer.
    pub fn eval_func(
        &mut self,
        arena: &mut Arena,
        name: &str,
        targs: &[Typ],
        values: &[Value],
    ) -> Eval<Value> {
        match self {
            Self::Target(runner) => runner.eval_func(arena, name, targs, values),
            Self::Meta(runner) => runner.eval_func(arena, name, targs, values),
        }
    }

    /// Resets this runner and all runners owned by its extern.
    pub fn reset(&mut self) {
        match self {
            Self::Target(runner) => runner.reset(),
            Self::Meta(runner) => runner.reset(),
        }
    }
}
