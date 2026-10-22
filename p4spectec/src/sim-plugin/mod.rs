//! Selects an architecture and assembles the native simulator
//!
//! `build` picks `ebpf`, `psa`, or `v1model` by name,
//! builds a host runner over the AL or SL specification
//! with that architecture as its extern,
//! and boxes it as a `Simulator` that runs STF tests.
//! `core` and `spec` are the helpers the architectures share;
//! `stf_runner`, `table`, `hash`, `io`, and `state` drive one test.

use std::path::{Path, PathBuf};

use crate::lang::data::{arena::Arena, value::ValueError};

use crate::interface::p4::P4Interface;

use crate::runner::{self, ExternError, Runner};

use self::{arch::Architecture, ebpf::Ebpf, io::Tx, psa::Psa, v1model::V1Model};

pub mod arch;
pub mod core;
pub mod dummy;
pub mod ebpf;
mod error;
mod externs;
pub mod hash;
pub mod io;
pub mod psa;
pub mod spec;
pub mod state;
pub mod stf_runner;
pub mod table;
pub mod v1model;

pub use error::SimError;

// == Simulator

/// Object-safe view of a runner, so `Simulator` can hold any architecture.
trait SimulatorRunner {
    fn run_stf_test(
        &mut self,
        arena: &mut Arena,
        includes: &[PathBuf],
        path_p4: &Path,
        path_stf: &Path,
        on_match: &mut dyn FnMut(&Tx),
    ) -> Result<(), SimError>;
}

impl<Arch> SimulatorRunner for Runner<P4Interface, Arch>
where
    Arch: Architecture + 'static,
{
    fn run_stf_test(
        &mut self,
        arena: &mut Arena,
        includes: &[PathBuf],
        path_p4: &Path,
        path_stf: &Path,
        on_match: &mut dyn FnMut(&Tx),
    ) -> Result<(), SimError> {
        stf_runner::run_stf_test(arena, self, includes, path_p4, path_stf, on_match).map(drop)
    }
}

/// A built runner ready to execute STF tests.
pub struct Simulator {
    arena: Arena,
    /// The runner, erased over its architecture type.
    runner: Box<dyn SimulatorRunner>,
}

impl Simulator {
    /// Boxes a runner.
    fn new<Arch>(arena: Arena, runner: Runner<P4Interface, Arch>) -> Self
    where
        Arch: Architecture + 'static,
    {
        Self { arena, runner: Box::new(runner) }
    }

    /// Runs one STF test, calling `on_match` for each matched output packet.
    pub fn run_stf_test(
        &mut self,
        includes: &[PathBuf],
        path_p4: &Path,
        path_stf: &Path,
        mut on_match: impl FnMut(&Tx),
    ) -> Result<(), SimError> {
        self.runner
            .run_stf_test(&mut self.arena, includes, path_p4, path_stf, &mut on_match)
    }
}

// == Construction

/// Builds a simulator for the named architecture, logging to stdout.
pub fn build(
    spec: runner::Spec,
    arch: &str,
    config: runner::Config,
) -> Result<Simulator, SimError> {
    // Each architecture is its own extern implementation
    match arch {
        "ebpf" => build_for_arch(spec, config, Ebpf::new),
        "psa" => build_for_arch(spec, config, Psa::new),
        "v1model" => build_for_arch(spec, config, V1Model::new),
        _ => Err(error::architecture_unsupported(arch)),
    }
}

/// Builds the AL, SL, or PL runner, whichever the specification is.
///
/// The architecture defines its extern syntax types in the new arena.
fn build_for_arch<Arch: Architecture + 'static>(
    spec: runner::Spec,
    config: runner::Config,
    define_arch: fn(&mut Arena) -> Result<Arch, ValueError>,
) -> Result<Simulator, SimError> {
    let mut arena = Arena::new();
    let arch = define_arch(&mut arena).map_err(|error| ExternError::from(error).into_report())?;
    let runner = match spec {
        runner::Spec::Al(spec) => runner::build_al(&mut arena, spec, config, arch)?,
        runner::Spec::Sl(spec) => runner::build_sl(&mut arena, spec, config, arch)?,
        runner::Spec::Pl(spec) => runner::build_pl(&mut arena, spec, config, arch)?,
    };
    Ok(Simulator::new(arena, runner))
}
