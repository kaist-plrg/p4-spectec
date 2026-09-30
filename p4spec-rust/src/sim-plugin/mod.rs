//! Selects an architecture and assembles the native simulator
//!
//! `build` picks `ebpf`, `psa`, or `v1model` by name,
//! builds a host runner over the AL or SL specification
//! with that architecture as its extern,
//! and boxes it as a `Simulator` that runs STF tests.
//! `core` and `spec` are the helpers the architectures share;
//! `stf_runner`, `table`, `hash`, `io`, and `state` drive one test.

use self::{arch::Architecture, ebpf::Ebpf, io::Tx, psa::Psa, v1model::V1Model};
use crate::{
    diagnostic::Report,
    lang::data::value::external::Encoding,
    runner::{self as host, BuiltinInterface, Interpreter, Runner},
};
use std::path::{Path, PathBuf};

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

// == Build errors

/// Why a simulator could not be built.
pub type BuildError = Box<crate::diagnostic::Report>;

// == Simulator

/// Object-safe view of a runner, so `Simulator` can hold any architecture.
trait SimulatorRunner {
    fn run_stf_test(
        &mut self,
        includes: &[PathBuf],
        path_p4: &Path,
        path_stf: &Path,
        on_match: &mut dyn FnMut(&Tx),
    ) -> Result<(), Box<Report>>;
}

impl<Interp, Arch> SimulatorRunner for Runner<Interp, BuiltinInterface, Arch>
where
    Interp: Interpreter<BuiltinInterface, Arch> + 'static,
    Arch: Architecture + 'static,
{
    fn run_stf_test(
        &mut self,
        includes: &[PathBuf],
        path_p4: &Path,
        path_stf: &Path,
        on_match: &mut dyn FnMut(&Tx),
    ) -> Result<(), Box<Report>> {
        stf_runner::run_stf_test(self, includes, path_p4, path_stf, on_match).map(drop)
    }
}

/// A built runner ready to execute STF tests.
pub struct Simulator {
    /// The runner, erased over its interpreter and architecture types.
    runner: Box<dyn SimulatorRunner>,
}

impl Simulator {
    /// Boxes a runner.
    fn new<Interp, Arch>(runner: Runner<Interp, BuiltinInterface, Arch>) -> Self
    where
        Interp: Interpreter<BuiltinInterface, Arch> + 'static,
        Arch: Architecture + 'static,
    {
        Self { runner: Box::new(runner) }
    }

    /// Runs one STF test, calling `on_match` for each matched output packet.
    pub fn run_stf_test(
        &mut self,
        includes: &[PathBuf],
        path_p4: &Path,
        path_stf: &Path,
        mut on_match: impl FnMut(&Tx),
    ) -> Result<(), Box<Report>> {
        self.runner
            .run_stf_test(includes, path_p4, path_stf, &mut on_match)
    }
}

// == Construction

/// Builds a simulator for the named architecture.
pub fn build(
    spec: host::Spec,
    arch: &str,
    config: host::Config,
    encoding: Encoding,
) -> Result<Simulator, BuildError> {
    // Each architecture is its own extern implementation
    match arch {
        "ebpf" => build_for_arch(spec, config, Ebpf::new(encoding)),
        "psa" => build_for_arch(spec, config, Psa::new(encoding)),
        "v1model" => build_for_arch(spec, config, V1Model::new(encoding)),
        _ => Err(error::architecture_unsupported(arch)),
    }
}

/// Builds the AL or SL runner, whichever the specification is.
fn build_for_arch<Arch: Architecture + 'static>(
    spec: host::Spec,
    config: host::Config,
    arch: Arch,
) -> Result<Simulator, BuildError> {
    match spec {
        host::Spec::Al(spec) => Ok(Simulator::new(host::build_al(spec, config, arch)?)),
        host::Spec::Sl(spec) => Ok(Simulator::new(host::build_sl(spec, config, arch)?)),
        host::Spec::Pl(spec) => Ok(Simulator::new(host::build_pl(spec, config, arch)?)),
    }
}
