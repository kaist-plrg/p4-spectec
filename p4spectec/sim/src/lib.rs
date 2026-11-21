//! Selects an architecture and assembles the native simulator
//!
//! `build` picks `ebpf`, `psa`, or `v1model` by name,
//! builds a host runner over the AL or SL specification
//! with that architecture as its extern,
//! and boxes it as a `Simulator` that runs STF tests.
//! `arch` holds the architectures and what they share,
//! written in the P4-level values of `p4`;
//! `spec` calls the specification's relations and functions
//! and converts its values to and from `p4`,
//! `stf` parses and runs one STF test,
//! and `cover` measures SL instruction coverage over many.

use std::path::{Path, PathBuf};

use p4spectec::lang::data::arena::Arena;

use p4spectec::interface::p4::P4Interface;

use p4spectec::runner::{self, Hooks, Runner};

use self::{
    arch::{ebpf::Ebpf, io::Tx, psa::Psa, v1model::V1Model},
    p4::RuntimeInterface,
};

mod arch;
pub mod cover;
mod error;
mod p4;
mod spec;
mod stf;

pub use arch::dummy::Dummy;
pub use error::SimError;

// == Simulator

/// Object-safe view of a runner, so `Simulator` can hold any architecture.
trait SimulatorRunner {
    fn hooks_mut(&mut self) -> &mut Hooks;

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
    Arch: RuntimeInterface,
{
    fn hooks_mut(&mut self) -> &mut Hooks {
        Runner::hooks_mut(self)
    }

    fn run_stf_test(
        &mut self,
        arena: &mut Arena,
        includes: &[PathBuf],
        path_p4: &Path,
        path_stf: &Path,
        on_match: &mut dyn FnMut(&Tx),
    ) -> Result<(), SimError> {
        stf::run::run_stf_test(arena, self, includes, path_p4, path_stf, on_match).map(drop)
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
        Arch: RuntimeInterface,
    {
        Self { arena, runner: Box::new(runner) }
    }

    /// Borrows the handlers retained across STF tests.
    pub fn hooks_mut(&mut self) -> &mut Hooks {
        self.runner.hooks_mut()
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
        "ebpf" => build_for_arch(spec, config, Ebpf::default()),
        "psa" => build_for_arch(spec, config, Psa::default()),
        "v1model" => build_for_arch(spec, config, V1Model::default()),
        _ => Err(error::architecture_unsupported(arch)),
    }
}

/// Builds the AL or SL runner, whichever the specification is.
fn build_for_arch<Arch: RuntimeInterface>(
    spec: runner::Spec,
    config: runner::Config,
    arch: Arch,
) -> Result<Simulator, SimError> {
    let mut arena = Arena::new();
    let runner = match spec {
        runner::Spec::Al(spec) => runner::build_al(&mut arena, spec, config, arch)?,
        runner::Spec::Sl(spec) => runner::build_sl(&mut arena, spec, config, arch)?,
        runner::Spec::Pl(spec) => runner::build_pl(&mut arena, spec, config, arch)?,
    };
    Ok(Simulator::new(arena, runner))
}
