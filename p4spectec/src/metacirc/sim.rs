//! STF simulation of a P4 program executed by spec-meta
//!
//! `SimTower` keeps a tower of two or more levels: spec-meta runs natively
//! at its base, each level above interprets the next through spec-meta,
//! and the P4 host with the architecture is the top of the tower's `Upper`.
//! For each STF operation, `with_context` reaches that host
//! and lends it the base's native interpreter as depth 0 (`TowerInterp`),
//! so the architecture drives the levels' execution of the P4 specification;
//! deeper depths are made when the architecture reenters.
//! `build_simulator` selects the architecture and boxes the tower as a `Simulator`.

use super::{
    ExecutionMode, Result, TowerConfig,
    config::Lang,
    engine::{EngineKind, SpecTecEngine},
    error::{config_invalid, input_invalid},
    interp::{TowerEnv, TowerInterp},
    tower::{BaseLayer, Top, Tower, Upper},
};
use crate::{
    interface::p4::P4Interface,
    lang::{
        common::source::Phrase,
        data::{arena::Arena, encoding::Encoding},
    },
    runner::{self, Runner, RunnerContext},
    sim_plugin::{
        SimError, Simulator, SimulatorRunner,
        arch::Architecture,
        ebpf::Ebpf,
        io::Tx,
        psa::Psa,
        stf_runner::{self, Run},
        v1model::V1Model,
    },
    stf::{self, ast::Statement},
};
use std::path::{Path, PathBuf};

// == Tower

/// A tower simulating its P4 top level through the spec-meta levels below it.
pub(super) struct SimTower<Arch: Architecture> {
    tower: Tower<Arch>,
}

impl<Arch: Architecture> SimTower<Arch> {
    /// Builds a tower of an SL-executed meta specification over a P4 top level.
    fn build(
        arena: &mut Arena,
        config: &TowerConfig,
        options: runner::Config,
        arch: Arch,
    ) -> Result<Self> {
        // Depth 0 is the base's SL interpreter, and the top level is P4
        if config.levels.len() < 2
            || config.top_lang() != Lang::P4
            || !matches!(config.mode, ExecutionMode::Sl)
        {
            return Err(config_invalid(
                "simulation requires an sl tower of two or more levels with a P4 top level",
            ));
        }
        let tower = Tower::build(arena, config, options, arch)?;
        Ok(Self { tower })
    }

    /// Runs `body` in the P4 context of depth 0, over the stored P4 host.
    fn with_context<T>(
        &mut self,
        arena: &mut Arena,
        body: impl FnOnce(&mut RunnerContext<'_, TowerInterp<'_>, P4Interface, Arch>) -> T,
    ) -> T {
        let Tower { levels, protocols, base, options } = &mut self.tower;
        // Reach depth 0 and the P4 host in the base runner
        let BaseLayer::MetaSpec(runner) = base else {
            unreachable!("a simulated tower has a meta base");
        };
        let Runner { backend, interface, external: upper, .. } = runner;
        let (engine, spec_base) = SpecTecEngine::base(backend, interface);
        // Lend the levels' storage at depth 0 and reach the P4 top
        let Upper { caches, ctxs, hosts, top, depth_next } = upper;
        let Top::P4(host) = top else {
            unreachable!("a simulated tower has a P4 top level");
        };
        // Share the definitions, and lend depth 0 to the host
        let env = TowerEnv { spec_base, levels, protocols, options: *options };
        let engine = EngineKind::SpecTec(engine);
        let mut interp = TowerInterp { engine, hosts, depth_next, caches, ctxs };
        host.with_context(arena, &env, &mut interp, body)
    }

    // - STF execution

    /// Parses the program and initializes the pipeline from a reset tower.
    fn init_pipe(
        &mut self,
        arena: &mut Arena,
        includes: &[PathBuf],
        path: &Path,
    ) -> std::result::Result<Run, SimError> {
        // Each program starts from an empty arena, cleared host state, and no C
        self.tower.reset();
        arena.reset_values();
        self.with_context(arena, |ctx| stf_runner::init_pipe_in(ctx, includes, path))
    }

    /// Executes one statement; returns the output packet it matched, if any.
    fn run_stf_stmt(
        &mut self,
        arena: &mut Arena,
        run: &mut Run,
        stmt: &Phrase<Statement>,
    ) -> std::result::Result<Option<Tx>, SimError> {
        self.with_context(arena, |ctx| stf_runner::run_stf_stmt_in(ctx, run, stmt))
    }
}

impl<Arch: Architecture + 'static> SimulatorRunner for SimTower<Arch> {
    fn run_stf_test(
        &mut self,
        arena: &mut Arena,
        includes: &[PathBuf],
        path_p4: &Path,
        path_stf: &Path,
        on_match: &mut dyn FnMut(&Tx),
    ) -> std::result::Result<(), SimError> {
        let mut run = self.init_pipe(arena, includes, path_p4)?;
        let stmts = stf::parse::parse_file(path_stf)?;
        for stmt in &stmts {
            if let Some(tx) = self.run_stf_stmt(arena, &mut run, stmt)? {
                on_match(&tx);
            }
        }
        // Everything expected must have arrived, and nothing unexpected
        run.finish()
    }
}

// == Construction

/// Builds a simulator for the named architecture on an SL tower over P4.
pub fn build_simulator(
    config: &TowerConfig,
    arch: &str,
    options: runner::Config,
    encoding: Encoding,
) -> Result<Simulator> {
    // Each architecture is its own extern implementation
    match arch {
        "ebpf" => build_for_arch(config, options, Ebpf::new(encoding)),
        "psa" => build_for_arch(config, options, Psa::new(encoding)),
        "v1model" => build_for_arch(config, options, V1Model::new(encoding)),
        _ => Err(input_invalid(format!("architecture {arch} is not supported"))),
    }
}

/// Builds the tower with the architecture as its P4 extern.
fn build_for_arch<Arch: Architecture + 'static>(
    config: &TowerConfig,
    options: runner::Config,
    arch: Arch,
) -> Result<Simulator> {
    let mut arena = Arena::new();
    let tower = SimTower::build(&mut arena, config, options, arch)?;
    Ok(Simulator::new(arena, tower))
}
