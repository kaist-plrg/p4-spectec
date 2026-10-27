//! STF simulation of a P4 program executed by spec-meta
//!
//! `SimTower` keeps a two-level tower: spec-meta runs natively at its base,
//! and the P4 host with the architecture is stored in the base relay.
//! For each STF operation, `with_context` lends the base's SL interpreter
//! to the P4 host as depth 0 (`MetaInterp`),
//! so the architecture drives spec-meta's execution of the P4 specification;
//! deeper depths are made when the architecture reenters.
//! `build_simulator` selects the architecture and boxes the tower as a `Simulator`.

use super::{
    ExecutionMode, Result, TowerConfig,
    config::Lang,
    error::{config_invalid, input_invalid},
    reentry::{MetaDepth, MetaEnv, MetaInterp},
    relay::{MetaExtern, RelayExtern},
    tower::{BaseLayer, HostLayer, Tower},
};
use crate::{
    interface::{meta::shared::Cache, p4::P4Interface},
    lang::{
        common::source::Phrase,
        data::{arena::Arena, encoding::Encoding, value::flat::Value},
    },
    runner::{self, Backend, Runner, RunnerContext},
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

/// A two-level tower simulating its P4 top level through spec-meta.
pub(super) struct SimTower<Arch: Architecture> {
    tower: Tower<Arch>,
    options: runner::Config,
    /// Depths 1 and deeper, made on the first reentry of a program.
    depth_next: Option<Box<MetaDepth>>,
    /// C of each level above the base for the current program, from level 1.
    ctx_targets: Vec<Option<Value>>,
    /// The codec cache of each level boundary, from the base up.
    caches: Vec<Cache>,
}

impl<Arch: Architecture> SimTower<Arch> {
    /// Builds a tower of an SL-executed meta specification over a P4 top level.
    fn build(
        arena: &mut Arena,
        config: &TowerConfig,
        options: runner::Config,
        arch: Arch,
    ) -> Result<Self> {
        // Depth 0 is the base's SL interpreter, and the host above it is P4
        if config.levels.len() != 2
            || config.top_lang() != Lang::P4
            || !matches!(config.mode, ExecutionMode::Sl)
        {
            return Err(config_invalid(
                "simulation requires a two-level sl tower with a P4 top level",
            ));
        }
        let tower = Tower::build(arena, config, options, arch)?;
        // Each level above the base has its C and the codec cache below it
        let count = config.levels.len() - 1;
        let caches = (0..count).map(|_| Cache::new(options.cache)).collect();
        Ok(Self { tower, options, depth_next: None, ctx_targets: vec![None; count], caches })
    }

    /// Runs `body` in the P4 context of depth 0, over the stored P4 host.
    fn with_context<T>(
        &mut self,
        arena: &mut Arena,
        body: impl FnOnce(&mut RunnerContext<'_, MetaInterp<'_>, P4Interface, Arch>) -> T,
    ) -> T {
        let Self { tower, options, depth_next, ctx_targets, caches } = self;
        let Tower { levels, base } = tower;
        // Reach depth 0 and the P4 host in the base runner
        let BaseLayer::MetaSpec(runner) = base else {
            unreachable!("a simulated tower has a meta base");
        };
        let Runner { backend, interface, external, .. } = runner;
        let Backend::Sl { spec_env, interp } = backend else {
            unreachable!("a simulated tower executes SL");
        };
        let MetaExtern::Relay(RelayExtern { layer_next, .. }) = external else {
            unreachable!("a simulated tower relays to its top level");
        };
        let HostLayer::P4Spec(host) = &mut **layer_next else {
            unreachable!("a simulated tower has a P4 top level");
        };
        // Share the definitions, and lend depth 0 to the host
        let env = MetaEnv { spec_meta: spec_env, levels, options: *options };
        let mut meta = MetaInterp { interp, interface, depth_next, ctx_targets, caches };
        host.with_context(arena, &env, &mut meta, body)
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
        self.depth_next = None;
        self.ctx_targets.fill(None);
        self.caches.iter_mut().for_each(Cache::clear);
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

/// Builds a simulator for the named architecture on a two-level SL tower over P4.
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
