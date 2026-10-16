//! Assembly and dispatch of specification execution components
//!
//! `Runner<Iface, Ext>` selects its prepared definitions and interpreter
//! through `Backend`, while each evaluation keeps a typed `RunnerContext`.
//! Host calls and simulator operations borrow the same components,
//! so externs can reenter the selected interpreter.
//! `build_al`, `build_sl`, and `build_pl` prepare the selected stage.

mod context;
mod externs;
mod interface;
mod interpreter;

use std::path::{Path, PathBuf};

use crate::lang::data::{arena::Arena, value::flat::Value};

use crate::lang::al;

use crate::lang::il::ast::Typ;

use crate::lang::sl;

use crate::lang::pl;

use crate::diagnostic::Report;

use crate::interp::al::{AlInterp, Config as AlConfig, context::SpecEnv as AlSpecEnv};

use crate::interp::sl::{Config as SlConfig, SlInterp, context::SpecEnv as SlSpecEnv};

use crate::interp::pl::{Config as PlConfig, PlInterp, context::SpecEnv as PlSpecEnv};

use crate::interface::{self as builtin, p4::P4Interface};

use crate::sim_plugin::dummy::Dummy;

pub use context::{Checkpoint, RunnerContext};
pub use externs::{Extern, ExternError, NullExtern};
pub use interface::{Interface, InterfaceError, NullInterface};
pub use interpreter::{Eval, Interpreter, InterpreterError, SideEffect};

// == Runner construction

/// An AL, SL, or PL specification.
pub enum Spec {
    /// An AL specification.
    Al(al::ast::Spec),
    /// An SL specification.
    Sl(sl::ast::Spec),
    /// A PL specification.
    Pl(pl::ast::Spec),
}

/// Interpreter options shared by AL, SL, and PL.
#[derive(Clone, Copy)]
pub struct Config {
    /// Memoize pure calls.
    cache: bool,
    /// Check determinism of candidate selection.
    det: bool,
    /// Type-check arguments and results at call boundaries.
    guard: bool,
}

impl Config {
    pub fn new(cache: bool, det: bool, guard: bool) -> Self {
        Self { cache, det, guard }
    }
}

/// A diagnostic produced while building a runner.
pub type BuildError = Box<Report>;

/// Builds an AL runner from a specification, with the P4 interface.
///
/// Takes a spec after lowering and interns notation in the execution arena.
/// Every evaluation must use this arena. Panics if a definition is repeated.
pub fn build_al<Ext: Extern>(
    arena: &mut Arena,
    spec: al::ast::Spec,
    config: Config,
    external: Ext,
) -> Result<Runner<P4Interface, Ext>, BuildError> {
    let spec = Spec::Al(spec);
    let interface = builtin::p4(&spec);
    Runner::new(arena, spec, config, interface, external)
}

/// Builds an SL runner from a specification, with the P4 interface.
///
/// Takes a spec after lowering and interns notation in the execution arena.
/// Every evaluation must use this arena. Panics if a definition is repeated.
pub fn build_sl<Ext: Extern>(
    arena: &mut Arena,
    spec: sl::ast::Spec,
    config: Config,
    external: Ext,
) -> Result<Runner<P4Interface, Ext>, BuildError> {
    let spec = Spec::Sl(spec);
    let interface = builtin::p4(&spec);
    Runner::new(arena, spec, config, interface, external)
}

/// Builds a PL runner from a specification, with the P4 interface.
///
/// Takes a spec after lowering and interns notation in the execution arena.
/// Every evaluation must use this arena. Panics if a definition is repeated.
pub fn build_pl<Ext: Extern>(
    arena: &mut Arena,
    spec: pl::ast::Spec,
    config: Config,
    external: Ext,
) -> Result<Runner<P4Interface, Ext>, BuildError> {
    let spec = Spec::Pl(spec);
    let interface = builtin::p4(&spec);
    Runner::new(arena, spec, config, interface, external)
}

// == Runner assembly

/// Prepared definitions, interpreter state, and host components.
///
/// Values and notation belong to the arena supplied during preparation.
/// All calls must borrow that same arena until this runner is dropped.
pub struct Runner<Iface: Interface, Ext: Extern> {
    pub(crate) backend: Backend,
    pub(crate) interface: Iface,
    pub(crate) external: Ext,
    pub(crate) effects: u64,
}

/// Pairs one stage's prepared definitions with its interpreter state.
pub(crate) enum Backend {
    Al { spec_env: AlSpecEnv, interp: AlInterp },
    Sl { spec_env: SlSpecEnv, interp: SlInterp },
    Pl { spec_env: PlSpecEnv, interp: PlInterp },
}

impl<Iface: Interface, Ext: Extern> Runner<Iface, Ext> {
    /// Prepares a specification with the supplied interface and extern state.
    ///
    /// Interns notation in `arena`; every evaluation must borrow that same arena.
    pub fn new(
        arena: &mut Arena,
        spec: Spec,
        config: Config,
        interface: Iface,
        external: Ext,
    ) -> Result<Self, BuildError> {
        let backend = match spec {
            Spec::Al(spec) => Backend::Al {
                spec_env: AlSpecEnv::load(&mut arena.mixop, spec)?,
                interp: AlInterp::new(AlConfig::new(config.cache, config.det, config.guard)),
            },
            Spec::Sl(spec) => Backend::Sl {
                spec_env: SlSpecEnv::load(&mut arena.mixop, config.det, spec)?,
                interp: SlInterp::new(SlConfig::new(config.cache, config.det, config.guard)),
            },
            Spec::Pl(spec) => Backend::Pl {
                spec_env: PlSpecEnv::load(&mut arena.mixop, spec)?,
                interp: PlInterp::new(PlConfig::new(config.cache, config.det, config.guard)),
            },
        };
        Ok(Self { backend, interface, external, effects: 0 })
    }

    // - Parsing

    /// Parses input files through the interface into the shared arena.
    ///
    /// Uses the same arena supplied when preparing this runner.
    pub fn parse_program(
        &mut self,
        arena: &mut Arena,
        includes: &[PathBuf],
        paths: &[PathBuf],
    ) -> Result<Value, InterfaceError> {
        self.interface.parse_program(arena, includes, paths)
    }

    // - Evaluation

    /// Runs the program entry through the selected interpreter.
    ///
    /// Retains cached results; call `reset` before preparing an independent program.
    pub fn eval_program(
        &mut self,
        arena: &mut Arena,
        name: &str,
        program: Value,
    ) -> Eval<Vec<Value>, InterpreterError> {
        match &mut self.backend {
            Backend::Al { spec_env, interp } => RunnerContext::new(
                arena,
                spec_env,
                interp,
                &mut self.interface,
                &mut self.external,
                &mut self.effects,
            )
            .call_program(name, program),
            Backend::Sl { spec_env, interp } => RunnerContext::new(
                arena,
                spec_env,
                interp,
                &mut self.interface,
                &mut self.external,
                &mut self.effects,
            )
            .call_program(name, program),
            Backend::Pl { spec_env, interp } => RunnerContext::new(
                arena,
                spec_env,
                interp,
                &mut self.interface,
                &mut self.external,
                &mut self.effects,
            )
            .call_program(name, program),
        }
    }

    /// Calls a relation through the selected interpreter.
    ///
    /// Shares cached results with other calls in the current program.
    pub fn eval_rel(
        &mut self,
        arena: &mut Arena,
        name: &str,
        values: &[Value],
    ) -> Eval<Vec<Value>, InterpreterError> {
        match &mut self.backend {
            Backend::Al { spec_env, interp } => RunnerContext::new(
                arena,
                spec_env,
                interp,
                &mut self.interface,
                &mut self.external,
                &mut self.effects,
            )
            .call_rel(name, values),
            Backend::Sl { spec_env, interp } => RunnerContext::new(
                arena,
                spec_env,
                interp,
                &mut self.interface,
                &mut self.external,
                &mut self.effects,
            )
            .call_rel(name, values),
            Backend::Pl { spec_env, interp } => RunnerContext::new(
                arena,
                spec_env,
                interp,
                &mut self.interface,
                &mut self.external,
                &mut self.effects,
            )
            .call_rel(name, values),
        }
    }

    /// Calls a function through the selected interpreter.
    ///
    /// Shares cached results with other calls in the current program.
    pub fn eval_func(
        &mut self,
        arena: &mut Arena,
        name: &str,
        targs: &[Typ],
        values: &[Value],
    ) -> Eval<Value, InterpreterError> {
        match &mut self.backend {
            Backend::Al { spec_env, interp } => RunnerContext::new(
                arena,
                spec_env,
                interp,
                &mut self.interface,
                &mut self.external,
                &mut self.effects,
            )
            .call_func(name, targs, values),
            Backend::Sl { spec_env, interp } => RunnerContext::new(
                arena,
                spec_env,
                interp,
                &mut self.interface,
                &mut self.external,
                &mut self.effects,
            )
            .call_func(name, targs, values),
            Backend::Pl { spec_env, interp } => RunnerContext::new(
                arena,
                spec_env,
                interp,
                &mut self.interface,
                &mut self.external,
                &mut self.effects,
            )
            .call_func(name, targs, values),
        }
    }

    // - Lifecycle

    /// Starts an independent program, keeping definitions and configuration.
    ///
    /// Clears this runner's caches, host state, and effects.
    /// Reset every runner sharing an arena before calling `Arena::reset_values`.
    pub fn reset(&mut self) {
        self.effects = 0;
        match &mut self.backend {
            Backend::Al { interp, .. } => Interpreter::<Iface, Ext>::reset(interp),
            Backend::Sl { interp, .. } => Interpreter::<Iface, Ext>::reset(interp),
            Backend::Pl { interp, .. } => Interpreter::<Iface, Ext>::reset(interp),
        }
        self.external.clear();
        self.interface.clear();
    }
}

// == Program execution

/// A failure while building a runner, parsing, or evaluating a program.
#[derive(Debug, thiserror::Error)]
pub enum RunError {
    /// A diagnostic from runner construction.
    #[error(transparent)]
    Build(BuildError),
    /// A program input or parsing failure.
    #[error(transparent)]
    Parse(InterfaceError),
    /// An aborting evaluation or unmatched relation.
    #[error(transparent)]
    Eval(InterpreterError),
}

/// Builds a runner with dummy externs and evaluates a P4 program's entry.
pub fn run(
    spec: Spec,
    config: Config,
    relation: &str,
    includes: &[PathBuf],
    path_p4: &Path,
) -> Result<(), RunError> {
    let mut arena = Arena::new();
    let runner = match spec {
        Spec::Al(spec_al) => build_al(&mut arena, spec_al, config, Dummy),
        Spec::Sl(spec_sl) => build_sl(&mut arena, spec_sl, config, Dummy),
        Spec::Pl(spec_pl) => build_pl(&mut arena, spec_pl, config, Dummy),
    }
    .map_err(RunError::Build)?;
    run_program(&mut arena, runner, relation, includes, path_p4)
}

/// Parses through the runner's interface and evaluates the entry relation.
fn run_program<Iface: Interface, Ext: Extern>(
    arena: &mut Arena,
    mut runner: Runner<Iface, Ext>,
    relation: &str,
    includes: &[PathBuf],
    path: &Path,
) -> Result<(), RunError> {
    // Parse the program into the arena used to prepare the specification
    let program = runner
        .parse_program(arena, includes, &[path.to_path_buf()])
        .map_err(RunError::Parse)?;

    // Discard effects at the command boundary, after evaluation has finished
    runner
        .eval_program(arena, relation, program)
        .result
        .map_err(RunError::Eval)?;
    Ok(())
}
