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

use crate::lang::data::{arena::Arena, notation::MixopArena, value::flat::Value};

use crate::lang::al;

use crate::lang::il::ast::Typ;

use crate::lang::sl;

use crate::lang::pl;

use crate::diagnostic::Report;

use crate::interp::al::{AlInterp, Config as AlConfig, context::SpecEnv as AlSpecEnv};

use crate::interp::sl::{Config as SlConfig, SlInterp, context::SpecEnv as SlSpecEnv};

use crate::interp::pl::{Config as PlConfig, PlInterp, context::SpecEnv as PlSpecEnv};

use crate::interface::{
    self as builtin,
    p4::{error::P4Error, parse::parse_file},
};

use crate::sim_plugin::dummy::Dummy;

pub use context::{Checkpoint, RunnerContext};
pub use externs::{Extern, ExternError, NullExtern};
pub use interface::{BuiltinInterface, Interface, InterfaceError, NullInterface};
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

/// Builds an AL runner from a specification, with the P4 builtins.
///
/// Takes a spec after lowering. Panics if a global definition is repeated.
pub fn build_al<Ext: Extern>(
    spec: al::ast::Spec,
    config: Config,
    external: Ext,
) -> Result<Runner<BuiltinInterface, Ext>, BuildError> {
    let spec = Spec::Al(spec);
    // Builtins are chosen from the specification before it is consumed
    let interface = builtin::p4(&spec);
    let Spec::Al(spec) = spec else { unreachable!() };
    // Load and prepare the definitions
    let mut arena_mixop = MixopArena::new();
    let spec_env = AlSpecEnv::load(&mut arena_mixop, spec)?;
    let config = AlConfig::new(config.cache, config.det, config.guard);
    Ok(Runner::new(
        arena_mixop,
        Backend::Al { spec_env, interp: AlInterp::new(config) },
        interface,
        external,
    ))
}

/// Builds an SL runner from a specification, with the P4 builtins.
///
/// Takes a spec after lowering. Panics if a global definition is repeated.
pub fn build_sl<Ext: Extern>(
    spec: sl::ast::Spec,
    config: Config,
    external: Ext,
) -> Result<Runner<BuiltinInterface, Ext>, BuildError> {
    let spec = Spec::Sl(spec);
    // Builtins are chosen from the specification before it is consumed
    let interface = builtin::p4(&spec);
    let Spec::Sl(spec) = spec else { unreachable!() };
    // Load and prepare the definitions
    let mut arena_mixop = MixopArena::new();
    let spec_env = SlSpecEnv::load(&mut arena_mixop, config.det, spec)?;
    let config = SlConfig::new(config.cache, config.det, config.guard);
    Ok(Runner::new(
        arena_mixop,
        Backend::Sl { spec_env, interp: SlInterp::new(config) },
        interface,
        external,
    ))
}

/// Builds a PL runner from a specification, with the P4 builtins.
///
/// Takes a spec after lowering. Panics if a global definition is repeated.
pub fn build_pl<Ext: Extern>(
    spec: pl::ast::Spec,
    config: Config,
    external: Ext,
) -> Result<Runner<BuiltinInterface, Ext>, BuildError> {
    let spec = Spec::Pl(spec);
    let interface = builtin::p4(&spec);
    let Spec::Pl(spec) = spec else { unreachable!() };
    let mut arena_mixop = MixopArena::new();
    let spec_env = PlSpecEnv::load(&mut arena_mixop, spec)?;
    let config = PlConfig::new(config.cache, config.det, config.guard);
    Ok(Runner::new(
        arena_mixop,
        Backend::Pl { spec_env, interp: PlInterp::new(config) },
        interface,
        external,
    ))
}

// == Runner assembly

/// Pairs one stage's prepared definitions with its interpreter state.
pub(crate) enum Backend {
    Al { spec_env: AlSpecEnv, interp: AlInterp },
    Sl { spec_env: SlSpecEnv, interp: SlInterp },
    Pl { spec_env: PlSpecEnv, interp: PlInterp },
}

/// An interpreter and its host components sharing one arena.
pub struct Runner<Iface: Interface, Ext: Extern> {
    pub(crate) arena: Arena,
    pub(crate) backend: Backend,
    pub(crate) interface: Iface,
    pub(crate) external: Ext,
    pub(crate) effects: u64,
}

// Keep stage selection at the boundary, including native simulator operations
macro_rules! with_context {
    ($runner:expr, |$ctx:ident| $body:expr) => {{
        let runner = &mut *$runner;
        match &mut runner.backend {
            $crate::runner::Backend::Al { spec_env, interp } => {
                let mut $ctx = $crate::runner::RunnerContext::new(
                    &mut runner.arena,
                    spec_env,
                    interp,
                    &mut runner.interface,
                    &runner.external,
                    &mut runner.effects,
                );
                $body
            }
            $crate::runner::Backend::Sl { spec_env, interp } => {
                let mut $ctx = $crate::runner::RunnerContext::new(
                    &mut runner.arena,
                    spec_env,
                    interp,
                    &mut runner.interface,
                    &runner.external,
                    &mut runner.effects,
                );
                $body
            }
            $crate::runner::Backend::Pl { spec_env, interp } => {
                let mut $ctx = $crate::runner::RunnerContext::new(
                    &mut runner.arena,
                    spec_env,
                    interp,
                    &mut runner.interface,
                    &runner.external,
                    &mut runner.effects,
                );
                $body
            }
        }
    }};
}

pub(crate) use with_context;

impl<Iface: Interface, Ext: Extern> Runner<Iface, Ext> {
    /// Assembles the components around the selected prepared specification.
    fn new(arena_mixop: MixopArena, backend: Backend, interface: Iface, external: Ext) -> Self {
        Self {
            arena: Arena::with_arena_mixop(arena_mixop),
            backend,
            interface,
            external,
            effects: 0,
        }
    }

    pub fn arena(&self) -> &Arena {
        &self.arena
    }

    pub fn arena_mut(&mut self) -> &mut Arena {
        &mut self.arena
    }

    // - Evaluation

    /// Runs the program entry through the selected interpreter.
    pub fn eval_program(
        &mut self,
        name: &str,
        program: Value,
    ) -> Eval<Vec<Value>, InterpreterError> {
        with_context!(self, |ctx| ctx.call_program(name, program))
    }

    /// Calls a relation through the selected interpreter.
    pub fn eval_rel(&mut self, name: &str, values: &[Value]) -> Eval<Vec<Value>, InterpreterError> {
        with_context!(self, |ctx| ctx.call_rel(name, values))
    }

    /// Calls a function through the selected interpreter.
    pub fn eval_func(
        &mut self,
        name: &str,
        targs: &[Typ],
        values: &[Value],
    ) -> Eval<Value, InterpreterError> {
        with_context!(self, |ctx| ctx.call_func(name, targs, values))
    }

    // - Lifecycle

    /// Starts an independent program, keeping definitions and configuration.
    ///
    /// Discards all value handles and cached results,
    /// while retaining the notation shapes used by prepared definitions.
    pub fn reset(&mut self) {
        self.effects = 0;
        with_context!(self, |ctx| Interpreter::<Iface, Ext>::reset(ctx.interp_mut()));
        self.external.clear();
        self.interface.clear();
        self.arena.reset_values();
    }
}

// == Program execution

/// A failure while building a runner, parsing P4, or evaluating a program.
#[derive(Debug, thiserror::Error)]
pub enum RunError {
    /// A diagnostic from runner construction.
    #[error(transparent)]
    Build(BuildError),
    /// A P4 preprocessing or syntax failure.
    #[error(transparent)]
    Parse(P4Error),
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
    match spec {
        Spec::Al(spec_al) => {
            let runner = build_al(spec_al, config, Dummy).map_err(RunError::Build)?;
            run_program(runner, relation, includes, path_p4)
        }
        Spec::Sl(spec_sl) => {
            let runner = build_sl(spec_sl, config, Dummy).map_err(RunError::Build)?;
            run_program(runner, relation, includes, path_p4)
        }
        Spec::Pl(spec_pl) => {
            let runner = build_pl(spec_pl, config, Dummy).map_err(RunError::Build)?;
            run_program(runner, relation, includes, path_p4)
        }
    }
}

/// Parses P4 into the runner's arena and evaluates its entry relation.
fn run_program(
    mut runner: Runner<BuiltinInterface, Dummy>,
    relation: &str,
    includes: &[PathBuf],
    path_p4: &Path,
) -> Result<(), RunError> {
    // Parse the program into the arena used by evaluation
    let program = parse_file(runner.arena_mut(), includes, path_p4).map_err(RunError::Parse)?;

    // Discard effects at the command boundary, after evaluation has finished
    runner
        .eval_program(relation, program)
        .result
        .map_err(RunError::Eval)?;
    Ok(())
}
