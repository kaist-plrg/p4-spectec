//! Static assembly of the specification execution components
//!
//! `Runner<Interp, Iface, Ext>` owns the arena, specification, interpreter,
//! builtin interface, and extern implementation.
//! The interpreter owns its configuration and cache.
//! Each evaluation borrows these components through a `RunnerContext`,
//! which an extern can use to reenter the interpreter.
//! `build_al` and `build_sl` assemble a runner from a specification.

mod context;
mod externs;
mod interface;
mod interpreter;

use std::path::{Path, PathBuf};

use crate::{
    interface::p4::{error::P4Error, parse::parse_file},
    sim_plugin::dummy::Dummy,
};

use crate::lang::data::value::{Value, ValueArena};

use crate::lang::al;

use crate::lang::sl;

use crate::lang::pl;

use crate::diagnostic::Report;

use crate::interp::al::{AlInterp, Config as AlConfig, context::Global as AlGlobal};

use crate::interp::sl::{Config as SlConfig, SlInterp, context::Global as SlGlobal};

use crate::interp::pl::{Config as PlConfig, PlInterp, context::Global as PlGlobal};

use crate::interface as builtin;

pub use context::RunnerContext;
pub use externs::{Extern, ExternError, NullExtern};
pub use interface::{BuiltinInterface, Interface, InterfaceError, NullInterface};
pub use interpreter::{Interpreter, InterpreterError};

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
) -> Result<Runner<AlInterp, BuiltinInterface, Ext>, BuildError> {
    let spec = Spec::Al(spec);
    // Builtins are chosen from the specification before it is consumed
    let interface = builtin::p4(&spec);
    let Spec::Al(spec) = spec else { unreachable!() };
    // Load and prepare the definitions
    let global = AlGlobal::load(spec)?;
    let config = AlConfig::new(config.cache, config.det, config.guard);
    Ok(Runner::new(global, AlInterp::new(config), interface, external))
}

/// Builds an SL runner from a specification, with the P4 builtins.
///
/// Takes a spec after lowering. Panics if a global definition is repeated.
pub fn build_sl<Ext: Extern>(
    spec: sl::ast::Spec,
    config: Config,
    external: Ext,
) -> Result<Runner<SlInterp, BuiltinInterface, Ext>, BuildError> {
    let spec = Spec::Sl(spec);
    // Builtins are chosen from the specification before it is consumed
    let interface = builtin::p4(&spec);
    let Spec::Sl(spec) = spec else { unreachable!() };
    // Load and prepare the definitions
    let global = SlGlobal::load(spec)?;
    let config = SlConfig::new(config.cache, config.det, config.guard);
    Ok(Runner::new(global, SlInterp::new(config), interface, external))
}

/// Builds a PL runner from a specification, with the P4 builtins.
///
/// Takes a spec after lowering. Panics if a global definition is repeated.
pub fn build_pl<Ext: Extern>(
    spec: pl::ast::Spec,
    config: Config,
    external: Ext,
) -> Result<Runner<PlInterp, BuiltinInterface, Ext>, BuildError> {
    let spec = Spec::Pl(spec);
    let interface = builtin::p4(&spec);
    let Spec::Pl(spec) = spec else { unreachable!() };
    let global = PlGlobal::load(spec)?;
    let config = PlConfig::new(config.cache, config.det, config.guard);
    Ok(Runner::new(global, PlInterp::new(config), interface, external))
}

// == Runner assembly

/// An interpreter and its host components sharing one value arena.
pub struct Runner<Interp, Iface, Ext>
where
    Interp: Interpreter<Iface, Ext>,
    Iface: Interface,
    Ext: Extern,
{
    arena: ValueArena,
    spec: Interp::Spec,
    interp: Interp,
    interface: Iface,
    external: Ext,
}

impl<Interp, Iface, Ext> Runner<Interp, Iface, Ext>
where
    Interp: Interpreter<Iface, Ext>,
    Iface: Interface,
    Ext: Extern,
{
    /// Assembles the components around a fresh arena.
    pub fn new(spec: Interp::Spec, interp: Interp, interface: Iface, external: Ext) -> Self {
        Self { arena: ValueArena::new(), spec, interp, interface, external }
    }

    /// Borrows the assembled components for a stage-specific evaluation entry.
    pub fn context(&mut self) -> RunnerContext<'_, Interp, Iface, Ext> {
        RunnerContext::new(
            &mut self.arena,
            &self.spec,
            &mut self.interp,
            &mut self.interface,
            &self.external,
        )
    }

    pub fn arena(&self) -> &ValueArena {
        &self.arena
    }

    pub fn arena_mut(&mut self) -> &mut ValueArena {
        &mut self.arena
    }

    // - Evaluation

    /// Runs the program entry through a fresh context.
    pub fn eval_program(
        &mut self,
        name: &str,
        program: Value,
    ) -> Result<Vec<Value>, InterpreterError> {
        let mut ctx = self.context();
        ctx.call_program(name, program)
    }

    // - Lifecycle

    /// Starts an independent program, keeping definitions and configuration.
    ///
    /// All previously returned arena handles become invalid.
    /// Call this before parsing the next program,
    /// after discarding the preceding program's values.
    pub fn reset(&mut self) {
        self.interp.reset();
        self.external.clear();
        self.interface.clear();
        self.arena = ValueArena::new();
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
fn run_program<Interp>(
    mut runner: Runner<Interp, BuiltinInterface, Dummy>,
    relation: &str,
    includes: &[PathBuf],
    path_p4: &Path,
) -> Result<(), RunError>
where
    Interp: Interpreter<BuiltinInterface, Dummy>,
{
    // Parse the program into the arena used by evaluation
    let program = parse_file(runner.arena_mut(), includes, path_p4).map_err(RunError::Parse)?;

    // Evaluate the entry without retaining arena-relative results
    runner
        .eval_program(relation, program)
        .map_err(RunError::Eval)?;
    Ok(())
}
