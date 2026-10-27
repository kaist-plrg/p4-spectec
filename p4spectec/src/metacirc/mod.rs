//! Configured interpreter towers over AL, IL, or P4 inputs
//!
//! Each runner is prepared in the arena used by all of its calls.
//! Only the base runs natively, on an `engine` executing AL or SL,
//! or a K library compiled from the base (ST-in-K);
//! each level above is interpreted by the level below through its `protocol`.
//! Calls go down the levels (`interp`), host calls come up across each
//! `boundary`, and a P4 extern that reenters runs on the next depth.
//! `tower` builds the levels, `sim` drives them with an STF test,
//! and `krun` runs a K definition on spec-k (K-in-ST).
//! The K parts are slots; `docs/metacirc/st-in-k.md` and `k-in-st.md`
//! say what each implementation fills.
//!
//! ```text
//! Tower
//! ├─ levels: [PreparedLevel]
//! ├─ protocols: [ProtocolKind]                          : Protocol
//! └─ BaseRunner
//!    └─ Upper                    ─lend→ UpperMut        : Extern
//!       ├─ Top                   ─lend→ TopMut
//!       └─ TowerInterpState      ─lend→ TowerInterp     : Interpreter
//!          └─ EngineStateKind    ─lend→ EngineKind      : Engine
//! ```

mod boundary;
pub mod config;
mod engine;
mod error;
mod interp;
mod protocol;
mod sim;
mod tower;

use crate::{
    interface::{self, p4::parse::parse_file},
    lang::data::{arena::Arena, value::flat as value},
    runner::{self, Interface},
    sim_plugin::dummy::Dummy,
};
use config::Lang;
pub use config::{ExecutionMode, TowerConfig};
use std::path::PathBuf;

pub use error::Result;

pub use sim::build_simulator;
pub use tower::Tower;

/// Runs a K definition from an initial term through a K-in-ST tower, as `krun` does.
///
/// `paths_program` are the definition (`definition.kore`)
/// and the initial term, both KORE text;
/// `depth` bounds the rewriting steps as `krun --depth` does.
/// Returns the run as `KoreInterface::print_runres` prints it.
pub fn krun(
    config: &TowerConfig,
    paths_program: &[PathBuf],
    options: runner::Config,
    depth: Option<u64>,
) -> Result<String> {
    if config.top_lang() != Lang::Kore {
        return Err(error::input_invalid("krun requires a tower whose top level accepts kore"));
    }
    if paths_program.len() != 2 {
        return Err(error::input_invalid(
            "a KORE top level requires a definition and an initial term",
        ));
    }
    let mut arena = Arena::new();
    let mut tower = Tower::build(&mut arena, config, options, Dummy)?;
    // Read the definition and the initial term as spec-k's values
    let mut interface = interface::kore();
    let value_program = interface
        .parse_program(&mut arena, &[], paths_program)
        .map_err(|error| error.into_report())?;
    let (definition, init) = value::get::tuple(&arena, &value_program)
        .and_then(value::get::two)
        .map(|(definition, init)| (*definition, *init))
        .map_err(|error| error::input_invalid(error.to_string()))?;
    // Run Krun on the base and print its runres
    let runres = tower.eval_krun(&mut arena, definition, init, depth)?;
    interface
        .print_runres(&arena, runres)
        .map_err(|error| error.into_report())
}

/// Executes a program through the configured interpreter tower.
pub fn run(
    config: &TowerConfig,
    includes: &[PathBuf],
    paths_program: &[PathBuf],
    options: runner::Config,
) -> Result<()> {
    if paths_program.is_empty() {
        return Err(error::input_invalid("a tower requires at least one program path"));
    }
    let lang_top = config.top_lang();
    // P4 inputs contain exactly one translation unit
    if lang_top == Lang::P4 && paths_program.len() != 1 {
        return Err(error::input_invalid("a P4 top level requires exactly one program path"));
    }
    let mut arena = Arena::new();
    let mut tower = Tower::build(&mut arena, config, options, Dummy)?;
    // Parse the input in the language the top level accepts
    let value_program = match lang_top {
        Lang::Al | Lang::Il => interface::meta(lang_top)
            .parse_program(&mut arena, includes, paths_program)
            .map_err(|error| error.into_report())?,
        Lang::P4 => parse_file(&mut arena, includes, &paths_program[0])
            .map_err(|error| error.into_report())?,
        // A KORE input enters spec-k through Krun, with a step bound
        Lang::Kore => {
            return Err(error::input_invalid("a KORE top level runs through krun"));
        }
    };
    // Encoding errors propagate before entering the native base
    let value = tower.patch(&mut arena, value_program)?;
    tower
        .eval_program(&mut arena, value)
        .result
        .map_err(|error| error.into_report())?;
    Ok(())
}
