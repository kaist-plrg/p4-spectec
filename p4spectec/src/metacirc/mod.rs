//! Configured interpreter towers over AL, IL, or P4 inputs
//!
//! Each runner is prepared in the arena used by all of its calls.
//! Only the base runs natively, on an `engine` executing AL or SL;
//! each level above is interpreted by the level below through its `protocol`.
//! Calls go down the levels (`interp`), host calls come up across each
//! `boundary`, and a P4 extern that reenters runs on the next depth.
//! `tower` builds the levels, `sim` drives them with an STF test.
//!
//! ```text
//! Tower
//! ├─ levels: [PreparedLevel]
//! ├─ protocols: [ProtocolKind]                          : Protocol
//! └─ BaseRunner
//!    └─ Upper                    ─lend→ UpperMut        : Extern
//!       ├─ Top                   ─lend→ TopMut
//!       └─ TowerInterpState      ─lend→ TowerInterp     : Interpreter
//!          └─ SpecTecEngineState ─lend→ SpecTecEngine   : Engine
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
    lang::data::arena::Arena,
    runner::{self, Interface},
    sim_plugin::dummy::Dummy,
};
use config::Lang;
pub use config::{ExecutionMode, TowerConfig};
use std::path::PathBuf;

pub use error::Result;

pub use sim::build_simulator;
pub use tower::Tower;

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
    };
    // Encoding errors propagate before entering the native base
    let values = tower.encode_input(&mut arena, value_program)?;
    tower
        .eval_input(&mut arena, &values)
        .map_err(|error| error.into_report())?;
    Ok(())
}
