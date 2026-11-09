//! Configured interpreter towers over AL, IL, or P4 inputs
//!
//! Each runner is prepared in the arena used by all of its calls.
//! Only the base runs natively, on an `engine` executing AL or SL,
//! a K library compiled from the base (ST-in-K),
//! or a `racket` process running a PLT Redex base (ST-in-Redex);
//! each level above is interpreted by the level below through its `protocol`.
//! Calls go down the levels (`interp`), host calls come up across each
//! `boundary`, and a P4 extern that reenters runs on the next depth.
//! `tower` builds the levels, `sim` drives them with an STF test,
//! and `run` runs a program through a tower, a K definition on spec-k included (K-in-ST).
//! `cover` measures the SL instruction coverage of an SL base over many P4 inputs.
//! Each backend is its own module, sharing nothing with the others:
//! `st` runs P4-SpecTec specifications (spec-meta, spec-k),
//! `k` spec-meta-k from its K library, and `redex` spec-meta-redex in `racket`.
//!
//! ```text
//! Tower
//! ├─ levels: [PreparedLevel]
//! ├─ protocols: [ProtocolKind]                          : Protocol
//! └─ Base
//!    └─ Upper                    ─lend→ UpperMut        : Extern
//!       ├─ Top                   ─lend→ TopMut
//!       └─ TowerInterpState      ─lend→ TowerInterp     : Interpreter
//!          └─ EngineStateKind    ─lend→ EngineKind      : Engine
//! ```

mod boundary;
pub mod config;
pub mod cover;
mod engine;
mod error;
mod interp;
mod k;
mod protocol;
mod redex;
mod sim;
mod st;
mod tower;

use crate::{lang::data::arena::Arena, runner, sim_plugin::dummy::Dummy};
pub use config::{BaseConfig, Repr, TowerConfig};
use std::path::PathBuf;

pub use error::Result;

pub use sim::build_simulator;
pub use tower::{Input, Kore, Script, Tower};

/// Executes a program through the configured interpreter tower.
///
/// `paths_program` are the program in the language the top level accepts:
/// one P4 program, the sources of an AL or IL script, or for spec-k a K definition
/// (`definition.kore`) and an initial term, both KORE text.
/// `depth` bounds the rewriting steps of a K definition, as `krun --depth` does.
/// Returns what the top prints of the run, the run of a K definition as krun
/// prints it, or nothing for any other top.
pub fn run(
    config: &TowerConfig,
    includes: &[PathBuf],
    paths_program: &[PathBuf],
    options: runner::Config,
    depth: Option<u64>,
) -> Result<Option<String>> {
    let mut arena = Arena::new();
    let mut tower = Tower::build(&mut arena, config, options, Dummy)?;
    // Read the input in the language the top level accepts;
    // encoding errors propagate before entering the native base
    let input = tower.parse_input(&mut arena, includes, paths_program, depth)?;
    let values = tower.encode_input(&mut arena, &input)?;
    let outputs = tower
        .eval_input(&mut arena, &values)
        .map_err(|error| error.into_report())?;
    tower.print_output(&arena, &outputs)
}
