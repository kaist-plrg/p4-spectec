//! Preparation of top, intermediate, and base levels
//!
//! Lowered specifications stay paired with their configured entry and input language.
//! Layers are assembled from the top level toward the base;
//! patched scripts carry each inner entry to the interpreter consuming them.
//!
//! Construction and execution for `[base, interm, top]`:
//!
//! ```text
//! Tower::build(config) -- once          Tower::patch(program) -- per input
//!             |                                      |
//!       lower levels                        parsed program Value
//!             |                                      |
//!       build_top(top)                      entry(top) + encode
//!             |                                      |
//!       Layer::AlSpec or Layer::P4Spec      encoded top script
//!             |                                      |
//!       build_relay(interm, layer)          entry(interm) + encode
//!             |                                      |
//!       Layer::AlSpec                       encoded intermediate script
//!             |                                      |
//!       build_relay(base, layer)                     |
//!             |                                      |
//!       Tower { levels, base }                       Value
//!             |                                      |
//!             +-------------------+------------------+
//!                                 |
//!                        Tower::eval_program
//! ```
//!
//! Both chains run from top toward base; the base is never encoded.
//! Before each input, reset every runner and then the arena values before parsing.
//! A single-level tower uses its top level as the base and consumes the input unchanged.
//! AL input definitions supply the terminal builtin runner during construction.

use super::{
    ExecutionMode, Result, TowerConfig,
    config::{Lang, ProgramConfig},
    error::config_invalid,
    externs::{AlExtern, RelayExtern, TerminalExtern},
    tower::{Layer, Level, Tower},
};
use crate::{
    interface::{self, al::Cache},
    lang::{al::ast as al, data::arena::Arena},
    pass::structure,
    runner::{self, NullExtern, Runner, Spec},
    sim_plugin::dummy::Dummy,
};

// == Runner construction

/// Converts lowered AL into the representation selected for the runner.
fn into_runner_spec(mode: ExecutionMode, spec_al: al::Spec) -> Result<Spec> {
    match mode {
        ExecutionMode::Al => Ok(Spec::Al(spec_al)),
        ExecutionMode::Sl => structure::convert(spec_al, true).map(Spec::Sl),
    }
}

/// Prepares the top level, hosting the AL program or taking a P4 program.
fn build_top(
    arena: &mut Arena,
    top: &Level,
    mode: ExecutionMode,
    program: &ProgramConfig,
    options: runner::Config,
) -> Result<Layer> {
    let spec_top = into_runner_spec(mode, top.spec_al.clone())?;
    match program {
        ProgramConfig::Al { spec_al } => {
            // Prepare the input script's definition environment for builtin calls
            let spec_program = into_runner_spec(mode, spec_al.clone())?;
            let runner_program =
                Runner::new(arena, spec_program, options, interface::al(), NullExtern)?;
            // Run the AL specification with builtin calls delegated to runner_program
            let external = AlExtern::Terminal(TerminalExtern {
                cache: Box::new(Cache::new(options.cache)),
                runner_program: Box::new(runner_program),
            });
            let runner = Runner::new(arena, spec_top, options, interface::al(), external)?;
            Ok(Layer::AlSpec(runner))
        }
        ProgramConfig::P4 => {
            // Prepare the P4 host environment independently of program parsing
            let interface_top = interface::p4(&spec_top);
            let runner = Runner::new(arena, spec_top, options, interface_top, Dummy)?;
            Ok(Layer::P4Spec(runner))
        }
    }
}

/// Prepares a level beneath an already built layer, relaying host calls to it.
fn build_relay(
    arena: &mut Arena,
    level: &Level,
    mode: ExecutionMode,
    layer_next: Layer,
    options: runner::Config,
) -> Result<Layer> {
    let spec = into_runner_spec(mode, level.spec_al.clone())?;
    let external = AlExtern::Relay(RelayExtern {
        cache: Box::new(Cache::new(options.cache)),
        layer_next: Box::new(layer_next),
    });
    Ok(Layer::AlSpec(Runner::new(arena, spec, options, interface::al(), external)?))
}

// == Tower assembly

impl Tower {
    /// Prepares a tower without parsing or encoding its input.
    ///
    /// The program supplies AL input definitions for terminal builtin calls.
    /// All calls must use the construction arena.
    pub fn build(
        arena: &mut Arena,
        config: &TowerConfig,
        program: &ProgramConfig,
        options: runner::Config,
    ) -> Result<Self> {
        // The input environment must match the top level's language
        match (config.top_lang(), program) {
            (Lang::Al, ProgramConfig::Al { .. }) | (Lang::P4, ProgramConfig::P4) => {}
            _ => {
                return Err(config_invalid(
                    "program language must match the top level of the tower",
                ));
            }
        }
        // Lower each specification once, preserving base-to-top order;
        // the IL-to-AL specification, if configured, lowers a P4 top level
        let idx_top = config.levels.len() - 1;
        let levels = config
            .levels
            .iter()
            .enumerate()
            .map(|(idx, level)| {
                let paths = std::slice::from_ref(&level.specdir);
                // Only a P4 top level is the specification under test
                let spec_al = if idx == idx_top && level.lang == Lang::P4 {
                    config.lower(paths, options)?
                } else {
                    crate::algo(paths)?
                };
                Ok(Level { config: level.clone(), spec_al })
            })
            .collect::<Result<Vec<_>>>()?;

        // Only the base uses the configured native execution mode
        let mode = |idx| if idx == 0 { config.mode } else { ExecutionMode::Sl };
        let (top, levels_lower) = levels.split_last().expect("validated nonempty tower");
        let mut layer = build_top(arena, top, mode(levels_lower.len()), program, options)?;

        // Assemble the levels beneath the top level down to the native base
        for (idx, level) in levels_lower.iter().enumerate().rev() {
            layer = build_relay(arena, level, mode(idx), layer, options)?;
        }
        Ok(Self { levels, base: layer })
    }
}
