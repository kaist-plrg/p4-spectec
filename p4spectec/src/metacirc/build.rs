//! Preparation of top, intermediate, and base levels
//!
//! The AL of each specification stays paired with its configured entry and input language.
//! Layers are assembled from the top level toward the base;
//! patched scripts carry each inner entry to the interpreter consuming them.
//!
//! Construction and execution for `[base, interm, top]`:
//!
//! ```text
//! Tower::build(config) -- once          Tower::patch(program) -- per input
//!             |                                      |
//!       algo levels                         parsed program Value
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
    interface::{self, meta::shared::Cache},
    lang::{al::ast as al, data::arena::Arena, il::ast as il},
    pass::structure,
    phrase,
    runner::{self, NullExtern, Runner, Spec},
    sim_plugin::dummy::Dummy,
};

// == Runner construction

/// Converts the AL of a specification into the representation selected for the runner.
fn into_runner_spec(mode: ExecutionMode, spec_al: al::Spec) -> Result<Spec> {
    match mode {
        ExecutionMode::Al => Ok(Spec::Al(spec_al)),
        ExecutionMode::Sl => structure::convert(spec_al, true).map(Spec::Sl),
    }
}

/// The type definitions and builtin declarations of an IL program, as AL.
///
/// Both are shared by IL and AL, so no rule or clause is converted;
/// a runner of the result answers the IL program's builtin calls.
fn builtins_of_il(spec_il: &il::Spec) -> al::Spec {
    spec_il
        .iter()
        .filter_map(|def_il| {
            let def_kind_al = match &def_il.node {
                il::DefKind::Typ(typ_def) => al::DefKind::Typ(typ_def.clone()),
                il::DefKind::MetaFunc(il::MetaFuncDef::Builtin(builtin_func)) => {
                    al::DefKind::MetaFunc(al::MetaFuncDef::Builtin(builtin_func.clone()))
                }
                _ => return None,
            };
            Some(phrase!(node: def_kind_al, span: def_il.span))
        })
        .collect()
}

/// Prepares an AL top level whose builtin calls are answered by the program's definitions.
fn build_terminal(
    arena: &mut Arena,
    spec_top: Spec,
    spec_al_program: al::Spec,
    mode: ExecutionMode,
    options: runner::Config,
) -> Result<Layer> {
    // Prepare the input script's definition environment for builtin calls
    let spec_program = into_runner_spec(mode, spec_al_program)?;
    let runner_program = Runner::new(arena, spec_program, options, interface::al(), NullExtern)?;
    // Run the AL specification with builtin calls delegated to runner_program
    let external = AlExtern::Terminal(TerminalExtern {
        cache: Box::new(Cache::new(options.cache)),
        runner_program: Box::new(runner_program),
    });
    let runner = Runner::new(arena, spec_top, options, interface::al(), external)?;
    Ok(Layer::AlSpec(runner))
}

/// Prepares the top level, hosting the AL or IL program or taking a P4 program.
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
            build_terminal(arena, spec_top, spec_al.clone(), mode, options)
        }
        // An IL program needs only its builtin declarations to answer builtin calls
        ProgramConfig::Il { spec_il } => {
            build_terminal(arena, spec_top, builtins_of_il(spec_il), mode, options)
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
            (Lang::Al, ProgramConfig::Al { .. })
            | (Lang::Il, ProgramConfig::Il { .. })
            | (Lang::P4, ProgramConfig::P4) => {}
            _ => {
                return Err(config_invalid(
                    "program language must match the top level of the tower",
                ));
            }
        }
        // Convert each specification to AL once, preserving base-to-top order
        let levels = config
            .levels
            .iter()
            .enumerate()
            .map(|(idx, level)| {
                let paths = std::slice::from_ref(&level.specdir);
                let spec_al = crate::algo(paths)?;
                // A level accepting IL receives the IL of the level above
                let spec_il = match idx
                    .checked_sub(1)
                    .map(|idx_below| config.levels[idx_below].lang)
                {
                    Some(Lang::Il) => Some(crate::elab(paths)?),
                    _ => None,
                };
                Ok(Level { config: level.clone(), spec_al, spec_il })
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
