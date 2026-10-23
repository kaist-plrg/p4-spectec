//! Preparation of target, intermediate, and base specifications
//!
//! Lowered specifications stay paired with their configured entry and input language.
//! Host helpers are assembled from target toward base;
//! patched scripts carry each inner entry to the interpreter consuming them.
//!
//! Construction and execution for `[base, interm, target]`:
//!
//! ```text
//! Tower::build(config) -- once          Tower::patch(program) -- per input
//!             |                                      |
//!       lower levels                        parsed program Value
//!             |                                      |
//!       build_target(target)                entry(target) + encode
//!             |                                      |
//!       Layer::Target                       encoded target script
//!             |                                      |
//!       build_interm(interm, layer)          entry(interm) + encode
//!             |                                      |
//!       Layer::Meta                         encoded intermediate script
//!             |                                      |
//!       build_base(base, layer)                      |
//!             |                                      |
//!       Tower { levels, base }                       Value
//!             |                                      |
//!             +-------------------+------------------+
//!                                 |
//!                        Tower::eval_program
//! ```
//!
//! Both chains run from target toward base; the base is never encoded.
//! Before each input, reset every runner and then the arena values before parsing.
//! A direct tower uses its target as the base and consumes the input unchanged.
//! AL input definitions supply the terminal builtin runner during construction.

use super::{
    ExecutionMode, Result, TowerConfig,
    config::{Lang, LevelConfigs, ProgramConfig},
    error::config_invalid,
    externs::{RelayExtern, TerminalExtern},
    tower::{Layer, Level, Target, Tower},
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

/// Prepares the input interface and its own terminal host environment.
fn build_target(
    arena: &mut Arena,
    target: &Level,
    mode: ExecutionMode,
    program: &ProgramConfig,
    options: runner::Config,
) -> Result<Target> {
    let spec_target = into_runner_spec(mode, target.spec_al.clone())?;
    match program {
        ProgramConfig::Al { spec_al } => {
            // Prepare the input script's definition environment for builtin calls
            let spec_program = into_runner_spec(mode, spec_al.clone())?;
            let runner_target =
                Runner::new(arena, spec_program, options, interface::al(), NullExtern)?;
            // Run the AL specification with builtin calls delegated to runner_target
            let runner = Runner::new(
                arena,
                spec_target,
                options,
                interface::al(),
                TerminalExtern {
                    cache: Box::new(Cache::new(options.cache)),
                    runner_target: Box::new(runner_target),
                },
            )?;
            Ok(Target::Al(runner))
        }
        ProgramConfig::P4 => {
            // Prepare the P4 host environment independently of program parsing
            let interface_target = interface::p4(&spec_target);
            let runner = Runner::new(arena, spec_target, options, interface_target, Dummy)?;
            Ok(Target::P4(runner))
        }
    }
}

/// Prepares an intermediate host helper above an already built layer.
fn build_interm(
    arena: &mut Arena,
    interm: &Level,
    layer: Layer,
    options: runner::Config,
) -> Result<Layer> {
    let spec = into_runner_spec(ExecutionMode::Sl, interm.spec_al.clone())?;
    let external =
        RelayExtern { cache: Box::new(Cache::new(options.cache)), runner_next: Box::new(layer) };
    Ok(Layer::Meta(Runner::new(arena, spec, options, interface::al(), external)?))
}

/// Prepares the native base with the requested interpreter mode.
fn build_base(
    arena: &mut Arena,
    base: &Level,
    mode: ExecutionMode,
    layer: Layer,
    options: runner::Config,
) -> Result<Layer> {
    let spec = into_runner_spec(mode, base.spec_al.clone())?;
    let external =
        RelayExtern { cache: Box::new(Cache::new(options.cache)), runner_next: Box::new(layer) };
    Ok(Layer::Meta(Runner::new(arena, spec, options, interface::al(), external)?))
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
        // The input environment must match the target language
        match (config.target_lang(), program) {
            (Lang::Al, ProgramConfig::Al { .. }) | (Lang::P4, ProgramConfig::P4) => {}
            _ => return Err(config_invalid("program language must match the tower target")),
        }
        // Lower each specification once, preserving base-to-target order
        let configs = match &config.levels {
            LevelConfigs::Direct(target) => vec![target],
            LevelConfigs::Nested { base, interms, target } => std::iter::once(base)
                .chain(interms.iter())
                .chain(std::iter::once(target))
                .collect(),
        };
        let levels = configs
            .into_iter()
            .enumerate()
            .map(|(idx, config)| {
                // Patching enters every nested level by its own relation
                let rel = config.rel.clone().ok_or_else(|| {
                    config_invalid(format!(
                        "level {idx} requires an entry relation in a nested tower"
                    ))
                })?;
                let spec_al = crate::algo(std::slice::from_ref(&config.specdir))?;
                Ok(Level { rel, spec_al })
            })
            .collect::<Result<Vec<_>>>()?;

        // Only a direct target uses the configured native execution mode
        let (target, metas) = levels.split_last().expect("validated nonempty tower");
        let mode_target = if metas.is_empty() { config.mode } else { ExecutionMode::Sl };
        let runner_target = build_target(arena, target, mode_target, program, options)?;
        let mut layer = Layer::Target(runner_target);

        // Assemble intermediate helpers and the native base above the target
        if let Some((base, interms)) = metas.split_first() {
            for interm in interms.iter().rev() {
                layer = build_interm(arena, interm, layer, options)?;
            }
            layer = build_base(arena, base, config.mode, layer, options)?;
        }
        Ok(Self { levels, base: layer })
    }
}
