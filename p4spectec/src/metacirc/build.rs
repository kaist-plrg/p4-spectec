//! Construction of a tower's levels and layers
//!
//! ```text
//!   +-------------+-------------+
//!   | Builtin P4  | Extern P4   |   top     Layer::P4Spec, Dummy
//!   +-------------+-------------+
//!   |      Interp(top AL)       |
//!   +---------------------------+
//!                 ^ layer_next
//!   +-------------+-------------+
//!   | Builtin ST  | Extern ST   |   interm  Layer::MetaSpec, RelayExtern
//!   +-------------+-------------+
//!   |     Interp(interm AL)     |
//!   +---------------------------+
//!                 ^ layer_next
//!   +-------------+-------------+
//!   | Builtin ST  | Extern ST   |   base    Layer::MetaSpec, RelayExtern
//!   +-------------+-------------+
//!   |  Interp(base AL): entry   |
//!   +---------------------------+
//! ```
//!
//! Only the base runs an entry; the others answer calls relayed from below.
//! An AL or IL top ends the chain at its program:
//!
//! ```text
//!   +-------------+-------------+
//!   | Builtin     | NullExtern  |   runner_program
//!   +-------------+-------------+
//!   | Interp(program builtins)  |
//!   +---------------------------+
//!                 ^ builtin calls only
//!   +-------------+-------------+
//!   | Builtin ST  | Extern ST   |   top     Layer::MetaSpec, TerminalExtern
//!   +-------------+-------------+
//!   |      Interp(top AL)       |
//!   +---------------------------+
//! ```
//!
//! | level below accepts | `Level`   | patched into the level below |
//! |---------------------|-----------|------------------------------|
//! | none (base)         | `Base`    | —                            |
//! | `al`                | `AboveAl` | AL                           |
//! | `il`                | `AboveIl` | IL                           |

use super::{
    ExecutionMode, Result, TowerConfig,
    config::{Lang, ProgramConfig},
    error::config_invalid,
    externs::{MetaExtern, RelayExtern, TerminalExtern},
    tower::{Layer, Level, Tower},
};
use crate::{
    interface::{
        self,
        meta::{ScriptLang, shared::Cache},
    },
    lang::{al::ast as al, data::arena::Arena},
    pass::{algo, structure},
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

/// The script language of a meta level accepting `lang`.
fn script_lang(lang: Lang) -> Result<ScriptLang> {
    match lang {
        Lang::Al => Ok(ScriptLang::Al),
        Lang::Il => Ok(ScriptLang::Il),
        Lang::P4 => Err(config_invalid("a meta level cannot accept P4")),
    }
}

/// Prepares a meta top level whose builtin calls are answered by the program's definitions.
fn build_terminal(
    arena: &mut Arena,
    spec_top: Spec,
    lang_top: ScriptLang,
    spec_program_al: al::Spec,
    mode: ExecutionMode,
    options: runner::Config,
) -> Result<Layer> {
    // Prepare the input script's definition environment for builtin calls;
    // this runner holds AL, whatever the top level accepts
    let spec_program = into_runner_spec(mode, spec_program_al)?;
    let interface_program = interface::meta(ScriptLang::Al);
    let runner_program = Runner::new(arena, spec_program, options, interface_program, NullExtern)?;
    // Run the meta specification with builtin calls delegated to runner_program
    let external = MetaExtern::Terminal(TerminalExtern {
        cache: Box::new(Cache::new(options.cache)),
        runner_program: Box::new(runner_program),
    });
    let runner_top = Runner::new(arena, spec_top, options, interface::meta(lang_top), external)?;
    Ok(Layer::MetaSpec(runner_top))
}

/// Prepares the top level, hosting the AL or IL program or taking a P4 program.
fn build_top(
    arena: &mut Arena,
    spec_top_al: al::Spec,
    mode: ExecutionMode,
    program: &ProgramConfig,
    options: runner::Config,
) -> Result<Layer> {
    let spec_top = into_runner_spec(mode, spec_top_al)?;
    match program {
        ProgramConfig::Al { spec_al: spec_program_al } => {
            build_terminal(arena, spec_top, ScriptLang::Al, spec_program_al.clone(), mode, options)
        }
        // An IL program needs only its builtin declarations to answer builtin calls
        ProgramConfig::Il { spec_il: spec_program_il } => {
            let spec_program_al = algo::convert_builtins(spec_program_il);
            build_terminal(arena, spec_top, ScriptLang::Il, spec_program_al, mode, options)
        }
        ProgramConfig::P4 => {
            // Prepare the P4 host environment independently of program parsing
            let interface_top = interface::p4(&spec_top);
            let runner_top = Runner::new(arena, spec_top, options, interface_top, Dummy)?;
            Ok(Layer::P4Spec(runner_top))
        }
    }
}

/// Prepares a level beneath an already built layer, relaying host calls to it.
fn build_relay(
    arena: &mut Arena,
    spec_level_al: al::Spec,
    lang_level: Lang,
    mode: ExecutionMode,
    layer_next: Layer,
    options: runner::Config,
) -> Result<Layer> {
    let spec_level = into_runner_spec(mode, spec_level_al)?;
    let interface_level = interface::meta(script_lang(lang_level)?);
    let external = MetaExtern::Relay(RelayExtern {
        cache: Box::new(Cache::new(options.cache)),
        layer_next: Box::new(layer_next),
    });
    Ok(Layer::MetaSpec(Runner::new(arena, spec_level, options, interface_level, external)?))
}

// == Tower assembly

impl Tower {
    /// Prepares a tower without parsing or encoding its input.
    ///
    /// The program supplies the definitions answering the top level's builtin calls.
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
        // Convert each specification once, from base to top, into its Level and its runner's AL;
        // the level below decides whether a level is passed as AL or IL
        let (levels, mut specs_level_al): (Vec<_>, Vec<_>) = config
            .levels
            .iter()
            .enumerate()
            .map(|(idx, config_level)| {
                let paths = std::slice::from_ref(&config_level.specdir);
                let lang_below = idx
                    .checked_sub(1)
                    .map(|idx_below| config.levels[idx_below].lang);
                Ok(match lang_below {
                    None => (Level::Base { config: config_level.clone() }, crate::algo(paths)?),
                    Some(Lang::Al) => {
                        let spec_level_al = crate::algo(paths)?;
                        let level = Level::AboveAl {
                            config: config_level.clone(),
                            spec_al: spec_level_al.clone(),
                        };
                        (level, spec_level_al)
                    }
                    // Elaborate once for the native runner and for the IL passed below
                    Some(Lang::Il) => {
                        let spec_level_il = crate::elab(paths)?;
                        let spec_level_al = algo::convert(spec_level_il.clone())?;
                        let level =
                            Level::AboveIl { config: config_level.clone(), spec_il: spec_level_il };
                        (level, spec_level_al)
                    }
                    Some(Lang::P4) => {
                        return Err(config_invalid(format!(
                            "level {} accepts P4 below the top level",
                            idx - 1
                        )));
                    }
                })
            })
            .collect::<Result<Vec<_>>>()?
            .into_iter()
            .unzip();

        // Only the base uses the configured native execution mode
        let mode = |idx| if idx == 0 { config.mode } else { ExecutionMode::Sl };
        let spec_top_al = specs_level_al.pop().expect("validated nonempty tower");
        let mut layer =
            build_top(arena, spec_top_al, mode(specs_level_al.len()), program, options)?;

        // Assemble the levels beneath the top level down to the native base
        for (idx, spec_level_al) in specs_level_al.into_iter().enumerate().rev() {
            let lang_level = config.levels[idx].lang;
            layer = build_relay(arena, spec_level_al, lang_level, mode(idx), layer, options)?;
        }
        Ok(Self { levels, base: layer })
    }
}
