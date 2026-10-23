//! Construction of a tower's levels and layers
//!
//! ```text
//!   +-------------+-------------+
//!   | Builtin P4  | Extern P4   |   top     Layer::P4Spec, Dummy
//!   +-------------+-------------+
//!   |      no interpreter       |
//!   +---------------------------+
//!                 ^ layer_next
//!   +-------------+-------------+
//!   | Builtin ST  | Extern ST   |   interm  Layer::MetaSpec, RelayExtern
//!   +-------------+-------------+
//!   |      no interpreter       |
//!   +---------------------------+
//!                 ^ layer_next
//!   +-------------+-------------+
//!   | Builtin ST  | Extern ST   |   base    Layer::MetaSpec, RelayExtern
//!   +-------------+-------------+
//!   |  Interp(base AL): entry   |
//!   +---------------------------+
//! ```
//!
//! Only the base runs natively.
//! A level above it is interpreted by the level below,
//! so its layer has no interpreter and answers relayed builtin and host calls.
//! An AL or IL top ends the chain at its program:
//!
//! ```text
//!   +-------------+-------------+
//!   | Builtin     | NullExtern  |   runner_program
//!   +-------------+-------------+
//!   |      no interpreter       |
//!   +---------------------------+
//!                 ^ builtin calls only
//!   +-------------+-------------+
//!   | Builtin ST  | Extern ST   |   top     Layer::MetaSpec, TerminalExtern
//!   +-------------+-------------+
//!   | Interp(top AL) if base    |
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
        p4::unparse::P4Unparser,
    },
    lang::{al::ast as al, data::arena::Arena},
    pass::structure,
    runner::{self, Extern, Interface, NullExtern, Runner, Spec},
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

/// Prepares a level's runner, which runs natively only at the base.
///
/// Only the base has `spec_runner_al`.
/// A level above it is interpreted by the level below,
/// so its runner has no interpreter and answers builtin and host calls only.
fn build_runner<Iface: Interface, Ext: Extern>(
    arena: &mut Arena,
    spec_runner_al: Option<al::Spec>,
    mode: ExecutionMode,
    options: runner::Config,
    interface: Iface,
    external: Ext,
) -> Result<Runner<Iface, Ext>> {
    match spec_runner_al {
        // The base executes its specification natively
        Some(spec_runner_al) => {
            let spec_runner = into_runner_spec(mode, spec_runner_al)?;
            Ok(Runner::new(arena, spec_runner, options, interface, external)?)
        }
        // A level above the base has no native interpreter
        None => Ok(Runner::host(interface, external)),
    }
}

/// Prepares a meta top level whose program's builtins go to an interface.
fn build_terminal(
    arena: &mut Arena,
    spec_runner_al: Option<al::Spec>,
    lang_top: ScriptLang,
    mode: ExecutionMode,
    options: runner::Config,
) -> Result<Layer> {
    // The meta specification interprets the program; nothing runs it natively
    let runner_program = Runner::host(interface::meta(ScriptLang::Al), NullExtern);
    // Run the meta specification with builtin calls delegated to runner_program
    let external = MetaExtern::Terminal(TerminalExtern {
        cache: Box::new(Cache::new(options.cache)),
        runner_program: Box::new(runner_program),
    });
    let interface_top = interface::meta(lang_top);
    let runner_top = build_runner(arena, spec_runner_al, mode, options, interface_top, external)?;
    Ok(Layer::MetaSpec(runner_top))
}

/// Prepares the top level, hosting the AL or IL program or taking a P4 program.
fn build_top(
    arena: &mut Arena,
    level_top: &Level,
    spec_runner_al: Option<al::Spec>,
    mode: ExecutionMode,
    program: &ProgramConfig,
    options: runner::Config,
) -> Result<Layer> {
    match program {
        ProgramConfig::Al { .. } => {
            build_terminal(arena, spec_runner_al, ScriptLang::Al, mode, options)
        }
        ProgramConfig::Il { .. } => {
            build_terminal(arena, spec_runner_al, ScriptLang::Il, mode, options)
        }
        ProgramConfig::P4 => {
            // Read print hints from the base's AL or from the form passed below
            let unparser = match (&spec_runner_al, level_top) {
                (Some(spec_al), _) | (None, Level::AboveAl { spec_al, .. }) => {
                    P4Unparser::from_al_spec(spec_al)
                }
                (None, Level::AboveIl { spec_il, .. }) => P4Unparser::from_il_spec(spec_il),
                (None, Level::Base { .. }) => unreachable!("the base has a native AL"),
            };
            // Prepare the P4 host environment independently of program parsing
            let interface_top = interface::p4_with_unparser(unparser);
            let runner_top =
                build_runner(arena, spec_runner_al, mode, options, interface_top, Dummy)?;
            Ok(Layer::P4Spec(runner_top))
        }
    }
}

/// Prepares a level beneath an already built layer, relaying host calls to it.
fn build_relay(
    arena: &mut Arena,
    spec_runner_al: Option<al::Spec>,
    lang_level: Lang,
    mode: ExecutionMode,
    layer_next: Layer,
    options: runner::Config,
) -> Result<Layer> {
    let interface_level = interface::meta(script_lang(lang_level)?);
    let external = MetaExtern::Relay(RelayExtern {
        cache: Box::new(Cache::new(options.cache)),
        layer_next: Box::new(layer_next),
    });
    let runner_level =
        build_runner(arena, spec_runner_al, mode, options, interface_level, external)?;
    Ok(Layer::MetaSpec(runner_level))
}

// == Tower assembly

impl Tower {
    /// Prepares a tower without parsing or encoding its input.
    ///
    /// Only the base is converted for a native runner;
    /// every other level is prepared in the form the level below accepts.
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
        // Prepare each specification once, from base to top, into its Level
        // and, at the base only, the AL its native runner executes
        let (mut levels, mut specs_runner_al): (Vec<_>, Vec<_>) = config
            .levels
            .iter()
            .enumerate()
            .map(|(idx, config_level)| {
                let paths = std::slice::from_ref(&config_level.specdir);
                let lang_below = idx
                    .checked_sub(1)
                    .map(|idx_below| config.levels[idx_below].lang);
                Ok(match lang_below {
                    None => {
                        (Level::Base { config: config_level.clone() }, Some(crate::algo(paths)?))
                    }
                    Some(Lang::Al) => {
                        let spec_level_al = crate::algo(paths)?;
                        let level =
                            Level::AboveAl { config: config_level.clone(), spec_al: spec_level_al };
                        (level, None)
                    }
                    Some(Lang::Il) => {
                        let spec_level_il = crate::elab(paths)?;
                        let level =
                            Level::AboveIl { config: config_level.clone(), spec_il: spec_level_il };
                        (level, None)
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

        let level_top = levels.pop().expect("validated nonempty tower");
        let spec_top_runner_al = specs_runner_al.pop().expect("validated nonempty tower");
        let mut layer =
            build_top(arena, &level_top, spec_top_runner_al, config.mode, program, options)?;
        levels.push(level_top);

        // Assemble the levels beneath the top level down to the native base
        for (idx, spec_runner_al) in specs_runner_al.into_iter().enumerate().rev() {
            let lang_level = config.levels[idx].lang;
            layer = build_relay(arena, spec_runner_al, lang_level, config.mode, layer, options)?;
        }
        Ok(Self { levels, base: layer })
    }
}
