//! Construction of a tower's levels and layers
//!
//! ```text
//!   +-------------+-------------+
//!   | Builtin P4  | Extern P4   |   top     HostLayer::P4Spec, Dummy
//!   +-------------+-------------+
//!                 ^ layer_next
//!   +-------------+-------------+
//!   | Builtin ST  | Extern ST   |   interm  HostLayer::MetaSpec, RelayExtern
//!   +-------------+-------------+
//!                 ^ layer_next
//!   +-------------+-------------+
//!   | Builtin ST  | Extern ST   |   base    BaseLayer::MetaSpec, RelayExtern
//!   +-------------+-------------+
//!   |  Interp(base AL): entry   |
//!   +---------------------------+
//! ```
//!
//! Only the base runs natively, as a runner with an interpreter.
//! A level above it is interpreted by the level below,
//! so it is a host that answers relayed builtin and host calls.
//! An AL or IL top ends the chain at its program, whose builtins
//! go to the program's interface:
//!
//! ```text
//!   +-------------+
//!   | Builtin     |                 interface_program
//!   +-------------+
//!                 ^ builtin calls only
//!   +-------------+-------------+
//!   | Builtin ST  | Extern ST   |   top     MetaSpec, TerminalExtern
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
    config::Lang,
    error::config_invalid,
    externs::{MetaExtern, RelayExtern, TerminalExtern},
    tower::{BaseLayer, HostLayer, Level, Tower},
};
use crate::{
    interface::{
        self,
        meta::{MetaInterface, shared::Cache},
        p4::{P4Interface, unparse::P4Unparser},
    },
    lang::{al::ast as al, data::arena::Arena},
    pass::structure,
    runner::{self, Host, Runner, Spec},
    sim_plugin::dummy::Dummy,
};

// == Level construction

/// Converts the AL of a specification into the representation selected for the runner.
fn into_runner_spec(mode: ExecutionMode, spec_al: al::Spec) -> Result<Spec> {
    match mode {
        ExecutionMode::Al => Ok(Spec::Al(spec_al)),
        ExecutionMode::Sl => structure::convert(spec_al, true).map(Spec::Sl),
    }
}

/// A level's interface and extern, before it becomes the base runner or a host.
enum Parts {
    P4Spec(P4Interface, Dummy),
    MetaSpec(MetaInterface, MetaExtern),
}

impl Parts {
    /// Makes a level above the base, which the level below interprets.
    fn into_host(self) -> HostLayer {
        match self {
            Self::P4Spec(interface, external) => HostLayer::P4Spec(Host::new(interface, external)),
            Self::MetaSpec(interface, external) => {
                HostLayer::MetaSpec(Host::new(interface, external))
            }
        }
    }

    /// Makes the base, which executes its specification natively.
    fn into_base(
        self,
        arena: &mut Arena,
        spec_base_al: al::Spec,
        mode: ExecutionMode,
        options: runner::Config,
    ) -> Result<BaseLayer> {
        let spec_base = into_runner_spec(mode, spec_base_al)?;
        Ok(match self {
            Self::P4Spec(interface, external) => {
                BaseLayer::P4Spec(Runner::new(arena, spec_base, options, interface, external)?)
            }
            Self::MetaSpec(interface, external) => {
                BaseLayer::MetaSpec(Runner::new(arena, spec_base, options, interface, external)?)
            }
        })
    }
}

/// Prepares a meta top level whose program's builtins go to an interface.
fn build_terminal(lang_top: Lang, options: runner::Config) -> Parts {
    // The meta specification interprets the program; nothing runs it natively
    let external = MetaExtern::Terminal(TerminalExtern {
        cache: Box::new(Cache::new(options.cache)),
        interface_program: Box::new(interface::meta(Lang::Al)),
    });
    Parts::MetaSpec(interface::meta(lang_top), external)
}

/// Prepares the top level, hosting the AL or IL program or taking a P4 program.
fn build_top(
    level_top: &Level,
    spec_base_al: &al::Spec,
    lang_top: Lang,
    options: runner::Config,
) -> Parts {
    match lang_top {
        Lang::Al | Lang::Il => build_terminal(lang_top, options),
        Lang::P4 => {
            // Read print hints from the form the top level is prepared in
            let unparser = match level_top {
                Level::Base { .. } => P4Unparser::from_al_spec(spec_base_al),
                Level::AboveAl { spec_al, .. } => P4Unparser::from_al_spec(spec_al),
                Level::AboveIl { spec_il, .. } => P4Unparser::from_il_spec(spec_il),
            };
            // Prepare the P4 host environment independently of program parsing
            Parts::P4Spec(interface::p4_with_unparser(unparser), Dummy)
        }
    }
}

/// Prepares a level beneath an already built host, relaying host calls to it.
fn build_relay(lang_level: Lang, layer_next: HostLayer, options: runner::Config) -> Parts {
    let external = MetaExtern::Relay(RelayExtern {
        cache: Box::new(Cache::new(options.cache)),
        layer_next: Box::new(layer_next),
    });
    Parts::MetaSpec(interface::meta(lang_level), external)
}

// == Tower assembly

impl Tower {
    /// Prepares a tower without parsing or encoding its input.
    ///
    /// Only the base is converted for a native runner;
    /// every other level is prepared in the form the level below accepts.
    /// All calls must use the construction arena.
    pub fn build(arena: &mut Arena, config: &TowerConfig, options: runner::Config) -> Result<Self> {
        // Prepare each specification once, from base to top, into its Level;
        // the base is also converted for its native runner
        let levels = config
            .levels
            .iter()
            .enumerate()
            .map(|(idx, config_level)| {
                let paths = std::slice::from_ref(&config_level.specdir);
                let lang_below = idx
                    .checked_sub(1)
                    .map(|idx_below| config.levels[idx_below].lang);
                Ok(match lang_below {
                    None => Level::Base { config: config_level.clone() },
                    Some(Lang::Al) => Level::AboveAl {
                        config: config_level.clone(),
                        spec_al: crate::algo(paths)?,
                    },
                    Some(Lang::Il) => Level::AboveIl {
                        config: config_level.clone(),
                        spec_il: crate::elab(paths)?,
                    },
                    Some(Lang::P4) => {
                        return Err(config_invalid(format!(
                            "level {} accepts P4 below the top level",
                            idx - 1
                        )));
                    }
                })
            })
            .collect::<Result<Vec<_>>>()?;
        let spec_base_al = crate::algo(std::slice::from_ref(&config.levels[0].specdir))?;

        // Assemble the levels from the top down, each level above the base
        // becoming the host of the level beneath it
        let level_top = levels.last().expect("validated nonempty tower");
        let mut parts = build_top(level_top, &spec_base_al, config.top_lang(), options);
        for idx in (0..levels.len() - 1).rev() {
            parts = build_relay(config.levels[idx].lang, parts.into_host(), options);
        }
        let base = parts.into_base(arena, spec_base_al, config.mode, options)?;
        Ok(Self { levels, base })
    }
}
