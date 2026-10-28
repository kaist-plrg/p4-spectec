//! Construction of a tower's levels and the storage above its base
//!
//! ```text
//!   +-----------------+-----------------+
//!   | Builtin P4      | Extern P4       |   top     Top::P4, Ext
//!   +-----------------+-----------------+
//!                     ^ caches[1]
//!   +-----------------+-----------------+
//!   | Builtin SpecTec | Extern SpecTec  |   interm  hosts[0], Boundary
//!   +-----------------+-----------------+
//!                     ^ caches[0]
//!   +-----------------+-----------------+
//!   | Builtin SpecTec | Extern SpecTec  |   base    BaseRunner::Meta, Upper
//!   +-----------------+-----------------+
//!   |      Interp(base AL): entry       |
//!   +-----------------------------------+
//! ```
//!
//! Only the base runs natively, as a runner with an interpreter.
//! A level above it is interpreted by the level below,
//! so `Upper` stores it per level, answering relayed builtin and host calls.
//! An AL or IL top ends the tower at its program, `Top::Program`,
//! which answers only the program's builtins
//! across one more boundary above the highest meta level.
//!
//! | level below accepts | `PreparedLevel` | encoded for the level below  |
//! |---------------------|-----------------|------------------------------|
//! | none (base)         | `Base`          | —                            |
//! | `al`                | `Al`            | AL                           |
//! | `il`                | `Il`            | IL                           |
//! | `el`                | `El`            | EL                           |

use super::super::protocol::{MetaProtocol, ProtocolKind};
use super::super::{ExecutionMode, Result, TowerConfig, config::Lang, error::config_invalid};
use super::{BaseRunner, PreparedLevel, Top, Tower, Upper};
use crate::{
    interface::{
        self,
        meta::shared::Cache,
        p4::{P4Interface, unparse::P4Unparser},
    },
    lang::{al::ast as al, data::arena::Arena},
    pass::structure,
    runner::{self, Extern, Host, Runner, Spec},
};

// == Helpers

/// Converts the AL of a specification into the representation selected for the runner.
fn into_runner_spec(mode: ExecutionMode, spec_al: al::Spec) -> Result<Spec> {
    match mode {
        ExecutionMode::Al => Ok(Spec::Al(spec_al)),
        ExecutionMode::Sl => structure::convert(spec_al, true).map(Spec::Sl),
    }
}

/// Prepares the P4 interface, with print hints from the form the top level is prepared in.
fn interface_p4(level_top: &PreparedLevel, spec_base_al: &al::Spec) -> P4Interface {
    let unparser = match level_top {
        PreparedLevel::Base { .. } => P4Unparser::from_al_spec(spec_base_al),
        PreparedLevel::Al { spec_al, .. } => P4Unparser::from_al_spec(spec_al),
        PreparedLevel::Il { spec_il, .. } => P4Unparser::from_il_spec(spec_il),
        // The EL top's types are elaborated natively only for printing
        PreparedLevel::El { spec_typ_il, .. } => P4Unparser::from_il_spec(spec_typ_il),
    };
    interface::p4_with_unparser(unparser)
}

// == Level construction

/// Prepares the levels above a meta base, up to the P4 top or the program,
/// and the protocol of each boundary below them.
///
/// Only a P4 top level uses `external_p4`.
fn build_upper<Ext: Extern>(
    config: &TowerConfig,
    level_top: &PreparedLevel,
    spec_base_al: &al::Spec,
    options: runner::Config,
    external_p4: Ext,
) -> (Upper<Ext>, Vec<ProtocolKind>) {
    let (top, configs_host) = match config.top_lang() {
        // Above a meta top level is its program; every level above the base is meta
        Lang::Al | Lang::Il | Lang::El => {
            let top = Top::Program(interface::meta(Lang::Al));
            (top, &config.levels[1..])
        }
        // A P4 top level is the highest level, above the meta levels below it
        Lang::P4 => {
            let host = Host::new(interface_p4(level_top, spec_base_al), external_p4);
            (Top::P4(host), &config.levels[1..config.levels.len() - 1])
        }
    };
    // Each meta level between the base and the top answers its builtins
    let hosts = configs_host
        .iter()
        .map(|config_level| interface::meta(config_level.lang))
        .collect::<Vec<_>>();
    // Each level through the top has C and the codec cache of the boundary below it
    let count = hosts.len() + 1;
    let caches = (0..count).map(|_| Cache::new(options.cache)).collect();
    let upper = Upper { caches, ctxs: vec![None; count], hosts, top, depth_next: None };
    // spec-meta interprets the level above it at every boundary
    let protocols = (0..count)
        .map(|_| ProtocolKind::Meta(MetaProtocol))
        .collect();
    (upper, protocols)
}

// == Tower assembly

impl<Ext: Extern> Tower<Ext> {
    /// Prepares a tower without parsing or encoding its input.
    ///
    /// Only the base is converted for a native runner;
    /// every other level is prepared in the form the level below accepts.
    /// All calls must use the construction arena.
    /// A P4 top level uses `external_p4` as its extern.
    pub fn build(
        arena: &mut Arena,
        config: &TowerConfig,
        options: runner::Config,
        external_p4: Ext,
    ) -> Result<Self> {
        // Prepare each specification once, from base to top, into its PreparedLevel;
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
                    None => PreparedLevel::Base { config: config_level.clone() },
                    Some(Lang::Al) => PreparedLevel::Al {
                        config: config_level.clone(),
                        spec_al: crate::algo(paths)?,
                    },
                    Some(Lang::Il) => PreparedLevel::Il {
                        config: config_level.clone(),
                        spec_il: crate::elab(paths)?,
                    },
                    Some(Lang::El) => PreparedLevel::El {
                        config: config_level.clone(),
                        spec_el: crate::parse(paths)?,
                        spec_typ_il: crate::elab_typs(paths)?,
                    },
                    Some(_) => {
                        return Err(config_invalid(format!(
                            "invalid language at level {}: expected al, il, or el",
                            idx - 1
                        )));
                    }
                })
            })
            .collect::<Result<Vec<_>>>()?;
        let spec_base_al = crate::algo(std::slice::from_ref(&config.levels[0].specdir))?;

        // Only the base runs natively
        let level_top = levels.last().expect("validated nonempty tower");
        let (base, protocols) = match config.levels[0].lang {
            // A P4 base is the whole tower
            Lang::P4 => {
                let interface = interface_p4(level_top, &spec_base_al);
                let spec_base = into_runner_spec(config.mode, spec_base_al)?;
                let runner = Runner::new(arena, spec_base, options, interface, external_p4)?;
                (BaseRunner::P4(runner), Vec::new())
            }
            // A meta base hosts the levels above it as its extern
            lang_base => {
                let (upper, protocols) =
                    build_upper(config, level_top, &spec_base_al, options, external_p4);
                let interface = interface::meta(lang_base);
                let spec_base = into_runner_spec(config.mode, spec_base_al)?;
                let runner = Runner::new(arena, spec_base, options, interface, upper)?;
                (BaseRunner::Meta(runner), protocols)
            }
        };
        Ok(Self { levels, protocols, base, options })
    }
}
