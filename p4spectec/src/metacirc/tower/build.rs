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
//! | level below accepts | `PreparedLevel` | patched into the level below |
//! |---------------------|-----------------|------------------------------|
//! | none (base)         | `Base`          | —                            |
//! | `al`                | `Al`            | AL                           |
//! | `il`                | `Il`            | IL                           |

use super::super::engine::{InterfaceKind, KEngineState, KRuntime};
use super::super::protocol::{KoreProtocol, MetaKProtocol, MetaProtocol, Protocol, ProtocolKind};
use super::super::{ExecutionMode, Result, TowerConfig, config::Lang, error::config_invalid};
use super::{BaseRunner, KBase, PreparedLevel, Top, Tower, Upper};
use crate::{
    interface::{
        self,
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
        ExecutionMode::K => unreachable!("a K base has no AL or SL runner"),
    }
}

/// Prepares the P4 interface, with print hints from the form the top level is prepared in.
///
/// `spec_base_al` is read only when the top level is the base, a P4 specification.
fn interface_p4(level_top: &PreparedLevel, spec_base_al: Option<&al::Spec>) -> P4Interface {
    let unparser = match level_top {
        PreparedLevel::Base { .. } => {
            P4Unparser::from_al_spec(spec_base_al.expect("a P4 base is read as AL"))
        }
        PreparedLevel::Al { spec_al, .. } => P4Unparser::from_al_spec(spec_al),
        PreparedLevel::Il { spec_il, .. } => P4Unparser::from_il_spec(spec_il),
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
    spec_base_al: Option<&al::Spec>,
    options: runner::Config,
    external_p4: Ext,
) -> (Upper<Ext>, Vec<ProtocolKind>) {
    let (top, configs_host) = match config.top_lang() {
        // Above a meta or KORE top level is its program; every level above the base is meta
        Lang::Al | Lang::Il | Lang::Kore => {
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
    // The level below each boundary interprets the level above it:
    // a K base runs spec-meta-k, a level accepting KORE is spec-k,
    // and every other level is spec-meta
    let count = hosts.len() + 1;
    let protocols = (0..count)
        .map(|idx_below| match (config.mode, config.levels[idx_below].lang) {
            (ExecutionMode::K, _) if idx_below == 0 => ProtocolKind::MetaK(MetaKProtocol),
            (_, Lang::Kore) => ProtocolKind::Kore(KoreProtocol),
            _ => ProtocolKind::Meta(MetaProtocol),
        })
        .collect::<Vec<_>>();
    // Each level through the top has C and the codec cache of the boundary below it
    let caches = protocols
        .iter()
        .map(|protocol| protocol.new_cache(options.cache))
        .collect();
    let upper = Upper { caches, ctxs: vec![None; count], hosts, top, depth_next: None };
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
                    Some(_) => {
                        return Err(config_invalid(format!(
                            "invalid language at level {}: expected al or il",
                            idx - 1
                        )));
                    }
                })
            })
            .collect::<Result<Vec<_>>>()?;

        // Only the base runs natively
        let level_top = levels.last().expect("validated nonempty tower");
        let config_base = &config.levels[0];
        // A K base runs from its compiled library and hosts the levels above it
        if let ExecutionMode::K = config.mode {
            let kompiled = config_base.kompiled.as_ref().expect("validated k base");
            let runtime = KRuntime::load(kompiled)?;
            let state = KEngineState::new(&runtime)?;
            let (upper, protocols) = build_upper(config, level_top, None, options, external_p4);
            let base = BaseRunner::K(KBase { runtime, state, upper, effects: 0 });
            return Ok(Self { levels, protocols, base, options });
        }
        let spec_base_al = crate::algo(std::slice::from_ref(&config_base.specdir))?;
        let (base, protocols) = match config_base.lang {
            // A P4 base is the whole tower
            Lang::P4 => {
                let interface = interface_p4(level_top, Some(&spec_base_al));
                let spec_base = into_runner_spec(config.mode, spec_base_al)?;
                let runner = Runner::new(arena, spec_base, options, interface, external_p4)?;
                (BaseRunner::P4(runner), Vec::new())
            }
            // A meta base or spec-k hosts the levels above it as its extern
            lang_base => {
                let (upper, protocols) =
                    build_upper(config, level_top, Some(&spec_base_al), options, external_p4);
                let interface = match lang_base {
                    Lang::Kore => InterfaceKind::Kore(interface::kore()),
                    _ => InterfaceKind::Meta(interface::meta(lang_base)),
                };
                let spec_base = into_runner_spec(config.mode, spec_base_al)?;
                let runner = Runner::new(arena, spec_base, options, interface, upper)?;
                (BaseRunner::Meta(runner), protocols)
            }
        };
        Ok(Self { levels, protocols, base, options })
    }
}
