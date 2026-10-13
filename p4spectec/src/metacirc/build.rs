//! Native runner preparation for metacircular execution
//!
//! The target is always prepared from its own specification.
//! Intermediate runners supply host calls for the script interpreted above them.
//! Only the outer runner uses the configured native mode;
//! target and intermediate native helpers use SL.

use super::{
    Config, Mode, Result,
    externs::{BuiltinExtern, TowerExtern},
    tower::Tower,
};
use crate::{
    interface::{self, al::AlInterface},
    lang::{al::ast as al, data::arena::Arena},
    runner::{self, NullExtern, Runner, Spec},
    sim_plugin::dummy::Dummy,
};
use std::path::PathBuf;

/// Prepares a native representation from already lowered AL.
fn native(spec_al: al::Spec, mode: Mode) -> Result<Spec> {
    match mode {
        Mode::Al => Ok(Spec::Al(spec_al)),
        Mode::Sl => crate::pass::structure::convert(spec_al, true).map(Spec::Sl),
    }
}

/// Builds an outer interpreter with a separately initialized builtin target.
pub(super) fn standalone(
    arena: &mut Arena,
    mode: Mode,
    paths: &[PathBuf],
    spec_target_al: al::Spec,
    options: runner::Config,
) -> Result<Runner<AlInterface, BuiltinExtern>> {
    // The builtin target must not inherit the outer interpreter's definitions
    let spec_target = native(spec_target_al, mode)?;
    let runner_target = Runner::new(arena, spec_target, options, interface::al(), NullExtern)?;
    // Prepare the meta specification in the same arena as its encoded input
    let spec_meta = native(crate::algo(paths)?, mode)?;
    Runner::new(arena, spec_meta, options, interface::al(), BuiltinExtern { runner_target })
}

/// Builds the host-call chain from the P4 target toward the outer interpreter.
pub(super) fn tower(
    arena: &mut Arena,
    config: &Config,
    specs_al: &[al::Spec],
    options: runner::Config,
) -> Result<Tower> {
    // Config validation guarantees a target and at least one meta level
    let idx_target = specs_al.len() - 1;
    let spec_target = native(specs_al[idx_target].clone(), Mode::Sl)?;
    let interface_target = interface::p4(&spec_target);
    let mut tower =
        Tower::Target(Runner::new(arena, spec_target, options, interface_target, Dummy)?);
    // Every meta runner owns the next layer through its extern implementation
    for (idx, spec_al) in specs_al[..idx_target].iter().enumerate().rev() {
        let mode = if idx == 0 { config.mode } else { Mode::Sl };
        let spec_meta = native(spec_al.clone(), mode)?;
        let external = TowerExtern { runner_next: Box::new(tower) };
        tower = Tower::Meta(Runner::new(arena, spec_meta, options, interface::al(), external)?);
    }
    Ok(tower)
}
