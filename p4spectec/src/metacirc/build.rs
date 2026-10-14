//! Preparation of target, intermediate, and boot specifications
//!
//! Lowered specifications stay paired with their configured entry and interface.
//! Host helpers are assembled from target toward boot;
//! patched scripts carry each inner entry to the interpreter consuming them.

use super::{
    Config, Mode, Result,
    config::{Interface, Level, Levels},
    error,
    externs::{RelayExtern, TerminalExtern},
    patch,
    tower::{Boot, Layer, Target},
};
use crate::{
    interface::{self, al::encode::Encode},
    lang::{
        al::ast as al,
        data::{arena::Arena, value::flat::Value},
    },
    runner::{self, NullExtern, Runner, Spec},
    sim_plugin::dummy::Dummy,
};
use std::path::PathBuf;

/// A lowered specification paired with its configuration.
pub(super) struct LevelSpec {
    level: Level,
    spec_al: al::Spec,
}

impl LevelSpec {
    /// Lowers one configured specification without losing its role metadata.
    fn new(level: &Level) -> Result<Self> {
        let spec_al = crate::algo(std::slice::from_ref(&level.specdir))?;
        Ok(Self { level: level.clone(), spec_al })
    }

    /// Patches this specification's entry around its encoded input.
    fn encode(&self, arena: &mut Arena, value: Value) -> Result<Value> {
        patch::entry(arena, &self.spec_al, &self.level.rel, value)?
            .encode(arena)
            .map_err(|error| error.into_report())
    }
}

/// Prepared specifications with explicit execution roles.
pub(super) enum TowerSpecs {
    Single(LevelSpec),
    Layered { boot: LevelSpec, interms: Vec<LevelSpec>, target: LevelSpec },
}

impl TowerSpecs {
    /// Lowers each specification exactly once in its named role.
    pub fn new(config: &Config) -> Result<Self> {
        match &config.levels {
            Levels::Single(target) => Ok(Self::Single(LevelSpec::new(target)?)),
            Levels::Layered { boot, interms, target } => Ok(Self::Layered {
                boot: LevelSpec::new(boot)?,
                interms: interms.iter().map(LevelSpec::new).collect::<Result<_>>()?,
                target: LevelSpec::new(target)?,
            }),
        }
    }

    /// Selects the relation entered by the native boot.
    pub fn relation(&self) -> &str {
        match self {
            Self::Single(target) => &target.level.rel,
            Self::Layered { boot, .. } => &boot.level.rel,
        }
    }
}

/// Prepares a native representation from already lowered AL.
fn native(spec_al: al::Spec, mode: Mode) -> Result<Spec> {
    match mode {
        Mode::Al => Ok(Spec::Al(spec_al)),
        Mode::Sl => crate::pass::structure::convert(spec_al, true).map(Spec::Sl),
    }
}

/// Prepares the input interface and its own terminal host environment.
fn build_target(
    arena: &mut Arena,
    target: &LevelSpec,
    mode: Mode,
    includes: &[PathBuf],
    paths_program: &[PathBuf],
    options: runner::Config,
) -> Result<(Target, Value)> {
    let spec_target = native(target.spec_al.clone(), mode)?;
    match target.level.interface {
        Interface::Al => {
            // Resolve script builtins against the input program's own definitions
            let spec_program_al = crate::algo(paths_program)?;
            let value = spec_program_al
                .encode(arena)
                .map_err(|error| error.into_report())?;
            let spec_program = native(spec_program_al, mode)?;
            let runner_target =
                Runner::new(arena, spec_program, options, interface::al(), NullExtern)?;
            let runner = Runner::new(
                arena,
                spec_target,
                options,
                interface::al(),
                TerminalExtern { runner_target: Box::new(runner_target) },
            )?;
            Ok((Target::Al(runner), value))
        }
        Interface::P4 => {
            // P4 parsing consumes one translation unit and its include paths
            if paths_program.len() != 1 {
                return Err(error("a P4 target requires exactly one program path"));
            }
            let interface_target = interface::p4(&spec_target);
            let mut runner = Runner::new(arena, spec_target, options, interface_target, Dummy)?;
            let value = runner
                .parse_program(arena, includes, paths_program)
                .map_err(|error| error.into_report())?;
            Ok((Target::P4(runner), value))
        }
    }
}

/// Prepares an intermediate host helper above an already built layer.
fn build_interm(
    arena: &mut Arena,
    interm: &LevelSpec,
    layer: Layer,
    options: runner::Config,
) -> Result<Layer> {
    let spec = native(interm.spec_al.clone(), Mode::Sl)?;
    let external = RelayExtern { runner_next: Box::new(layer) };
    Ok(Layer::Interm(Runner::new(arena, spec, options, interface::al(), external)?))
}

/// Prepares the native boot with the requested interpreter mode.
fn build_boot(
    arena: &mut Arena,
    boot: &LevelSpec,
    mode: Mode,
    layer: Layer,
    options: runner::Config,
) -> Result<Boot> {
    let spec = native(boot.spec_al.clone(), mode)?;
    let external = RelayExtern { runner_next: Box::new(layer) };
    Ok(Boot::Meta(Runner::new(arena, spec, options, interface::al(), external)?))
}

/// Assembles the host chain and encodes the input for its native boot.
pub(super) fn tower(
    arena: &mut Arena,
    specs: &TowerSpecs,
    mode: Mode,
    includes: &[PathBuf],
    paths_program: &[PathBuf],
    options: runner::Config,
) -> Result<(Boot, Value)> {
    match specs {
        TowerSpecs::Single(target) => {
            // The target itself is the boot, with no synthetic entry layer
            let (target, value) =
                build_target(arena, target, mode, includes, paths_program, options)?;
            Ok((Boot::Target(target), value))
        }
        TowerSpecs::Layered { boot, interms, target } => {
            // Prepare and encode the innermost target first
            let (runner_target, value) =
                build_target(arena, target, Mode::Sl, includes, paths_program, options)?;
            let mut value = target.encode(arena, value)?;
            let mut layer = Layer::Target(runner_target);
            // Each intermediate interprets the script prepared immediately above it
            for interm in interms.iter().rev() {
                layer = build_interm(arena, interm, layer, options)?;
                value = interm.encode(arena, value)?;
            }
            // The boot executes natively and is never encoded as an input script
            Ok((build_boot(arena, boot, mode, layer, options)?, value))
        }
    }
}
