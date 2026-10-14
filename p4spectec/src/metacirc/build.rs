//! Preparation of target, intermediate, and base specifications
//!
//! Lowered specifications stay paired with their configured entry and input language.
//! Host helpers are assembled from target toward base;
//! patched scripts carry each inner entry to the interpreter consuming them.

use super::{
    ExecutionMode, Result, TowerConfig,
    config::{Lang, LevelConfig, LevelConfigs},
    error::input_invalid,
    externs::{RelayExtern, TerminalExtern},
    patch,
    tower::{Layer, Target, Tower},
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
pub(super) struct Level {
    config: LevelConfig,
    spec_al: al::Spec,
}

impl Level {
    /// Lowers one configured specification without losing its role metadata.
    fn new(config: &LevelConfig) -> Result<Self> {
        let spec_al = crate::algo(std::slice::from_ref(&config.specdir))?;
        Ok(Self { config: config.clone(), spec_al })
    }

    /// Patches this specification's entry around its encoded input.
    fn encode(&self, arena: &mut Arena, value: Value) -> Result<Value> {
        patch::entry(arena, &self.spec_al, &self.config.rel, value)?
            .encode(arena)
            .map_err(|error| error.into_report())
    }
}

/// Prepared specifications with explicit execution roles.
pub(super) enum TowerSpecs {
    Direct(Level),
    Nested { base: Level, interms: Vec<Level>, target: Level },
}

impl TowerSpecs {
    /// Lowers each specification exactly once in its named role.
    pub fn new(config: &TowerConfig) -> Result<Self> {
        match &config.levels {
            LevelConfigs::Direct(target) => Ok(Self::Direct(Level::new(target)?)),
            LevelConfigs::Nested { base, interms, target } => Ok(Self::Nested {
                base: Level::new(base)?,
                interms: interms.iter().map(Level::new).collect::<Result<_>>()?,
                target: Level::new(target)?,
            }),
        }
    }

    /// Selects the relation entered by the native base.
    pub fn relation(&self) -> &str {
        match self {
            Self::Direct(target) => &target.config.rel,
            Self::Nested { base, .. } => &base.config.rel,
        }
    }
}

/// Prepares a native representation from already lowered AL.
fn native(spec_al: al::Spec, mode: ExecutionMode) -> Result<Spec> {
    match mode {
        ExecutionMode::Al => Ok(Spec::Al(spec_al)),
        ExecutionMode::Sl => crate::pass::structure::convert(spec_al, true).map(Spec::Sl),
    }
}

/// Prepares the input interface and its own terminal host environment.
fn build_target(
    arena: &mut Arena,
    target: &Level,
    mode: ExecutionMode,
    includes: &[PathBuf],
    paths_program: &[PathBuf],
    options: runner::Config,
) -> Result<(Target, Value)> {
    let spec_target = native(target.spec_al.clone(), mode)?;
    match target.config.lang {
        Lang::Al => {
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
        Lang::P4 => {
            // P4 parsing consumes one translation unit and its include paths
            if paths_program.len() != 1 {
                return Err(input_invalid("a P4 target requires exactly one program path"));
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
    interm: &Level,
    layer: Layer,
    options: runner::Config,
) -> Result<Layer> {
    let spec = native(interm.spec_al.clone(), ExecutionMode::Sl)?;
    let external = RelayExtern { runner_next: Box::new(layer) };
    Ok(Layer::Interm(Runner::new(arena, spec, options, interface::al(), external)?))
}

/// Prepares the native base with the requested interpreter mode.
fn build_base(
    arena: &mut Arena,
    base: &Level,
    mode: ExecutionMode,
    layer: Layer,
    options: runner::Config,
) -> Result<Tower> {
    let spec = native(base.spec_al.clone(), mode)?;
    let external = RelayExtern { runner_next: Box::new(layer) };
    Ok(Tower::Nested(Runner::new(arena, spec, options, interface::al(), external)?))
}

/// Assembles the host chain and encodes the input for its native base.
///
/// Construction for `[base, interm, target]`:
///
/// ```text
///                  build_target(target)
///                           |
///              +------------+------------+
///              |                         |
///        runner_target             value_program
///              |                         |
///       Layer::Target              target.encode
///              |                         |
///         layer_target              value_target
///              |                         |
///   build_interm(interm, ...)      interm.encode
///              |                         |
///         layer_interm              value_interm
///              |                         |
///    build_base(base, ...)               |
///              |                         |
///            Tower                     Value
///              |                         |
///              +------------+------------+
///                           |
///                    return (Tower, Value)
/// ```
///
/// The two intermediate steps repeat from the last intermediate to the first.
/// Each runner owns the layer already built beneath it; each encoded script
/// embeds the value already built beneath it in a synthetic `main`.
/// With no intermediates, both columns proceed directly to the base step.
///
/// A direct tower only calls `build_target` and returns
/// `(Tower::Direct(runner_target), value_program)`.
pub(super) fn tower(
    arena: &mut Arena,
    specs: &TowerSpecs,
    mode: ExecutionMode,
    includes: &[PathBuf],
    paths_program: &[PathBuf],
    options: runner::Config,
) -> Result<(Tower, Value)> {
    match specs {
        TowerSpecs::Direct(target) => {
            // The target itself is the base, with no synthetic entry layer
            let (target, value) =
                build_target(arena, target, mode, includes, paths_program, options)?;
            Ok((Tower::Direct(target), value))
        }
        TowerSpecs::Nested { base, interms, target } => {
            // Prepare and encode the innermost target first
            let (runner_target, value) =
                build_target(arena, target, ExecutionMode::Sl, includes, paths_program, options)?;
            let mut value = target.encode(arena, value)?;
            let mut layer = Layer::Target(runner_target);
            // Each intermediate interprets the script prepared immediately above it
            for interm in interms.iter().rev() {
                layer = build_interm(arena, interm, layer, options)?;
                value = interm.encode(arena, value)?;
            }
            // The base executes natively and is never encoded as an input script
            Ok((build_base(arena, base, mode, layer, options)?, value))
        }
    }
}
