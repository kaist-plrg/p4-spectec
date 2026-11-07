//! JSON descriptions of AL interpreter towers
//!
//! Levels run from the native base toward a top level consuming AL, IL, P4, or KORE.
//! Each level names the language it accepts:
//! the level above it is passed to it as AL or IL, in the meta syntax of
//! spec-meta (`al`) or spec-meta-ext (`al-ext`, `il-ext`).
//! The base runs natively, as `BaseConfig` says: a P4-SpecTec specification on
//! the AL or SL interpreter (`St`), a K library compiled from the base (`K`),
//! or a Racket module of the base run by `racket` (`Redex`).
//! An AL or IL script above the top level is entered at `entry`,
//! the function `main` unless the JSON names another function or relation.
//! Specification, library, and module paths are resolved relative to the JSON file.
//!
//! The JSON names the base by `mode` and gives the K library (`kompiled`)
//! or the Racket module (`module`) on the base's level:
//!
//! ```json
//! { "mode": "k",
//!   "levels": [ { "specdir": "../spec-meta-k", "kompiled": "...", "rel": "Eval_AL_func", "lang": "al" } ] }
//! ```

use super::{Result, error::config_invalid};
pub use crate::interface::Lang;
use serde::Deserialize;
use std::{
    fs,
    path::{Path, PathBuf},
};

/// Validated tower roles and the native base.
#[derive(Clone, Debug)]
pub struct TowerConfig {
    /// What runs the base specification natively.
    pub(super) base: BaseConfig,
    /// Nonempty, ordered from base toward top.
    pub(super) levels: Vec<LevelConfig>,
    /// The function or relation entering an AL or IL script above the top level.
    pub(super) entry: String,
}

/// What runs the base specification natively: the meta-language the base
/// is written in, and its native implementation.
#[derive(Clone, Debug)]
pub enum BaseConfig {
    /// A P4-SpecTec specification, run by the AL or SL interpreter.
    St { repr: Repr },
    /// A K specification, run from the library `kompile` made of it.
    K { kompiled: PathBuf },
    /// A PLT Redex specification, run by `racket` on its entry module.
    Redex { module: PathBuf },
}

/// The representation the St interpreter executes.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Repr {
    /// The algorithmic representation.
    Al,
    /// The structured representation.
    Sl,
}

/// One specification and the relation used to enter it.
#[derive(Clone, Debug)]
pub struct LevelConfig {
    /// A specification file or directory.
    pub specdir: PathBuf,
    /// The relation entering this level, called with the inputs from above.
    pub rel: String,
    /// The language accepted by this level.
    pub lang: Lang,
}

// == JSON

/// The base named in the JSON.
#[derive(Clone, Copy, Debug, Deserialize)]
#[serde(rename_all = "lowercase")]
enum ModeJson {
    Al,
    Sl,
    K,
    Redex,
}

/// A level at the JSON boundary, with the base's library or module.
#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct LevelJson {
    specdir: PathBuf,
    rel: String,
    lang: Lang,
    #[serde(default)]
    kompiled: Option<PathBuf>,
    #[serde(default)]
    module: Option<PathBuf>,
}

/// Ordered levels at the JSON boundary.
#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct TowerJson {
    mode: ModeJson,
    levels: Vec<LevelJson>,
    #[serde(default = "entry_default")]
    entry: String,
}

/// A script is entered at its function `main` unless configured otherwise.
fn entry_default() -> String {
    "main".to_owned()
}

impl TowerConfig {
    /// Reads tower roles from a JSON file.
    ///
    /// Inputs are supplied only after the tower is built,
    /// so a corpus can build a tower once and run many inputs through it.
    pub fn from_file(path: &Path) -> Result<Self> {
        let text = fs::read_to_string(path)
            .map_err(|cause| config_invalid(format!("{}: {cause}", path.display())))?;
        let config: TowerJson = serde_json::from_str(&text)
            .map_err(|cause| config_invalid(format!("{}: {cause}", path.display())))?;
        // Resolve source, library, and module paths relative to the JSON file
        let path_parent = path.parent().unwrap_or(Path::new("."));
        let resolve = |path: PathBuf| {
            if !path.as_os_str().is_empty() && path.is_relative() {
                path_parent.join(path)
            } else {
                path
            }
        };
        // Only the base names a library or module, as its mode requires
        let mut kompiled = None;
        let mut module = None;
        let mut levels = Vec::with_capacity(config.levels.len());
        for (idx, level) in config.levels.into_iter().enumerate() {
            if level.kompiled.is_some() && (idx != 0 || !matches!(config.mode, ModeJson::K)) {
                return Err(config_invalid(format!(
                    "kompiled at level {idx}: only the base of a k tower takes a library"
                )));
            }
            if level.module.is_some() && (idx != 0 || !matches!(config.mode, ModeJson::Redex)) {
                return Err(config_invalid(format!(
                    "module at level {idx}: only the base of a redex tower takes a Racket module"
                )));
            }
            kompiled = kompiled.or(level.kompiled.map(resolve));
            module = module.or(level.module.map(resolve));
            levels.push(LevelConfig {
                specdir: resolve(level.specdir),
                rel: level.rel,
                lang: level.lang,
            });
        }
        let base = match config.mode {
            ModeJson::Al => BaseConfig::St { repr: Repr::Al },
            ModeJson::Sl => BaseConfig::St { repr: Repr::Sl },
            ModeJson::K => BaseConfig::K {
                kompiled: kompiled.ok_or_else(|| {
                    config_invalid("a k tower's base requires kompiled, its compiled library")
                })?,
            },
            ModeJson::Redex => BaseConfig::Redex {
                module: module.ok_or_else(|| {
                    config_invalid("a redex tower's base requires module, its Racket entry module")
                })?,
            },
        };
        Self::new(base, levels, config.entry)
    }

    /// Validates ordered levels.
    ///
    /// Specification paths are relative to the current directory.
    /// A single level is both the top level and the base.
    /// A K or Redex base accepts AL;
    /// KORE is accepted only by a single level, since no level stacks over K yet.
    pub fn new(base: BaseConfig, levels: Vec<LevelConfig>, entry: String) -> Result<Self> {
        if levels.is_empty() {
            return Err(config_invalid("a tower requires at least one level"));
        }
        // A level below another receives the level above as a script
        let idx_top = levels.len() - 1;
        for (idx, level) in levels.iter().enumerate() {
            if idx != idx_top && !level.lang.is_script() {
                return Err(config_invalid(format!(
                    "invalid language at level {idx}: expected al, al-ext, or il-ext"
                )));
            }
            if level.rel.is_empty() || level.specdir.as_os_str().is_empty() {
                return Err(config_invalid(format!(
                    "level {idx} requires a specification path and entry relation"
                )));
            }
        }
        // A K or Redex base interprets AL
        let lang_base = levels[0].lang;
        match base {
            BaseConfig::K { .. } if lang_base != Lang::Al => {
                return Err(config_invalid("a k tower's base accepts al"));
            }
            BaseConfig::Redex { .. } if lang_base != Lang::Al => {
                return Err(config_invalid("a redex tower's base accepts al"));
            }
            _ => {}
        }
        // Stacking a level over a K definition is not implemented
        if levels[idx_top].lang == Lang::Kore && idx_top != 0 {
            return Err(config_invalid(
                "kore is accepted only by a single-level tower; stacking over K is not implemented",
            ));
        }
        Ok(Self { base, levels, entry })
    }

    pub(super) fn top(&self) -> &LevelConfig {
        self.levels.last().expect("validated nonempty levels")
    }

    /// Returns the language consumed by the top level.
    pub fn top_lang(&self) -> Lang {
        self.top().lang
    }
}
