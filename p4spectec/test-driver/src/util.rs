//! Helpers shared by the snapshot, corpus, and diagnostic runners
//!
//! Corpus and diagnostic runners read `suites/<name>.toml` with `load`.
//! Corpus paths are relative to the repository, expectations to the driver,
//! and diagnostic inputs and expectations to `testdata/spec`.

use std::{
    fs,
    path::{Path, PathBuf},
};

use indicatif::{ProgressBar, ProgressStyle};
use serde::{Deserialize, de::DeserializeOwned};

use p4spectec::lang::data::arena::Arena;

use p4spectec::interface::p4::P4Interface;

use p4spectec::runner::{self, Config, Extern, Runner, Spec};

use crate::{Error, Result};

/// Selects the production interpreter used by a configuration or diagnostic case.
#[derive(Clone, Copy, Debug, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "lowercase")]
pub enum Language {
    Al,
    Sl,
    Pl,
}

impl Language {
    /// Returns the language's registration spelling.
    pub fn name(self) -> &'static str {
        match self {
            Self::Al => "al",
            Self::Sl => "sl",
            Self::Pl => "pl",
        }
    }

    /// Converts specification sources into this language's runner specification.
    pub fn spec(self, paths: &[PathBuf]) -> Result<Spec> {
        let spec_lang = match self {
            Self::Al => p4spectec::SpecLang::Al,
            Self::Sl => p4spectec::SpecLang::Sl,
            Self::Pl => p4spectec::SpecLang::Pl,
        };
        Ok(p4spectec::runner_spec_with_warnings(spec_lang, paths).0?)
    }
}

/// Names one interpreter setup under which a corpus runs every suite.
#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Configuration {
    pub name: String,
    pub language: Language,
    pub cache: bool,
    pub det: bool,
}

impl Configuration {
    /// Finds the configuration named on the command line.
    pub fn find<'a>(configs: &'a [Self], name: &str) -> Result<&'a Self> {
        configs
            .iter()
            .find(|config| config.name == name)
            .ok_or_else(|| Error::from(format!("no configuration named {name}")))
    }

    /// Returns the runner options of this configuration.
    pub fn runner(&self) -> Config {
        Config::new(self.cache, self.det, false)
    }

    /// Describes the configuration in progress and summary lines.
    pub fn describe(&self) -> String {
        format!("{} cache={} det={}", self.name, self.cache, self.det)
    }
}

/// Builds a P4 runner from an AL, SL, or PL specification.
pub fn build<Ext: Extern>(
    arena: &mut Arena,
    spec: Spec,
    config: Config,
    external: Ext,
) -> Result<Runner<P4Interface, Ext>> {
    let runner = match spec {
        Spec::Al(spec_al) => runner::build_al(arena, spec_al, config, external),
        Spec::Sl(spec_sl) => runner::build_sl(arena, spec_sl, config, external),
        Spec::Pl(spec_pl) => runner::build_pl(arena, spec_pl, config, external),
    }?;
    Ok(runner)
}

/// Reads the registration `suites/<name>.toml` with its path attached to errors.
pub fn load<T: DeserializeOwned>(name: &str) -> Result<T> {
    let path = Path::new(env!("CARGO_MANIFEST_DIR")).join(format!("suites/{name}.toml"));
    let text = fs::read_to_string(&path)
        .map_err(|error| Error::from(format!("{}: {error}", path.display())))?;
    toml::from_str(&text).map_err(|error| Error::from(format!("{}: {error}", path.display())))
}

/// Returns the specification sources shared by every runner.
pub fn spec() -> [PathBuf; 1] {
    [PathBuf::from("spec")]
}

/// Returns the P4 include directories shared by every corpus runner.
pub fn includes() -> [PathBuf; 1] {
    [PathBuf::from("p4c/p4include")]
}

/// Resolves the expectation `expected/<module>/<name>.expected` of one suite.
pub fn expected_path(module: &str, name: &str) -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join(format!("expected/{module}/{name}.expected"))
}

/// Creates a progress bar over `len` tests.
pub fn progress_bar(len: usize) -> Result<ProgressBar> {
    let style = ProgressStyle::with_template("[{bar:24}] {pos}/{len} {elapsed_precise} {msg}")?;
    Ok(ProgressBar::new(len as u64).with_style(style))
}
