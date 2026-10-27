//! Interpreter construction shared by corpus and diagnostic suites
//!
//! `Language::spec` converts specification sources for one interpreter,
//! `build` builds a P4 runner from the result,
//! and `Configuration` names the language and runner options a corpus runs under.

use std::path::PathBuf;

use serde::Deserialize;

use p4spectec::lang::data::arena::Arena;

use p4spectec::interface::p4::P4Interface;

use p4spectec::runner::{self, Config, Extern, Runner, Spec};

use super::{Error, Result};

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
