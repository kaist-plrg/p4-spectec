//! Interpreter construction shared by corpus and diagnostic suites
//!
//! `Language::spec` converts specification sources for one interpreter,
//! `build` builds a P4 runner from the result.

use std::path::PathBuf;

use p4spectec::lang::data::arena::Arena;

use p4spectec::interface::p4::P4Interface;

use p4spectec::runner::{self, Config, Extern, Runner, Spec};

use super::{Error, Result};

/// Selects the production interpreter used by a corpus or diagnostic case.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Language {
    Al,
    Sl,
    Pl,
}

impl Language {
    /// Returns the language's spelling in worker arguments.
    pub fn name(self) -> &'static str {
        match self {
            Self::Al => "al",
            Self::Sl => "sl",
            Self::Pl => "pl",
        }
    }

    /// Reads a language spelled as in worker arguments.
    pub fn from_name(name: &str) -> Result<Self> {
        [Self::Al, Self::Sl, Self::Pl]
            .into_iter()
            .find(|language| language.name() == name)
            .ok_or_else(|| Error::from(format!("unknown language {name}")))
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
