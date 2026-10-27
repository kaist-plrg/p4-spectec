//! Configurations shared by the metacirc run and sim suites
//!
//! A configuration names the `Base` language of the towers it runs,
//! and registrations name one tower or value per base in `ByBase`;
//! runners take the entries of the configuration's base.

use p4spectec::runner::Config;

use serde::Deserialize;

use crate::harness::{Error, Result};

/// Selects the language the base level of a metacircular tower takes.
#[derive(Clone, Copy, Debug, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "lowercase")]
pub enum Base {
    /// The native `algo` pass converts the base specification and AL inputs.
    Al,
    /// The base level converts its IL input with the meta specification's `Algo_IL`.
    Il,
}

impl Base {
    /// Returns the base's registration spelling.
    pub fn name(self) -> &'static str {
        match self {
            Self::Al => "al",
            Self::Il => "il",
        }
    }
}

/// Names a value for each base; a missing value skips that base.
#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct ByBase<T> {
    pub al: Option<T>,
    pub il: Option<T>,
}

impl<T> ByBase<T> {
    /// Returns the value of `base`, if registered.
    pub fn get(&self, base: Base) -> Option<&T> {
        match base {
            Base::Al => self.al.as_ref(),
            Base::Il => self.il.as_ref(),
        }
    }
}

/// Names one tower setup under which a metacirc corpus runs.
#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Configuration {
    pub name: String,
    pub base: Base,
    pub cache: bool,
    pub det: bool,
}

impl Configuration {
    /// Finds the configuration named on the command line.
    pub fn find<'a>(configs: &'a [Self], name: &str) -> Result<&'a Self> {
        configs
            .iter()
            .find(|config| config.name == name)
            .ok_or_else(|| Error::from(format!("no metacirc configuration named {name}")))
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
