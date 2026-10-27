//! Argument-file command diagnostic acceptance
//!
//! Each fixture supplies the complete product invocation and expected stderr.

use std::path::Path;

use crate::Result;

/// Loads and compares this module's CLI diagnostic fixtures.
pub fn run(path_cli: Option<&Path>) -> Result<()> {
    let cases = super::load("command")?;
    super::run_registered("command", &cases, path_cli, None, Default::default())
}
