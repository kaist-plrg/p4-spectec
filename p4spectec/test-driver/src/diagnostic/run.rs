//! Argument-file run diagnostic acceptance
//!
//! Each fixture supplies the complete product invocation and expected stderr.

use std::path::Path;

use crate::Result;

/// Loads and compares this module's CLI diagnostic fixtures.
pub fn run(path: &Path, path_cli: Option<&Path>) -> Result<()> {
    let groups = super::load(path, true)?;
    super::run_registered("run", &groups, path_cli, None, Default::default())
}
