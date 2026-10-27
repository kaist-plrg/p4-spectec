//! The library `kompile` made of a K base specification
//!
//! A `k` tower names it in its base's `kompiled` field.
//! `KRuntime::load` opens it once per tower (`libloading`)
//! and resolves the K runtime's entry points;
//! every depth's `KEngineState` runs on the same library.
//! Calls into the library go through `ffi`, ST-in-K's only module allowed `unsafe`.

use crate::metacirc::{Result, error};
use std::path::{Path, PathBuf};

/// A K library loaded for a tower, shared by every depth.
pub(in crate::metacirc) struct KRuntime {
    /// The library file, as the configuration resolved it.
    #[expect(dead_code, reason = "a slot for the ST-in-K implementation")]
    path: PathBuf,
}

impl KRuntime {
    /// Loads the library at `path`, compiled from the base by `make kompile`.
    pub(in crate::metacirc) fn load(path: &Path) -> Result<Self> {
        if !path.is_file() {
            return Err(error::config_invalid(format!(
                "kompiled library {} not found; build it with make kompile first",
                path.display()
            )));
        }
        Err(error::unimplemented("KRuntime::load"))
    }
}
