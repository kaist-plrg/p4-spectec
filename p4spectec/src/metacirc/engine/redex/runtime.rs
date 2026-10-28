//! The `racket` process running a PLT Redex base specification
//!
//! A `redex` tower names the base's entry module in its `module` field.
//! `RedexRuntime::load` starts `racket` on it once per tower, with piped
//! stdin and stdout, and every depth's `RedexEngineState` shares the process:
//! a nested call from a deeper depth goes to the same pipe while the outer call
//! waits for a host response, and Racket answers it in place.
//! The runtime keeps each load request under a key, which C names,
//! and the key the process loaded last.

use crate::metacirc::{Result, error};
use std::path::{Path, PathBuf};

/// A `racket` process running the base's entry module, shared by every depth.
pub(in crate::metacirc) struct RedexRuntime {
    /// The entry module, as the configuration resolved it.
    #[expect(dead_code, reason = "a slot for the ST-in-Redex implementation")]
    path: PathBuf,
}

impl RedexRuntime {
    /// Starts `racket` on the entry module at `path`, compiled by `make redex`,
    /// and waits until it serves.
    pub(in crate::metacirc) fn load(path: &Path) -> Result<Self> {
        if !path.is_file() {
            return Err(error::config_invalid(format!(
                "Racket module {} not found; build it with make redex first",
                path.display()
            )));
        }
        Err(error::unimplemented("RedexRuntime::load"))
    }
}
