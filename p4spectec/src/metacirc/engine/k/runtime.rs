//! The library `kompile` made of a K base specification
//!
//! A `k` tower names it in its base's `kompiled` field.
//! `KRuntime::load` opens it once per tower (`libloading`)
//! and resolves the K runtime's entry points;
//! every depth's `KEngineState` runs on the same library.
//! Calls into the library go through `ffi`, ST-in-K's only module allowed `unsafe`.
//!
//! A specification loaded into spec-meta-k lives in one K thread's configuration,
//! but C, its handle, is shared by every depth.
//! The runtime keeps each load request under a key, which C names,
//! so a depth's thread can load it again on first use.

use super::ffi::Library;
use crate::metacirc::{Result, error};
use std::{
    cell::RefCell,
    path::{Path, PathBuf},
    rc::Rc,
};

/// A K library loaded for a tower, shared by every depth.
pub(in crate::metacirc) struct KRuntime {
    /// The library file, as the configuration resolved it.
    path: PathBuf,
    pub(super) library: &'static Library,
    /// The load requests made so far, at their keys; equal requests share one.
    loads: RefCell<Vec<Rc<str>>>,
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
        let library = Library::load(path).map_err(|cause| {
            error::config_invalid(format!("cannot load the K library {}: {cause}", path.display()))
        })?;
        Ok(Self { path: path.to_owned(), library, loads: RefCell::default() })
    }

    /// Returns the library file.
    pub(super) fn path(&self) -> &Path {
        &self.path
    }

    /// Returns the key of a load request, keeping the request if it is new.
    pub(super) fn register(&self, request: &str) -> usize {
        let mut loads = self.loads.borrow_mut();
        if let Some(key) = loads.iter().position(|load| **load == *request) {
            return key;
        }
        loads.push(request.into());
        loads.len() - 1
    }

    /// Returns the load request at `key`, if one was registered.
    pub(super) fn request(&self, key: usize) -> Option<Rc<str>> {
        self.loads.borrow().get(key).cloned()
    }
}
