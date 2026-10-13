//! Standalone acceptance checks for the AL meta representation
//!
//! `main` checks compiler snapshots, source AL codec boundaries,
//! and DSL semantic regressions through guarded native interpreters.
//! Fixture paths resolve from this crate independently of the working directory.

mod boundary;
mod script;
mod semantics;
mod snapshot;

use std::{
    error::Error,
    path::{Path, PathBuf},
};

use p4spectec::{
    interface::{self, al::AlInterface},
    lang::data::{
        arena::Arena,
        value::flat::{Value, get},
    },
    runner::{Config, NullExtern, Runner, Spec},
};

type Result<T> = std::result::Result<T, Box<dyn Error>>;
type MetaRunner = Runner<AlInterface, NullExtern>;

fn main() -> Result<()> {
    snapshot::run()?;
    script::run()?;
    boundary::run()?;
    semantics::run()?;
    eprintln!("metacirc: all acceptance checks passed");
    Ok(())
}

fn fixture(name: &str) -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("fixtures")
        .join(name)
}

fn source(name: &str) -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .join(name)
}

/// Prepares the meta specification and its guards in the supplied arena.
fn meta_runner(arena: &mut Arena) -> Result<MetaRunner> {
    let paths = [source("spec-meta"), fixture("guards.watsup")];
    let spec_al = p4spectec::algo(&paths)?;
    Ok(Runner::new(
        arena,
        Spec::Al(spec_al),
        Config::new(false, true, true),
        interface::al(),
        NullExtern,
    )?)
}

/// Requires a guarded call to return true rather than merely evaluate.
fn check_guard(
    runner: &mut MetaRunner,
    arena: &mut Arena,
    name: &str,
    values: &[Value],
) -> Result<()> {
    let value = runner.eval_func(arena, name, &[], values).result?;
    assert!(get::bool(arena, &value)?, "{name}: expected true");
    Ok(())
}
