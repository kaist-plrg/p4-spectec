//! K definitions run through a K-in-ST tower and compared with `krun`,
//! registered in `suites/metacirc/k-in-st.toml`
//!
//! `suite` reads the registration into definitions and their programs; `build`
//! kompiles each definition, builds its K library, and makes each initial term
//! with krun, into `$CARGO_TARGET_DIR/k-in-st/`. `run` runs each program with
//! `metacirc::krun` and compares with `<program>.out` (`compare`): the same first
//! line (steps, how the run ended) and configurations equal in normal form.
//! A run that ends elsewhere passes by search if its final configuration is one
//! of `<program>.search`, the finals K allows, as where rules of the same priority both apply.
//! `UPDATE_EXPECT=1` writes `<program>.out` from krun's own run, and `<program>.search`
//! where the run still differs (`krun`).
//!
//! Three pieces only work around K's tools, apart from the semantics: `krun`
//! finds the configuration before a failing step, which the interpreter does not
//! print, by running again with fewer steps; `fastthrow.c` makes the interpreter
//! abort at once at an uncaught exception; `build` renames `@` in the symbols of
//! a K library so that it links.

mod build;
mod compare;
mod krun;
mod suite;

use std::{
    fs,
    path::{Path, PathBuf},
    time::Instant,
};

use p4spectec::{
    metacirc::{self, TowerConfig},
    runner,
};

pub use self::suite::Registration;
use self::{
    build::Case,
    compare::{compare, reached},
    suite::{KInStSuite, Program},
};
use super::config::Configuration;
use crate::harness::{self, Error, Result};

/// The directory of what is built for the cases.
fn dir_build() -> PathBuf {
    std::env::var_os("CARGO_TARGET_DIR")
        .map_or_else(|| PathBuf::from("p4spectec/target"), PathBuf::from)
        .join("k-in-st")
}

/// Runs the suites under the named configuration, comparing each case with `krun`.
///
/// `name_suite` restricts the run to the registered suite of that name.
pub fn run(name_config: &str, name_suite: Option<&str>) -> Result<()> {
    let registration: Registration = harness::load("metacirc/k-in-st")?;
    let config = Configuration::find(&registration.configs, name_config)?;
    if config.skip_pending("k-in-st") {
        return Ok(());
    }
    if let Some(name) = name_suite
        && !registration.suites.iter().any(|suite| suite.name == name)
    {
        return Err(Error::from(format!("unknown K-in-ST suite {name}")));
    }
    let update = std::env::var_os("UPDATE_EXPECT").is_some();
    let text_mode = format!("metacirc k-in-st {}", config.describe());
    let start = Instant::now();
    let mut executed = 0;
    let mut mismatches = Vec::new();
    for suite in &registration.suites {
        // Skip suites of other names and suites without a tower for the base
        if name_suite.is_some_and(|name| suite.name != name) {
            continue;
        }
        let Some(path_tower) = suite.towers.get(config.base) else {
            continue;
        };
        let tower = TowerConfig::from_file(path_tower)
            .map_err(|error| Error::from(format!("{}: {error}", suite.name)))?;
        let Some(groups) = suite::groups(suite)? else {
            continue;
        };
        for (case, programs) in groups {
            for program in programs {
                let start_program = Instant::now();
                let name = program.expected.display().to_string();
                let verdict = check(&tower, config.runner(), suite, &case, &program, update)?;
                let secs = start_program.elapsed().as_secs_f64();
                match &verdict {
                    Ok(pass) => eprintln!("{name}: {pass} ({secs:.1}s)"),
                    Err(difference) => {
                        eprintln!("{name}: FAIL, {difference} ({secs:.1}s)");
                        mismatches.push(format!("{name} ({}): {difference}", suite.name));
                    }
                }
                executed += 1;
            }
        }
    }
    let text_summary = format!(
        "{text_mode}: executed={executed} mismatched={} elapsed={:.3}s",
        mismatches.len(),
        start.elapsed().as_secs_f64()
    );
    eprintln!("{text_summary}");
    // Report every mismatched program before failing
    if !mismatches.is_empty() {
        return Err(Error::from(format!(
            "{text_mode}: runs differ from krun's:\n{}",
            mismatches.join("\n")
        )));
    }
    eprintln!("{text_mode}: all {executed} runs matched krun's");
    Ok(())
}

/// Runs a program through the tower and compares the run with krun's, after
/// writing krun's under `update`: how it passes, or how the runs differ.
fn check(
    tower: &TowerConfig,
    options: runner::Config,
    suite: &KInStSuite,
    case: &Case,
    program: &Program,
    update: bool,
) -> Result<std::result::Result<&'static str, String>> {
    let path_init = case.init(program)?;
    let depth = program.depth(suite)?;
    let input = program.input.as_deref();
    let path_expected = program.expected("out");
    let path_search = program.expected("search");
    if update {
        let output = case.krun(&path_init, depth, input)?;
        fs::create_dir_all(program.expected.parent().unwrap_or(Path::new(".")))?;
        fs::write(&path_expected, output + "\n")?;
    }
    let expected = fs::read_to_string(&path_expected).map_err(|error| {
        Error::from(format!("{}: {error} (UPDATE_EXPECT=1 writes it)", path_expected.display()))
    })?;
    let paths_program = [case.path_definition(), path_init.clone()];
    let output = metacirc::krun(tower, &paths_program, options, depth)
        .unwrap_or_else(|error| format!("failed: {error}"));
    let differs = compare(&case.info, &output, &expected);
    if update {
        // Only a run that ends elsewhere than krun's needs the finals K allows
        if differs.is_err() {
            case.search(&path_init, depth, input, &path_search)?;
        } else if path_search.exists() {
            fs::remove_file(&path_search)?;
        }
    }
    Ok(differs.map(|()| "pass").or_else(|difference| {
        if !path_search.exists() {
            return Err(difference);
        }
        let finals = fs::read(&path_search).map_err(|error| error.to_string())?;
        reached(&case.info, &output, &finals)
            .map(|()| "pass by search")
            .map_err(|error| format!("{difference}, and {error}"))
    }))
}
