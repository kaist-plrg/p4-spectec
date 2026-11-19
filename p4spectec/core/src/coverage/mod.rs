//! SL instruction coverage over P4 programs and STF tests
//!
//! `cover_run` evaluates programs on one SL runner that is reset between inputs;
//! the simulator's `cover_sim` runs STF tests the same way.
//! A program's hits count whether it passes, fails, or panics;
//! a panic is caught and the next input starts from a reset runner.
//! `corpus` collects inputs, `instr` records hits, and `log` writes the report.

pub mod corpus;
mod error;
pub mod instr;
pub mod log;

use std::{
    panic::{self, AssertUnwindSafe},
    path::PathBuf,
};

use crate::lang::{data::arena::Arena, sl};

use crate::runner::{self, Config, Extern};

pub use error::CoverError;
pub use instr::{Cover, Hits, InstrHandler, Measure};

/// How one input ended.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Outcome {
    Pass,
    Fail,
    Crash,
}

impl Outcome {
    pub fn name(self) -> &'static str {
        match self {
            Self::Pass => "pass",
            Self::Fail => "fail",
            Self::Crash => "crash",
        }
    }
}

/// Progress through the inputs, by index.
#[derive(Clone, Copy, Debug)]
pub enum Progress {
    Start(usize),
    Finish(usize, Outcome),
}

/// Accumulated coverage and each input's outcome, in input order.
pub struct Coverage {
    pub cover: Cover,
    pub outcomes: Vec<Outcome>,
}

impl Coverage {
    /// Summarizes the outcomes as `N (pass P, fail F, crash C), excluded E`.
    pub fn describe_outcomes(&self, excluded: usize) -> String {
        let count = |outcome| self.outcomes.iter().filter(|o| **o == outcome).count();
        format!(
            "{} (pass {}, fail {}, crash {}), excluded {excluded}",
            self.outcomes.len(),
            count(Outcome::Pass),
            count(Outcome::Fail),
            count(Outcome::Crash)
        )
    }
}

/// Runs `body` and classifies its result, catching a panic as a crash.
pub fn run_catching<T, E>(body: impl FnOnce() -> Result<T, E>) -> Outcome {
    match panic::catch_unwind(AssertUnwindSafe(body)) {
        Ok(Ok(_)) => Outcome::Pass,
        Ok(Err(_)) => Outcome::Fail,
        Err(_) => Outcome::Crash,
    }
}

/// Evaluates each program with `external` through `relation`
/// and records SL instruction coverage.
///
/// `config` must disable `tco`.
pub fn cover_run<Ext: Extern>(
    spec_sl: &sl::ast::Spec,
    config: Config,
    external: Ext,
    relation: &str,
    includes: &[PathBuf],
    paths_p4: &[PathBuf],
    mut on_progress: impl FnMut(Progress),
) -> Result<Coverage, CoverError> {
    let mut cover = Cover::new(spec_sl);
    let mut arena = Arena::new();
    let mut runner = runner::build_sl(&mut arena, spec_sl.clone(), config, external)?;
    let hits = Hits::new(cover.len());
    runner
        .hooks_mut()
        .register(Box::new(InstrHandler::new(hits.clone())));

    let mut outcomes = Vec::with_capacity(paths_p4.len());
    for (index, path) in paths_p4.iter().enumerate() {
        on_progress(Progress::Start(index));
        let outcome = run_catching(|| {
            let program = runner
                .parse_program(&mut arena, includes, std::slice::from_ref(path))
                .map_err(drop)?;
            runner
                .eval_program(&mut arena, relation, program)
                .result
                .map_err(drop)
        });
        // Count hits up to the end or the panic, then start the next program afresh
        cover.add_hits(&hits);
        runner.reset();
        arena.reset_values();
        outcomes.push(outcome);
        on_progress(Progress::Finish(index, outcome));
    }
    Ok(Coverage { cover, outcomes })
}
