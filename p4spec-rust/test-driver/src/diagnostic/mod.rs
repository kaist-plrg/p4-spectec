//! Native diagnostic snapshot acceptance
//!
//! Each case executes a product API and renders its report without colors.
//! Each input has an adjacent `.expect` file containing its complete output.
//! Comparisons preserve whitespace, and successful cases have empty expectations.

mod algo;
mod cases;
mod elab;
mod parse;
mod splice;

use std::path::Path;

use clap::ValueEnum;
use expect_test::expect_file;
use indicatif::ProgressBar;
use p4spec_rust::diagnostic::{RenderConfig, Renderer, Report};

use crate::{Error, Result};

// = Helpers

fn failure(name: &str, message: impl std::fmt::Display) -> Error {
    Error::Invalid(format!("{name}: {message}"))
}

// = Suites

/// Selects an implemented diagnostic snapshot suite.
#[derive(Clone, Copy, Debug, ValueEnum)]
pub enum Suite {
    Parse,
    Elab,
    Algo,
    Splice,
}

// = Acceptance runner

/// Executes one diagnostic suite and compares each case with its expectation.
fn run_suite(
    name_suite: &str,
    cases: &[&str],
    run_case: fn(&str) -> Result<Vec<Report>>,
) -> Result<()> {
    let progress = ProgressBar::new(cases.len() as u64);
    let path_suite = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("expected/diagnostic")
        .join(name_suite);

    // Render each input's diagnostics in emission order
    for name in cases {
        let reports = run_case(name)?;
        let mut text = String::new();
        for report in reports {
            let rendered = Renderer::new(RenderConfig::default())
                .render_to_string(&report)
                .map_err(|error| failure(name, error))?;
            text.push_str(&rendered);
        }
        // Compare the complete output without trimming codespan whitespace
        let path = path_suite.join(name).with_extension("expect");
        expect_file![path].assert_eq(&text);
        progress.inc(1);
    }
    progress.finish_and_clear();

    eprintln!("diagnostics/{name_suite}: {} cases passed", cases.len());
    Ok(())
}

/// Executes selected diagnostic inputs and compares their rendered output.
pub fn run(suite: Option<Suite>) -> Result<()> {
    // Keep source identities independent of the checkout location
    std::env::set_current_dir(Path::new(env!("CARGO_MANIFEST_DIR")).join("expected/diagnostic"))?;
    eprintln!("diagnostics: OCaml reference {}", cases::REVISION);

    // Absence selects every active suite in stage order
    match suite {
        Some(Suite::Parse) => run_parse(),
        Some(Suite::Elab) => run_suite("elab", cases::ELAB, elab::run),
        Some(Suite::Algo) => run_suite("algo", cases::ALGO, algo::run),
        Some(Suite::Splice) => run_suite("splice", cases::SPLICE, splice::run),
        None => {
            run_parse()?;
            run_suite("elab", cases::ELAB, elab::run)?;
            run_suite("algo", cases::ALGO, algo::run)?;
            run_suite("splice", cases::SPLICE, splice::run)
        }
    }
}

/// Adapts parser failures to the shared diagnostic sequence.
fn run_parse() -> Result<()> {
    run_suite("parse", cases::PARSE, |name| parse::run(name).map(|report| vec![*report]))
}
