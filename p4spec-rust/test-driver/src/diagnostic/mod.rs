//! Native diagnostic snapshot acceptance
//!
//! Cases render product API reports or capture the product CLI's stderr.
//! Each input has an adjacent `.expect` file containing its complete output.
//! Comparisons preserve whitespace, and successful cases have empty expectations.

mod algo;
mod boundary;
mod cases;
mod elab;
mod parse;
mod prose;
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
    Prose,
    Splice,
    Boundary,
}

// = Acceptance runner

/// Executes one diagnostic suite and compares each case with its expectation.
fn run_suite(
    name_suite: &str,
    cases: &[&str],
    run_case: fn(&str) -> Result<Vec<Report>>,
) -> Result<()> {
    run_output_suite(name_suite, cases, |name| {
        let reports = run_case(name)?;
        let mut text = String::new();
        // Retain complete report output in emission order
        for report in reports {
            let rendered = Renderer::new(RenderConfig::default())
                .render_to_string(&report)
                .map_err(|error| failure(name, error))?;
            text.push_str(&rendered);
        }
        Ok(text)
    })
}

/// Compares complete rendered API or subprocess output with native expectations.
fn run_output_suite(
    name_suite: &str,
    cases: &[&str],
    run_case: impl Fn(&str) -> Result<String>,
) -> Result<()> {
    let progress = ProgressBar::new(cases.len() as u64);
    let path_suite = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("expected/diagnostic")
        .join(name_suite);

    // Exercise each input before comparing its complete rendered output
    for name in cases {
        let text = run_case(name)?;
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
pub fn run(suite: Option<Suite>, path_cli: Option<&Path>) -> Result<()> {
    // Require an explicit binary whenever subprocess acceptance is selected
    if matches!(suite, None | Some(Suite::Boundary)) && path_cli.is_none() {
        return Err(failure("boundary", "--cli is required for command diagnostic acceptance"));
    }
    // Keep source identities independent of the checkout location
    std::env::set_current_dir(Path::new(env!("CARGO_MANIFEST_DIR")).join("expected/diagnostic"))?;
    eprintln!("diagnostics: OCaml reference {}", cases::REVISION);

    // Absence selects every active suite in stage order
    match suite {
        Some(Suite::Parse) => run_parse(),
        Some(Suite::Elab) => run_suite("elab", cases::ELAB, elab::run),
        Some(Suite::Algo) => run_suite("algo", cases::ALGO, algo::run),
        Some(Suite::Prose) => run_suite("prose", cases::PROSE, prose::run),
        Some(Suite::Splice) => run_suite("splice", cases::SPLICE, splice::run),
        Some(Suite::Boundary) => run_boundary(path_cli),
        None => {
            run_parse()?;
            run_suite("elab", cases::ELAB, elab::run)?;
            run_suite("algo", cases::ALGO, algo::run)?;
            run_suite("prose", cases::PROSE, prose::run)?;
            run_suite("splice", cases::SPLICE, splice::run)?;
            run_boundary(path_cli)
        }
    }
}

/// Executes command cases after the binary-path admission check.
fn run_boundary(path_cli: Option<&Path>) -> Result<()> {
    let path_cli = path_cli.expect("boundary admission requires a CLI path");
    run_output_suite("boundary", cases::BOUNDARY, |name| boundary::run(path_cli, name))
}

/// Adapts parser failures to the shared diagnostic sequence.
fn run_parse() -> Result<()> {
    run_suite("parse", cases::PARSE, |name| parse::run(name).map(|report| vec![*report]))
}
