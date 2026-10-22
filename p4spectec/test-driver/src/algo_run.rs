//! IL-to-AL scripts lowered natively and through the meta specification
//!
//! Each script is lowered by the native pass and by the IL-to-AL specification,
//! and its `main` is executed in three ways:
//! native lowering with native execution,
//! meta lowering with native execution,
//! and meta lowering with execution by the AL specification.
//! The three outcomes are compared with the registered expectation.

use std::time::Instant;

use expect_test::expect_file;

use p4spectec::{
    interface,
    lang::{al::ast as al, data::arena::Arena},
    metacirc::{self, ExecutionMode, TowerConfig},
    runner::{Config, NullExtern, Runner, Spec},
};

use crate::{
    Error, Result, corpus,
    suite::{self, AlgoRunConfig},
};

/// The outcome of one lowering and execution path.
fn outcome_text(lowered: bool, passed: bool) -> &'static str {
    match (lowered, passed) {
        (false, _) => "reject",
        (true, true) => "pass",
        (true, false) => "fail",
    }
}

/// Calls `main` of lowered AL with the native AL interpreter.
fn run_native(spec_al: al::Spec, config: Config) -> bool {
    let mut arena = Arena::new();
    let Ok(mut runner) =
        Runner::new(&mut arena, Spec::Al(spec_al), config, interface::al(), NullExtern)
    else {
        return false;
    };
    runner
        .eval_func(&mut arena, "main", &[], &[])
        .result
        .is_ok()
}

/// Runs registered scripts through every lowering and execution path.
pub fn run(config_run: &AlgoRunConfig) -> Result<()> {
    let start = Instant::now();
    let config = Config::new(true, true, false);
    let tower = TowerConfig::from_file(&config_run.tower)
        .map_err(|error| Error::Invalid(error.to_string()))?;
    let guard_stdout = crate::output::SilenceStdout::new()?;
    let mut results = Vec::new();
    for suite in &config_run.suites {
        let mut actual = String::new();
        for path in corpus::collect(&suite.root, ".watsup")? {
            let paths = std::slice::from_ref(&path);
            // Native lowering, native execution
            let native = p4spectec::algo(paths).map(|spec_al| run_native(spec_al, config));
            // Meta lowering, native execution
            let meta = metacirc::algo(paths, &config_run.algo, ExecutionMode::Sl, config)
                .map(|spec_al| run_native(spec_al, config));
            // Meta lowering, execution by the AL specification
            let meta_meta = meta
                .is_ok()
                .then(|| metacirc::run(&tower, &[], paths, config).is_ok());
            let text_outcome = [
                outcome_text(native.is_ok(), native.unwrap_or(false)),
                outcome_text(meta.is_ok(), meta.as_ref().is_ok_and(|passed| *passed)),
                outcome_text(meta_meta.is_some(), meta_meta.unwrap_or(false)),
            ]
            .join("/");
            actual.push_str(&format!("{text_outcome}\t{}\n", path.display()));
        }
        results.push((suite.expected.clone(), actual));
    }
    guard_stdout.finish()?;
    for (path_expected, actual) in results {
        expect_file![suite::expected_path(&path_expected)].assert_eq(&actual);
    }
    eprintln!(
        "algo-run: all script outcomes matched, elapsed={:.3}s",
        start.elapsed().as_secs_f64()
    );
    Ok(())
}
