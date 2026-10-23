//! IL-to-AL scripts converted natively and through the meta specification
//!
//! Each script is converted to AL by the native `algo` pass and by the IL-to-AL
//! specification, and its `main` is executed in three ways:
//! native algo with native execution,
//! meta algo with native execution,
//! and meta algo with execution by the AL specification.
//! Each path records pass, fail, reject (no AL), or panic (native
//! interpreter abort); the outcomes are compared with the registered expectation.

use std::{path::PathBuf, time::Instant};

use expect_test::expect_file;

use p4spectec::{
    interface::{
        self,
        meta::shared::{decode::Decode, encode::Encode},
    },
    lang::{al::ast as al, data::arena::Arena},
    metacirc::{
        self, ExecutionMode, Tower, TowerConfig,
        config::{Lang, LevelConfig, ProgramConfig},
    },
    runner::{Config, NullExtern, Runner, Spec},
};

use crate::{
    Error, Result, corpus,
    suite::{self, AlgoRunConfig},
};

/// Calls `main` of converted AL with the native AL interpreter.
///
/// An interpreter panic is recorded rather than aborting the suite.
fn run_native(spec_al: al::Spec, config: Config) -> &'static str {
    let run = std::panic::AssertUnwindSafe(|| {
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
    });
    match std::panic::catch_unwind(run) {
        Ok(true) => "pass",
        Ok(false) => "fail",
        Err(_) => "panic",
    }
}

/// Converts a script to AL through a single-level tower entered by `Algo_IL`.
///
/// `None` means the script has no AL: elaboration failed,
/// the IL-to-AL specification has no derivation, or the result is not one AL specification.
fn algo_meta(config_tower: &TowerConfig, paths: &[PathBuf], config: Config) -> Option<al::Spec> {
    let mut arena = Arena::new();
    // Encode the elaborated script as the tower's IL input
    let spec_il = p4spectec::elab(paths).ok()?;
    let value_il = spec_il.encode(&mut arena).ok()?;
    let program = ProgramConfig::Il { spec_il };
    let mut tower = Tower::build(&mut arena, config_tower, &program, config).ok()?;
    let value = tower.patch(&mut arena, value_il).ok()?;
    // The entry's single output is the AL specification
    let values = tower.eval_program(&mut arena, value).result.ok()?;
    let [value_al] = values.as_slice() else {
        return None;
    };
    al::Spec::decode(&mut arena, *value_al).ok()
}

/// Runs registered scripts through every algo and execution path.
pub fn run(config_run: &AlgoRunConfig) -> Result<()> {
    let start = Instant::now();
    let config = Config::new(true, true, false);
    let tower = TowerConfig::from_file(&config_run.tower)
        .map_err(|error| Error::Invalid(error.to_string()))?;
    let level_algo =
        LevelConfig { specdir: config_run.algo.clone(), rel: "Algo_IL".into(), lang: Lang::Il };
    let tower_algo = TowerConfig::new(ExecutionMode::Sl, vec![level_algo])
        .map_err(|error| Error::Invalid(error.to_string()))?;
    let guard_stdout = crate::output::SilenceStdout::new()?;
    let mut results = Vec::new();
    for suite in &config_run.suites {
        let mut actual = String::new();
        for path in corpus::collect(&suite.root, ".watsup")? {
            let paths = std::slice::from_ref(&path);
            // Native algo, native execution
            let native = p4spectec::algo(paths).map(|spec_al| run_native(spec_al, config));
            // Meta algo, native execution
            let meta =
                algo_meta(&tower_algo, paths, config).map(|spec_al| run_native(spec_al, config));
            // Meta algo, execution by the AL specification
            let meta_meta = meta
                .is_some()
                .then(|| metacirc::run(&tower, &[], paths, config).is_ok());
            let text_meta_meta = match meta_meta {
                None => "reject",
                Some(true) => "pass",
                Some(false) => "fail",
            };
            let text_outcome =
                [native.unwrap_or("reject"), meta.unwrap_or("reject"), text_meta_meta].join("/");
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
