//! Micro tests: native execution of standalone spec-language sources
//!
//! Each source is a standalone specification in a registered suite,
//! which gives every source under its root one outcome at one stage.
//! A success source must pass every stage up to that stage;
//! a failure source must pass every earlier stage and fail at it.
//! The specdoc stage fails when AsciiDoc rendering of PL warns;
//! the run stage builds a runner and evaluates `$main()`.
//! Each language runs the stages registered for it,
//! and skips suites whose stage is outside them.
//! `<excludes>/<language>/*.exclude` lists sources skipped for one language.

use std::{
    collections::BTreeSet,
    path::{Path, PathBuf},
    time::Instant,
};

use p4spectec::lang::{al, data::arena::Arena, el, il, pl, sl};

use p4spectec::pass::{algo, elaborate, prosify, structure};

use p4spectec::specdoc::adoc;

use p4spectec::interface::p4::P4Interface;

use p4spectec::runner::{self, BuildError, Config, NullExtern, Runner};

use crate::{
    Error, ExecutionOptions, Result, corpus,
    diagnostic::Outcome,
    suite::{Language, MicroConfig, Stage},
};

/// Holds what the last completed stage produced for one source.
enum Output {
    Source(PathBuf),
    El(el::ast::Spec),
    Il(il::ast::Spec),
    Al(al::ast::Spec),
    Sl(sl::ast::Spec),
    Pl(pl::ast::Spec),
    Value,
}

/// Runs every corpus source through the stages of the selected language.
pub fn run(config: &MicroConfig, language: Language, options: ExecutionOptions) -> Result<()> {
    let ExecutionOptions { cache_on, no_det } = options;
    let det = !no_det;
    let config_runner = Config::new(cache_on, det, false);
    let text_cache = if cache_on { "on" } else { "off" };
    let text_mode = format!("micro {} cache={text_cache} det={det}", language.name());
    let start = Instant::now();
    let stages = config.stages.get(language);

    // A missing exclusion directory lists no entries
    let path_excludes = config.excludes.join(language.name());
    let excludes = if path_excludes.is_dir() {
        corpus::collect_excludes(&path_excludes)?
    } else {
        BTreeSet::new()
    };

    // Compare each source's first failing stage with its suite's verdict
    let guard_stdout = crate::output::SilenceStdout::new()?;
    let (mut executed, mut skipped, mut excluded) = (0, 0, 0);
    let mut mismatches = Vec::new();
    for suite in &config.suites {
        let paths = corpus::collect(&suite.root, ".watsup")?;
        // Skip suites whose stage this language does not run
        let Some(idx) = stages.iter().position(|stage| *stage == suite.stage) else {
            skipped += paths.len();
            continue;
        };
        // A success source stops after its stage; a failure source runs on
        let (stages_run, stage_expect) = match suite.outcome {
            Outcome::Success => (&stages[..=idx], None),
            Outcome::Failure => (stages, Some(suite.stage)),
        };
        for path in paths {
            // Skip sources excluded for this language
            if path.to_str().is_some_and(|path| excludes.contains(path)) {
                excluded += 1;
                continue;
            }
            let stage_fail = first_failure(&path, stages_run, config_runner)?;
            if stage_fail != stage_expect {
                mismatches.push(format!(
                    "{} ({}): expected {}, got {}",
                    path.display(),
                    suite.name,
                    describe(stage_expect),
                    describe(stage_fail)
                ));
            }
            executed += 1;
        }
    }
    guard_stdout.finish()?;

    // Report every mismatched source before failing
    eprintln!(
        "{text_mode}: executed={executed} skipped={skipped} excluded={excluded} mismatched={} elapsed={:.3}s",
        mismatches.len(),
        start.elapsed().as_secs_f64()
    );
    if !mismatches.is_empty() {
        return Err(Error::Invalid(format!(
            "{text_mode}: outcomes differ from their suite:\n{}",
            mismatches.join("\n")
        )));
    }
    eprintln!("{text_mode}: all {executed} file results matched their suite");
    Ok(())
}

/// Spells a source outcome for mismatch reports.
fn describe(stage_fail: Option<Stage>) -> String {
    match stage_fail {
        Some(stage) => format!("failure at {}", stage.name()),
        None => "success".to_owned(),
    }
}

/// Runs stages in order and returns the first one that rejects the source.
///
/// Structuring keeps rule groups only when the prose stage follows,
/// which needs them; SL execution needs them removed.
fn first_failure(path: &Path, stages: &[Stage], config: Config) -> Result<Option<Stage>> {
    let without_rule_groups = !stages.contains(&Stage::Prose);
    let mut output = Output::Source(path.to_owned());
    for &stage in stages {
        let output_next = match (stage, output) {
            (Stage::Parse, Output::Source(path)) => p4spectec::parse([path]).ok().map(Output::El),
            (Stage::Elab, Output::El(spec_el)) => elaborate::convert(spec_el).ok().map(Output::Il),
            (Stage::Algo, Output::Il(spec_il)) => algo::convert(spec_il).ok().map(Output::Al),
            (Stage::Structure, Output::Al(spec_al)) => {
                structure::convert(spec_al, without_rule_groups)
                    .ok()
                    .map(Output::Sl)
            }
            (Stage::Prose, Output::Sl(spec_sl)) => prosify::convert(spec_sl).ok().map(Output::Pl),
            // Rendering checks PL markup without changing it
            (Stage::Specdoc, Output::Pl(spec_pl)) => {
                let mut warnings = Vec::new();
                adoc::pl::render_spec(&mut warnings, &spec_pl);
                warnings.is_empty().then_some(Output::Pl(spec_pl))
            }
            (Stage::Run, Output::Al(spec_al)) => {
                eval_main(|arena| runner::build_al(arena, spec_al, config, NullExtern))
            }
            (Stage::Run, Output::Sl(spec_sl)) => {
                eval_main(|arena| runner::build_sl(arena, spec_sl, config, NullExtern))
            }
            (Stage::Run, Output::Pl(spec_pl)) => {
                eval_main(|arena| runner::build_pl(arena, spec_pl, config, NullExtern))
            }
            // Registered stages must chain from one language to the next
            (stage, _) => {
                return Err(Error::Invalid(format!(
                    "micro stage {} cannot follow the stages before it",
                    stage.name()
                )));
            }
        };
        // Stop at the first stage that rejects the source
        let Some(output_next) = output_next else {
            return Ok(Some(stage));
        };
        output = output_next;
    }
    Ok(None)
}

/// Builds a runner and evaluates `$main()`, returning `None` on failure.
fn eval_main(
    build: impl FnOnce(&mut Arena) -> std::result::Result<Runner<P4Interface, NullExtern>, BuildError>,
) -> Option<Output> {
    let mut arena = Arena::new();
    let mut runner = build(&mut arena).ok()?;
    runner
        .eval_func(&mut arena, "main", &[], &[])
        .result
        .ok()
        .map(|_| Output::Value)
}
