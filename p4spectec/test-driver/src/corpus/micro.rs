//! Micro tests: native execution of standalone spec-language sources
//!
//! `suites/micro.toml` registers each suite as a directory of sources
//! with one outcome at one stage.
//! A success source must pass every stage up to that stage;
//! a failure source must pass every earlier stage and fail at it.
//! The specdoc stage fails when AsciiDoc rendering of PL warns;
//! the run stage builds a runner and evaluates `$main()`.
//! Each configuration runs its `stages` in order,
//! and skips suites whose stage is outside them.

use std::{
    collections::BTreeSet,
    path::{Path, PathBuf},
    time::Instant,
};

use p4spectec::lang::{al, data::arena::Arena, el, il, pl, sl};

use p4spectec::pass::{algo, elaborate, prosify, structure};

use p4spectec::specdoc::adoc;

use p4spectec::runner::{Config, NullExtern, Spec};

use serde::Deserialize;

use crate::{
    Error, Result,
    corpus::{self, log::TestLog},
    diagnostic::Outcome,
    util,
};

/// Names a stage of the spec-language pipeline, in pipeline order.
#[derive(Clone, Copy, Debug, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "lowercase")]
pub enum Stage {
    Parse,
    Elab,
    Algo,
    Structure,
    Prose,
    Specdoc,
    Run,
}

impl Stage {
    /// Returns the stage's registration spelling.
    fn name(self) -> &'static str {
        match self {
            Self::Parse => "parse",
            Self::Elab => "elab",
            Self::Algo => "algo",
            Self::Structure => "structure",
            Self::Prose => "prose",
            Self::Specdoc => "specdoc",
            Self::Run => "run",
        }
    }
}

/// Names the stages, runner options, and exclusion lists of one micro run.
#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct MicroConfiguration {
    pub name: String,
    pub cache: bool,
    pub det: bool,
    pub stages: Vec<Stage>,
    pub excludes: Vec<PathBuf>,
}

/// Registers a directory of spec-language sources with one verdict.
#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Suite {
    pub name: String,
    pub root: PathBuf,
    pub outcome: Outcome,
    pub stage: Stage,
}

/// Supplies micro configurations and suites.
#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Registration {
    pub configs: Vec<MicroConfiguration>,
    pub suites: Vec<Suite>,
}

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

/// Runs every registered source through the stages of the named configuration.
pub fn run(name_config: &str, path_log: Option<&Path>) -> Result<()> {
    let registration: Registration = util::load("micro")?;
    let config = registration
        .configs
        .iter()
        .find(|config| config.name == name_config)
        .ok_or_else(|| Error::from(format!("no configuration named {name_config}")))?;
    let config_runner = Config::new(config.cache, config.det, false);
    let text_mode = format!("micro {} cache={} det={}", config.name, config.cache, config.det);
    let start = Instant::now();
    let stages = config.stages.as_slice();
    let mut excludes = BTreeSet::new();
    for path in &config.excludes {
        excludes.extend(corpus::collect_excludes(path)?);
    }

    // Collect every suite first so the log knows how many sources run
    let suites = registration
        .suites
        .iter()
        .map(|suite| Ok((suite, corpus::collect(&suite.root, ".watsup")?)))
        .collect::<Result<Vec<_>>>()?;
    let len = suites
        .iter()
        .filter(|(suite, _)| stages.contains(&suite.stage))
        .map(|(_, paths)| paths.len())
        .sum();
    let mut log = TestLog::create(path_log, &format!("micro-{}", config.name), len)?;
    log.note(&text_mode)?;

    // Compare each source's first failing stage with its suite's verdict
    let guard_stdout = super::output::SilenceStdout::new()?;
    let (mut executed, mut skipped, mut excluded) = (0, 0, 0);
    let mut mismatches = Vec::new();
    for (suite, paths) in &suites {
        log.suite(&suite.name, &suite.root)?;
        // Skip suites whose stage this configuration does not run
        let Some(idx) = stages.iter().position(|stage| *stage == suite.stage) else {
            log.note(&format!(
                "skipped {} sources: {} does not run stage {}",
                paths.len(),
                config.name,
                suite.stage.name()
            ))?;
            skipped += paths.len();
            continue;
        };
        // A success source stops after its stage; a failure source runs on
        let (stages_run, stage_expect) = match suite.outcome {
            Outcome::Success => (&stages[..=idx], None),
            Outcome::Failure => (stages, Some(suite.stage)),
        };
        for path in paths {
            // Skip sources excluded for this configuration
            if path.to_str().is_some_and(|path| excludes.contains(path)) {
                log.skip(path, "exclude")?;
                excluded += 1;
                continue;
            }
            log.begin(path)?;
            let stage_fail = first_failure(path, stages_run, config_runner)?;
            // Name the expected outcome on lines that differ from their suite
            if stage_fail == stage_expect {
                log.end(&describe(stage_fail))?;
            } else {
                log.end(&format!("{}, expected {}", describe(stage_fail), describe(stage_expect)))?;
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
    let text_summary = format!(
        "{text_mode}: executed={executed} skipped={skipped} excluded={excluded} mismatched={} elapsed={:.3}s",
        mismatches.len(),
        start.elapsed().as_secs_f64()
    );
    eprintln!("{text_summary}");
    log.finish(&text_summary)?;
    if !mismatches.is_empty() {
        return Err(Error::from(format!(
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
            (Stage::Run, Output::Al(spec_al)) => eval_main(Spec::Al(spec_al), config),
            (Stage::Run, Output::Sl(spec_sl)) => eval_main(Spec::Sl(spec_sl), config),
            (Stage::Run, Output::Pl(spec_pl)) => eval_main(Spec::Pl(spec_pl), config),
            // Registered stages must chain from one language to the next
            (stage, _) => {
                return Err(Error::from(format!(
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
fn eval_main(spec: Spec, config: Config) -> Option<Output> {
    let mut arena = Arena::new();
    let mut runner = util::build(&mut arena, spec, config, NullExtern).ok()?;
    runner
        .eval_func(&mut arena, "main", &[], &[])
        .result
        .ok()
        .map(|_| Output::Value)
}
