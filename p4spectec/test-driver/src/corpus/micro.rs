//! Micro tests: native execution of standalone spec-language sources
//!
//! Each `testdata/spec/{neg,pos}/<stage>` directory is one suite,
//! which gives every source under it one outcome at one stage.
//! A `pos` source must pass every stage up to that stage;
//! a `neg` source must pass every earlier stage and fail at it.
//! The specdoc stage fails when AsciiDoc rendering of PL warns;
//! the run stage builds a runner and evaluates `$main()`.
//! Each language runs the stages listed by `stages`,
//! and skips suites whose stage is outside them.
//! `excludes/micro/<language>/*.exclude` lists sources skipped for one language.

use std::{
    collections::BTreeSet,
    path::{Path, PathBuf},
    time::Instant,
};

use p4spectec::lang::{al, data::arena::Arena, el, il, pl, sl};

use p4spectec::pass::{algo, elaborate, prosify, structure};

use p4spectec::specdoc::adoc;

use p4spectec::runner::{Config, NullExtern, Spec};

use crate::{
    Error, ExecutionOptions, Result,
    corpus::{self, log::TestLog},
    diagnostic::Outcome,
    util::{self, Language},
};

/// Names a stage of the spec-language pipeline, in pipeline order.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Stage {
    Parse,
    Elab,
    Algo,
    Structure,
    Prose,
    Specdoc,
    Run,
}

impl Stage {
    const ALL: [Self; 7] = [
        Self::Parse,
        Self::Elab,
        Self::Algo,
        Self::Structure,
        Self::Prose,
        Self::Specdoc,
        Self::Run,
    ];

    /// Returns the stage's directory name.
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

/// Lists the stages one interpreter language runs, in pipeline order.
fn stages(language: Language) -> &'static [Stage] {
    match language {
        Language::Al => &[Stage::Parse, Stage::Elab, Stage::Algo, Stage::Run],
        Language::Sl => &[Stage::Parse, Stage::Elab, Stage::Algo, Stage::Structure, Stage::Run],
        Language::Pl => &Stage::ALL,
    }
}

/// Holds a directory of spec-language sources with one verdict.
struct Suite {
    name: String,
    root: PathBuf,
    outcome: Outcome,
    stage: Stage,
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

/// Runs every corpus source through the stages of the selected language.
pub fn run(language: Language, options: ExecutionOptions, path_log: Option<&Path>) -> Result<()> {
    let ExecutionOptions { cache_on, no_det } = options;
    let det = !no_det;
    let config_runner = Config::new(cache_on, det, false);
    let text_cache = if cache_on { "on" } else { "off" };
    let text_mode = format!("micro {} cache={text_cache} det={det}", language.name());
    let start = Instant::now();
    let stages = stages(language);

    // A missing exclusion directory lists no entries
    let path_excludes = Path::new("excludes/micro").join(language.name());
    let excludes = if path_excludes.is_dir() {
        corpus::collect_excludes(&path_excludes)?
    } else {
        BTreeSet::new()
    };

    // Collect every suite first so the log knows how many sources run
    let mut suites = Vec::new();
    for (dir, outcome) in [("neg", Outcome::Failure), ("pos", Outcome::Success)] {
        for stage in Stage::ALL {
            let root = Path::new("testdata/spec").join(dir).join(stage.name());
            if root.is_dir() {
                let paths = corpus::collect(&root, ".watsup")?;
                let name = format!("{dir}-{}", stage.name());
                suites.push((Suite { name, root, outcome, stage }, paths));
            }
        }
    }
    let len = suites
        .iter()
        .filter(|(suite, _)| stages.contains(&suite.stage))
        .map(|(_, paths)| paths.len())
        .sum();
    let mut log = TestLog::create(path_log, &format!("micro-{}", language.name()), len)?;
    log.note(&text_mode)?;

    // Compare each source's first failing stage with its suite's verdict
    let guard_stdout = super::output::SilenceStdout::new()?;
    let (mut executed, mut skipped, mut excluded) = (0, 0, 0);
    let mut mismatches = Vec::new();
    for (suite, paths) in &suites {
        log.suite(&suite.name, &suite.root)?;
        // Skip suites whose stage this language does not run
        let Some(idx) = stages.iter().position(|stage| *stage == suite.stage) else {
            log.note(&format!(
                "skipped {} sources: {} does not run stage {}",
                paths.len(),
                language.name(),
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
            // Skip sources excluded for this language
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
