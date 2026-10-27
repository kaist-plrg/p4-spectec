use std::{
    path::{Path, PathBuf},
    time::Instant,
};

use expect_test::expect_file;
use indicatif::{ProgressBar, ProgressStyle};

use p4spectec::lang::{
    common::source::Span,
    data::{arena::Arena, typ},
};

use p4spectec::interface::p4::{
    error::P4Error,
    parse::{parse_file, parse_string},
    unparse::P4Unparser,
};

use crate::{
    Error, Result,
    corpus::{self, Outcome, Results},
    log::TestLog,
    suite::{self, ParseConfig},
};

fn roundtrip(unparser: &P4Unparser, includes: &[PathBuf], path: &Path) -> Result<Outcome> {
    let mut arena = Arena::new();
    let program = match parse_file(&mut arena, includes, path) {
        Ok(program) => program,
        Err(error) => match error {
            P4Error { kind: p4spectec::interface::p4::error::P4ErrorKind::Syntax(_), .. } => {
                return Ok(Outcome::ParseFail);
            }
            _ => {
                return Err(Error::from(format!("{}: {}", path.display(), error.into_report())));
            }
        },
    };
    // The parser's root nonterminal is the program's declared type
    let typ_program = typ::make::var(
        p4spectec::phrase!(node: "p4program".into(), span: Span::default()),
        Vec::new(),
    );
    let text = unparser
        .render(&arena, &typ_program, &program)
        .map_err(|error| Error::from(format!("{}: {error}", path.display())))?;
    let program_roundtrip = match parse_string(&mut arena, path, &text) {
        Ok(program) => program,
        Err(error) => match error {
            P4Error { kind: p4spectec::interface::p4::error::P4ErrorKind::Syntax(_), .. } => {
                return Ok(Outcome::ReparseFail);
            }
            _ => {
                return Err(Error::from(format!("{}: {}", path.display(), error.into_report())));
            }
        },
    };
    // Canonical bodies ignore source spans and type notes, like IL value equality
    let outcome =
        if arena.value.canon_id(program.node) == arena.value.canon_id(program_roundtrip.node) {
            Outcome::Pass
        } else {
            Outcome::RoundtripFail
        };
    Ok(outcome)
}

/// Checks registered P4 parsing corpora against their expected outcomes.
pub fn run(config: &ParseConfig, path_log: Option<&Path>) -> Result<()> {
    let start = Instant::now();
    // Keep each root's paths apart so the log shows them by root
    let mut suites = Vec::new();
    for suite in &config.suites {
        let path_expected = suite::expected_path(&suite.expected);
        let mut roots = Vec::new();
        for root in &suite.roots {
            roots.push((root, corpus::collect(root, ".p4")?));
        }
        suites.push((&suite.name, roots, Results::new(expect_file![path_expected])));
    }
    if suites.is_empty() {
        return Err(Error::from("no P4 parsing suites registered"));
    }
    let collected: usize = suites
        .iter()
        .flat_map(|(_, roots, _)| roots)
        .map(|(_, paths)| paths.len())
        .sum();
    let mut log = TestLog::create(path_log, "p4parse", collected)?;
    let text_collect =
        format!("P4 parser: collected={collected}, excluded=0; preparing print hints");
    eprintln!("{text_collect}");
    log.note(&text_collect)?;
    let spec_al = p4spectec::algo(&config.spec)?;
    let unparser = P4Unparser::from_al_spec(&spec_al);
    log.note(&format!("print hints prepared in {:.1}s", start.elapsed().as_secs_f64()))?;
    let includes = &config.includes;
    let progress = ProgressBar::new(collected as u64).with_style(ProgressStyle::with_template(
        "[{bar:24}] {pos}/{len} {elapsed_precise} {msg}",
    )?);
    let mut passed = 0;
    for (name, roots, results) in &mut suites {
        for (root, paths) in roots.iter() {
            log.suite(name, root)?;
            for path in paths {
                progress.set_message(path.display().to_string());
                log.begin(path)?;
                let outcome = roundtrip(&unparser, includes, path)?;
                log.end(outcome.name())?;
                if outcome == Outcome::Pass {
                    passed += 1;
                }
                results.record(path, outcome)?;
                progress.inc(1);
            }
        }
    }
    progress.finish_with_message("complete");
    let text_summary = format!(
        "P4 parser collected={collected} excluded=0 executed={collected} pass={passed} fail={} elapsed={:.3}s",
        collected - passed,
        start.elapsed().as_secs_f64()
    );
    eprintln!("{text_summary}");
    log.finish(&text_summary)?;
    for (_, _, results) in suites {
        results.check();
    }
    eprintln!("P4 parser: all {collected} file results matched expected");
    Ok(())
}
