use std::{
    fs,
    path::{Path, PathBuf},
    time::Instant,
};

use expect_test::expect_file;
use indicatif::{ProgressBar, ProgressStyle};

use p4spec_rust::lang::data::value::ValueArena;

use p4spec_rust::interface::p4::{
    error::P4Error,
    parse::{parse_file, parse_string},
    unparse::P4Unparser,
};

use crate::{
    Error, Result,
    corpus::{self, Outcome, Results},
    suite::{self, Registry},
};

fn roundtrip(unparser: &P4Unparser, includes: &[PathBuf], path: &Path) -> Result<Outcome> {
    let mut arena = ValueArena::new();
    fs::File::open(path)?;
    let program = match parse_file(&mut arena, includes, path) {
        Ok(program) => program,
        Err(error) => match error {
            P4Error { kind: p4spec_rust::interface::p4::error::P4ErrorKind::Syntax(_), .. } => {
                return Ok(Outcome::ParseFail);
            }
            _ => {
                return Err(Error::Invalid(format!("{}: {}", path.display(), error.into_report())));
            }
        },
    };
    let text = unparser
        .render(&arena, &program)
        .map_err(|error| Error::Invalid(format!("{}: {error}", path.display())))?;
    let program_roundtrip = match parse_string(&mut arena, path, &text) {
        Ok(program) => program,
        Err(error) => match error {
            P4Error { kind: p4spec_rust::interface::p4::error::P4ErrorKind::Syntax(_), .. } => {
                return Ok(Outcome::ReparseFail);
            }
            _ => {
                return Err(Error::Invalid(format!("{}: {}", path.display(), error.into_report())));
            }
        },
    };
    // Canonical bodies ignore source spans and type notes, like IL value equality
    let outcome = if arena.canon_id(&program) == arena.canon_id(&program_roundtrip) {
        Outcome::Pass
    } else {
        Outcome::RoundtripFail
    };
    Ok(outcome)
}

/// Checks registered P4 parsing corpora against their expected outcomes.
pub fn run(registry: &Registry) -> Result<()> {
    let start = Instant::now();
    let mut suites = Vec::new();
    for suite in registry.parsing() {
        let path_expected = suite::expected_path(&suite.expected);
        let mut paths = Vec::new();
        for path in &suite.roots {
            paths.extend(corpus::collect(path, ".p4")?);
        }
        suites.push((paths, Results::new(expect_file![path_expected])));
    }
    if suites.is_empty() {
        return Err(Error::Invalid("no P4 parsing suites registered".into()));
    }
    let collected: usize = suites.iter().map(|(paths, _)| paths.len()).sum();
    eprintln!("P4 parser: collected={collected}, excluded=0; preparing print hints");
    let spec_al =
        p4spec_rust::algo(&registry.spec).map_err(|error| Error::Invalid(error.to_string()))?;
    let unparser = P4Unparser::from_al_spec(&spec_al);
    let includes = &registry.includes;
    for path in includes {
        fs::read_dir(path)?;
    }
    let progress = ProgressBar::new(collected as u64).with_style(
        ProgressStyle::with_template("[{bar:24}] {pos}/{len} {elapsed_precise} {msg}")
            .map_err(|error| Error::Invalid(error.to_string()))?,
    );
    let mut passed = 0;
    for (paths, results) in &mut suites {
        for path in paths.iter() {
            progress.set_message(path.display().to_string());
            let outcome = roundtrip(&unparser, includes, path)?;
            if outcome == Outcome::Pass {
                passed += 1;
            }
            results.record(path, outcome)?;
            progress.inc(1);
        }
    }
    progress.finish_with_message("complete");
    eprintln!(
        "P4 parser collected={collected} excluded=0 executed={collected} pass={passed} fail={} elapsed={:.3}s",
        collected - passed,
        start.elapsed().as_secs_f64()
    );
    for (_, results) in suites {
        results.check();
    }
    eprintln!("P4 parser: all {collected} file results matched expected");
    Ok(())
}
