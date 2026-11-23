//! Corpus discovery and execution for the integration tests
//!
//! `collect_excludes` reads exclusion lists.
//! A test runs a corpus in its own process, from the repository root:
//! `Corpus::files(dir, ".p4")?.excludes(dirs)?.run(name, run_case)?.check(outcome)`
//! runs every case not excluded through `run_case`
//! and checks that each reaches `outcome`,
//! panicking with the others and their details.
//! The run also writes `logs/<name>.log` under the repository root, whatever the working directory:
//! one `<outcome>,<paths>,<seconds>` line per case in corpus order
//! (seconds empty for an excluded case).

use std::{
    collections::BTreeSet,
    fs::{self, File},
    io::Write,
    path::{Path, PathBuf},
    time::Instant,
};

use super::{Outcome, Result, collect, output::SilenceStdout, root};

/// Bounds the cases a failure lists.
const LIMIT_CASES: usize = 20;

pub fn collect_excludes(dir: &Path) -> Result<BTreeSet<String>> {
    let mut excludes = BTreeSet::new();
    for path in collect(dir, ".exclude")? {
        let text = fs::read_to_string(path)?;
        // Split on LF only; preserve whitespace and CR in exclusion entries
        excludes.extend(
            text.split_terminator('\n')
                .filter(|line| !line.starts_with('#'))
                .map(str::to_owned),
        );
    }
    Ok(excludes)
}

/// Cases of one corpus, each a list of paths relative to the repository root,
/// and the exclusion lists that skip some of them.
pub struct Corpus {
    cases: Vec<Vec<PathBuf>>,
    excludes: BTreeSet<String>,
}

impl Corpus {
    /// Makes a corpus of the given cases.
    pub fn new(cases: Vec<Vec<PathBuf>>) -> Self {
        Self { cases, excludes: BTreeSet::new() }
    }

    /// Collects one case per file ending in `suffix` under `dir`.
    pub fn files(dir: &str, suffix: &str) -> Result<Self> {
        let root = root();
        let cases = collect(&root.join(dir), suffix)?
            .into_iter()
            .map(|path| Ok(vec![path.strip_prefix(&root)?.to_path_buf()]))
            .collect::<Result<_>>()?;
        Ok(Self::new(cases))
    }

    /// Skips the cases with a path named by the `.exclude` files under `dirs`.
    pub fn excludes(mut self, dirs: &[&str]) -> Result<Self> {
        for dir in dirs {
            self.excludes.extend(collect_excludes(&root().join(dir))?);
        }
        Ok(self)
    }

    /// Runs every case not excluded through `run_case`,
    /// which gets the case's paths and answers its outcome and details.
    ///
    /// The interpreter's stdout is silenced meanwhile.
    /// Each case's line goes to `logs/<name>.log` under the repository root
    /// as soon as the case ends.
    pub fn run(
        &self,
        name: &str,
        mut run_case: impl FnMut(&[PathBuf]) -> Result<(Outcome, String)>,
    ) -> Result<Report> {
        // Open the log, replacing the previous run's
        let dir_log = root().join("logs");
        fs::create_dir_all(&dir_log)?;
        let mut file_log = File::create(dir_log.join(format!("{name}.log")))?;
        let mut report = Report { outcomes: Vec::new() };
        let guard_stdout = SilenceStdout::new()?;
        for paths in &self.cases {
            let text_case = paths
                .iter()
                .map(|path| path.display().to_string())
                .collect::<Vec<_>>()
                .join(",");
            // Record excluded cases without running them
            if paths.iter().any(|path| {
                path.to_str()
                    .is_some_and(|path| self.excludes.contains(path))
            }) {
                writeln!(file_log, "exclude,{text_case},")?;
                continue;
            }
            // Run the case and log its outcome with its duration
            let start = Instant::now();
            let (outcome, details) = run_case(paths)?;
            let secs = start.elapsed().as_secs_f64();
            writeln!(file_log, "{},{text_case},{secs:.2}", outcome.name())?;
            report.outcomes.push((text_case, outcome, details));
        }
        guard_stdout.finish()?;
        Ok(report)
    }
}

/// Outcomes of one corpus run, in corpus order.
pub struct Report {
    /// Each case run, with its outcome and failure details.
    outcomes: Vec<(String, Outcome, String)>,
}

impl Report {
    /// Checks that every case run reaches `outcome`.
    ///
    /// A failure panics with the other cases, at most `LIMIT_CASES`,
    /// each with its failure details.
    pub fn check(&self, outcome: Outcome) {
        // Collect the cases that ran to another outcome
        let mismatches: Vec<_> = self
            .outcomes
            .iter()
            .filter(|(_, other, _)| *other != outcome)
            .collect();
        if mismatches.is_empty() {
            return;
        }
        // Panic with them, at most `LIMIT_CASES`, as the test's failure message
        let mut text = format!(
            "{} of {} cases did not {}",
            mismatches.len(),
            self.outcomes.len(),
            outcome.name()
        );
        for (text_case, _, details) in mismatches.iter().take(LIMIT_CASES) {
            text.push_str(&format!("\n{text_case}"));
            for line_details in details.lines() {
                text.push_str(&format!("\n    {line_details}"));
            }
        }
        if mismatches.len() > LIMIT_CASES {
            text.push_str(&format!("\nand {} more", mismatches.len() - LIMIT_CASES));
        }
        panic!("{text}");
    }
}
