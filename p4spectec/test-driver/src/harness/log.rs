//! Per-test timing logs for corpus runs
//!
//! `--log DIR` makes each run create `DIR/<name>-<UTC time>.log`,
//! refusing to overwrite an existing file.
//! `TestLog::begin` writes `[  n/len] path ... ` before a test starts
//! and `TestLog::end` appends its outcome, duration, and running total,
//! so `tail -f` shows progress and a stalled test leaves its line open.
//! `TestLog::finish` closes with the run summary and the slowest tests.
//! Without `--log`, the log writes nothing.

use std::{
    fs::{self, File},
    io::Write,
    path::{Path, PathBuf},
    time::{Instant, SystemTime},
};

use super::{Error, Result};

/// Number of slowest tests listed when a run finishes.
const LEN_SLOWEST: usize = 10;

/// Writes the timing log of one driver run.
pub(crate) struct TestLog {
    file: Option<File>,
    start: Instant,
    start_test: Instant,
    len: usize,
    idx: usize,
    root: PathBuf,
    path_test: PathBuf,
    timings: Vec<(f64, PathBuf)>,
}

impl TestLog {
    /// Creates `<dir>/<name>-<UTC time>.log` for a run of `len` tests.
    ///
    /// Without a directory the log only counts tests.
    pub(crate) fn create(dir: Option<&Path>, name: &str, len: usize) -> Result<Self> {
        let start = Instant::now();
        let mut log = Self {
            file: None,
            start,
            start_test: start,
            len,
            idx: 0,
            root: PathBuf::new(),
            path_test: PathBuf::new(),
            timings: Vec::new(),
        };
        let Some(dir) = dir else {
            return Ok(log);
        };
        let secs = SystemTime::now()
            .duration_since(SystemTime::UNIX_EPOCH)?
            .as_secs();
        let (year, month, day, hour, minute, second) = utc(secs);
        // Each run owns a new file; a name taken within the same second fails
        fs::create_dir_all(dir)?;
        let path = fs::canonicalize(dir)?.join(format!(
            "{name}-{year:04}{month:02}{day:02}T{hour:02}{minute:02}{second:02}Z.log"
        ));
        let mut file = File::options()
            .write(true)
            .create_new(true)
            .open(&path)
            .map_err(|error| Error::from(format!("{}: {error}", path.display())))?;
        writeln!(file, "p4spectec-test {name}")?;
        writeln!(
            file,
            "started {year:04}-{month:02}-{day:02} {hour:02}:{minute:02}:{second:02} UTC"
        )?;
        eprintln!("log: {}", path.display());
        log.file = Some(file);
        Ok(log)
    }

    /// Writes one line of free text, such as collection counts.
    pub(crate) fn note(&mut self, text: &str) -> Result<()> {
        self.write(&format!("{text}\n"))
    }

    /// Starts a suite section whose test paths are shown relative to `root`.
    pub(crate) fn suite(&mut self, name: &str, root: &Path) -> Result<()> {
        self.root = root.to_owned();
        self.write(&format!("\n== {name} ({})\n", root.display()))
    }

    /// Opens the line of the next test and starts its clock.
    pub(crate) fn begin(&mut self, path: &Path) -> Result<()> {
        self.idx += 1;
        self.path_test = path.to_owned();
        let text = self.head(path);
        self.write(&text)?;
        self.start_test = Instant::now();
        Ok(())
    }

    /// Closes the open test line with its outcome and timing.
    pub(crate) fn end(&mut self, outcome: &str) -> Result<()> {
        let secs = self.start_test.elapsed().as_secs_f64();
        let secs_total = self.start.elapsed().as_secs_f64();
        self.timings
            .push((secs, std::mem::take(&mut self.path_test)));
        self.write(&format!("{outcome}  {secs:.2}s  ({secs_total:.1}s)\n"))
    }

    /// Writes the whole line of a test that did not run.
    pub(crate) fn skip(&mut self, path: &Path, outcome: &str) -> Result<()> {
        self.idx += 1;
        let text = self.head(path);
        self.write(&format!("{text}{outcome}\n"))
    }

    /// Writes the run summary and the slowest tests.
    pub(crate) fn finish(mut self, summary: &str) -> Result<()> {
        let mut text = format!("\n{summary}\n");
        // List the slowest tests first, with full paths
        self.timings
            .sort_by(|(secs_a, _), (secs_b, _)| secs_b.total_cmp(secs_a));
        self.timings.truncate(LEN_SLOWEST);
        if !self.timings.is_empty() {
            text.push_str(&format!("\nslowest {}:\n", self.timings.len()));
        }
        for (secs, path) in &self.timings {
            text.push_str(&format!("{secs:>8.2}s  {}\n", path.display()));
        }
        self.write(&text)
    }

    /// Formats the counter and suite-relative path that open a test line.
    fn head(&self, path: &Path) -> String {
        let width = self.len.to_string().len();
        let path = path.strip_prefix(&self.root).unwrap_or(path);
        format!("[{:>width$}/{}] {} ... ", self.idx, self.len, path.display())
    }

    /// Appends text to the log file, if any.
    fn write(&mut self, text: &str) -> Result<()> {
        if let Some(file) = &mut self.file {
            file.write_all(text.as_bytes())?;
        }
        Ok(())
    }
}

/// Splits Unix seconds into UTC year, month, day, hour, minute, and second.
///
/// Days follow Hinnant's `civil_from_days` over 400-year eras
/// that start on March 1, so each leap day ends its year.
fn utc(secs: u64) -> (u64, u64, u64, u64, u64, u64) {
    let (days, secs_day) = (secs / 86_400, secs % 86_400);
    // Count days from 0000-03-01 instead of 1970-01-01
    let days = days + 719_468;
    let era = days / 146_097;
    let days_era = days % 146_097;
    let years_era = (days_era - days_era / 1_460 + days_era / 36_524 - days_era / 146_096) / 365;
    let days_year = days_era - (365 * years_era + years_era / 4 - years_era / 100);
    // Months count from March; January and February belong to the next year
    let month_march = (5 * days_year + 2) / 153;
    let day = days_year - (153 * month_march + 2) / 5 + 1;
    let month = if month_march < 10 { month_march + 3 } else { month_march - 9 };
    let year = era * 400 + years_era + u64::from(month <= 2);
    (year, month, day, secs_day / 3_600, secs_day % 3_600 / 60, secs_day % 60)
}
