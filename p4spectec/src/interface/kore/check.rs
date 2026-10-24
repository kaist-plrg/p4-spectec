//! Checking a run step by step against K
//!
//! Where a run of the specification ends elsewhere than krun's, K may still
//! allow each of its steps: K leaves the choice among rules of the same
//! priority open. Each next configuration must be one of those the search
//! binary of the kompiled definition finds one step on, and the interpreter
//! must take no step from the last configuration of a run that ends.
//! A nondeterministic program is checked along the run the specification
//! takes, without exploring the others.

use std::{
    fs,
    path::{Path, PathBuf},
    process::{Command, Stdio},
};

use super::{ast::Pattern, parse};

/// The configurations of a disjunction, as the search binary writes them.
fn alternatives(pattern: Pattern) -> Vec<Pattern> {
    match pattern {
        Pattern::App(name, _, args) if name == "\\or" => {
            args.into_iter().flat_map(alternatives).collect()
        }
        Pattern::App(name, _, _) if name == "\\bottom" => vec![],
        Pattern::App(name, _, mut args) if name == "\\equals" && args.len() == 2 => {
            vec![args.remove(1)]
        }
        pattern => vec![pattern],
    }
}

/// A step check of one run.
pub struct Checker {
    kompiled: PathBuf,
    /// The configuration given to K, and what K writes.
    current: PathBuf,
    next: PathBuf,
    /// The last configuration of the run, and its normal form.
    last: Option<(Pattern, String)>,
    steps: usize,
    /// Steps where K allows more than one configuration.
    branching: usize,
    failure: Option<String>,
}

impl Checker {
    /// A check against the binaries of a kompiled directory.
    pub fn new(kompiled: &Path) -> Self {
        let tmp = std::env::temp_dir();
        let id = std::process::id();
        Self {
            kompiled: kompiled.to_path_buf(),
            current: tmp.join(format!("kstep-{id}.kore")),
            next: tmp.join(format!("knext-{id}.kore")),
            last: None,
            steps: 0,
            branching: 0,
            failure: None,
        }
    }

    /// Runs `<kompiled>/<binary> <configuration> <depth> <next>`; whether it exits with 0.
    fn run_k(
        &self,
        binary: &str,
        pattern: &Pattern,
        depth: u64,
        extra: &[&str],
    ) -> Result<bool, String> {
        fs::write(&self.current, pattern.to_string()).map_err(|error| error.to_string())?;
        // The interpreter appends to its output file
        let _ = fs::remove_file(&self.next);
        let status = Command::new(self.kompiled.join(binary))
            .arg(&self.current)
            .arg(depth.to_string())
            .arg(&self.next)
            .args(extra)
            .stdin(Stdio::null())
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .status()
            .map_err(|error| format!("{binary}: {error}"))?;
        Ok(status.success())
    }

    /// A new configuration of the run, with its normal form: one step after the last one.
    pub fn step(
        &mut self,
        pattern: Pattern,
        text: String,
        normal: impl Fn(&Pattern) -> String,
    ) -> Result<(), String> {
        if self.failure.is_some() {
            return Ok(());
        }
        if let Some((last, _)) = &self.last {
            // The configurations K allows one step after the last one
            if !self.run_k("search", last, 1, &[])? {
                self.failure = Some(format!("search failed after {} steps", self.steps));
            } else {
                let src = fs::read(&self.next).map_err(|error| error.to_string())?;
                let found = parse::parse_pattern(&src).map_err(|error| {
                    format!("search output, line {}: {}", error.line, error.message)
                })?;
                let mut nexts: Vec<String> = alternatives(found).iter().map(normal).collect();
                if nexts.contains(&text) {
                    self.steps += 1;
                    nexts.sort();
                    nexts.dedup();
                    if nexts.len() > 1 {
                        self.branching += 1;
                    }
                } else {
                    self.failure = Some(format!(
                        "step {} is none of the {} steps K allows",
                        self.steps + 1,
                        nexts.len()
                    ));
                }
            }
        }
        self.last = Some((pattern, text));
        Ok(())
    }

    /// After the run, whose end tells whether it stopped (not at a step
    /// limit nor on an error): a report, and whether the check holds.
    pub fn finish(mut self, ended: bool) -> Result<(String, bool), String> {
        if self.failure.is_none()
            && ended
            && let Some((last, _)) = &self.last
        {
            // The exit code is the configuration's exit-code cell, so the step
            // count the interpreter writes tells whether it took a step
            self.run_k("interpreter", last, 1, &["--statistics"])?;
            let first = fs::read_to_string(&self.next)
                .ok()
                .and_then(|text| text.lines().next().map(str::to_owned));
            self.failure = match first.as_deref() {
                Some("0") => None,
                Some(_) => Some(format!(
                    "K takes a step from the configuration after {} steps",
                    self.steps
                )),
                None => Some("the interpreter failed on the last configuration".to_owned()),
            };
        }
        let _ = fs::remove_file(&self.current);
        let _ = fs::remove_file(&self.next);
        Ok(match &self.failure {
            None => (
                format!(
                    "steps check: each of {} steps is one K allows (K allows more than one at {}){}",
                    self.steps,
                    self.branching,
                    if ended { ", and K takes no step from the last" } else { "" }
                ),
                true,
            ),
            Some(message) => (format!("steps check FAILED: {message}"), false),
        })
    }
}
