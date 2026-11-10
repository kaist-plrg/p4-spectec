//! What krun gives for a program, for `KRUN_EXPECT=1`
//!
//! `Case::krun` runs the kompiled interpreter as krun does, with `--statistics`,
//! and writes the run as `print_runres` prints one; `Case::search` gives the
//! final configurations K allows, as the kompiled `search` binary writes them.

use std::{
    fs,
    path::Path,
    process::{Command, Stdio},
    time::Duration,
};

use super::build::{Case, failed, stdin};
use crate::harness::Result;

/// Exit codes of the interpreter for a run that fails: a stuck configuration,
/// and an uncaught exception, which aborts.
const EXIT_FAILED: [i32; 2] = [113, 134];

/// A step bound as the interpreter and the search binary take it: -1 for none.
fn arg_depth(depth: Option<u64>) -> String {
    depth.map_or_else(|| "-1".to_owned(), |depth| depth.to_string())
}

impl Case<'_> {
    /// What krun's run of an initial term gives, as `print_runres` prints a run:
    /// the steps and the final configuration, or `error` for a run that fails.
    pub fn krun(
        &self,
        path_init: &Path,
        depth: Option<u64>,
        input: Option<&Path>,
    ) -> Result<String> {
        let dir_temp = tempfile::tempdir()?;
        let path_out = dir_temp.path().join("result.kore");
        let status = Command::new(self.kompiled().join("interpreter"))
            .arg(path_init)
            .arg(arg_depth(depth))
            .arg(&path_out)
            .arg("--statistics")
            .stdin(stdin(input)?)
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .status()?;
        // A signal (no code) or a failing code is a failed run; any other code is the exit cell's
        let failed = status.code().is_none_or(|code| EXIT_FAILED.contains(&code));
        if failed || !path_out.exists() {
            return Ok("error".to_owned());
        }
        Ok(fs::read_to_string(path_out)?.trim_end().to_owned())
    }

    /// The final configurations K's search from an initial term reaches within
    /// `timeout`, if it ends, as the kompiled `search` binary writes them: one, or
    /// several under `\or`.
    pub fn search(
        &self,
        path_init: &Path,
        depth: Option<u64>,
        input: Option<&Path>,
        timeout: Option<Duration>,
    ) -> Result<Option<String>> {
        let dir_temp = tempfile::tempdir()?;
        let path_out = dir_temp.path().join("search.kore");
        let search = self.kompiled().join("search");
        let mut command = match timeout {
            Some(timeout) => {
                let mut command = Command::new("timeout");
                command.arg(timeout.as_secs().to_string()).arg(search);
                command
            }
            None => Command::new(search),
        };
        let output = command
            .arg(path_init)
            .arg(arg_depth(depth))
            .arg(&path_out)
            .stdin(stdin(input)?)
            .output()?;
        if output.status.success() {
            return Ok(Some(fs::read_to_string(path_out)?));
        }
        // `timeout` exits with 124 when the time is up
        if timeout.is_some() && output.status.code() == Some(124) {
            return Ok(None);
        }
        Err(failed(&format!("search {}", path_init.display()), &output))
    }
}
