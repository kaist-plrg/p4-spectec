//! What krun gives for a program, for `KRUN_EXPECT=1`
//!
//! `Case::krun` runs the kompiled interpreter as krun does, with `--statistics`,
//! and writes the run as `print_runres` prints one; `Case::search` writes the
//! final configurations K allows, as the kompiled `search` binary finds them.

use std::{
    fs,
    path::Path,
    process::{Command, Stdio},
};

use super::build::{Case, run, stdin};
use crate::harness::Result;

/// Exit codes of the interpreter for a run that fails: a stuck configuration,
/// and an uncaught exception, which aborts.
const EXIT_FAILED: [i32; 2] = [113, 134];

/// How long the search binary may take, in seconds: where many orders of the
/// same rules reach many configurations, as threads do, it may not end.
const SEARCH_SECONDS: u32 = 600;

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

    /// Writes the final configurations K allows from an initial term to `path_search`,
    /// as the kompiled `search` binary finds them within `SEARCH_SECONDS`; a search
    /// that does not end then fails, and writes nothing.
    pub fn search(
        &self,
        path_init: &Path,
        depth: Option<u64>,
        input: Option<&Path>,
        path_search: &Path,
    ) -> Result<()> {
        if path_search.exists() {
            // The search binary appends to its output file
            fs::remove_file(path_search)?;
        }
        let searched = run(
            Command::new("timeout")
                .arg(SEARCH_SECONDS.to_string())
                .arg(self.kompiled().join("search"))
                .arg(path_init)
                .arg(arg_depth(depth))
                .arg(path_search),
            input,
            &format!("search {}", path_init.display()),
        );
        if searched.is_err() && path_search.exists() {
            fs::remove_file(path_search)?;
        }
        searched.map(|_| ())
    }
}
