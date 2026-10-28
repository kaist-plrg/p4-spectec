//! What krun gives for a program, for `UPDATE_EXPECT=1`
//!
//! `Case::krun` runs the kompiled interpreter as krun does, with `--statistics`,
//! and writes the run as `print_runres` prints one; `Case::search` writes the
//! final configurations K allows, as the kompiled `search` binary finds them.

use std::{
    fs,
    path::{Path, PathBuf},
    process::{Command, Stdio},
};

use super::{
    build::{Case, run, stdin},
    dir_build,
};
use crate::harness::{Error, Result};

/// Exit codes of the interpreter for a run that fails: a stuck configuration,
/// and an uncaught exception, which aborts.
const EXIT_FAILED: [i32; 2] = [113, 134];

/// `fastthrow.c` compiled once with `cc`; none if it cannot be, and the interpreter
/// then takes its time to abort.
fn fastthrow() -> Option<PathBuf> {
    let dir = dir_build();
    let path_library = dir.join("fastthrow.so");
    if path_library.exists() {
        return Some(path_library);
    }
    let path_source = dir.join("fastthrow.c");
    fs::create_dir_all(&dir).ok()?;
    fs::write(&path_source, include_str!("fastthrow.c")).ok()?;
    let status = Command::new("cc")
        .args(["-shared", "-fPIC", "-O2", "-o"])
        .arg(&path_library)
        .arg(&path_source)
        .status()
        .ok()?;
    status.success().then_some(path_library)
}

/// A step bound as the interpreter and the search binary take it: -1 for none.
fn arg_depth(depth: Option<u64>) -> String {
    depth.map_or_else(|| "-1".to_owned(), |depth| depth.to_string())
}

impl Case<'_> {
    /// What krun's run of an initial term gives, as `print_runres` prints a run:
    /// for a run that fails, the configuration before the failing step,
    /// found by running again with fewer steps.
    pub fn krun(
        &self,
        path_init: &Path,
        depth: Option<u64>,
        input: Option<&Path>,
    ) -> Result<String> {
        if let Some(output) = self.interpret(path_init, depth, input)? {
            return Ok(output);
        }
        if self.interpret(path_init, Some(0), input)?.is_none() {
            return Ok("error on the initial term".to_owned());
        }
        // Step ok + 1 succeeds before step bad fails; bisect between them
        let (mut ok, mut bad) = (0, 1);
        while self.interpret(path_init, Some(bad), input)?.is_some() {
            (ok, bad) = (bad, bad * 2);
        }
        while bad - ok > 1 {
            let mid = (ok + bad) / 2;
            if self.interpret(path_init, Some(mid), input)?.is_some() {
                ok = mid;
            } else {
                bad = mid;
            }
        }
        let output = self
            .interpret(path_init, Some(ok), input)?
            .ok_or_else(|| Error::from("the interpreter failed where it succeeded before"))?;
        let config = output.split_once('\n').map_or("", |(_, config)| config);
        Ok(format!("error after {ok} steps\n{config}"))
    }

    /// The interpreter's output after at most `depth` steps (all if none):
    /// the steps and the final configuration; none if the run fails.
    fn interpret(
        &self,
        path_init: &Path,
        depth: Option<u64>,
        input: Option<&Path>,
    ) -> Result<Option<String>> {
        let dir_temp = tempfile::tempdir()?;
        let path_out = dir_temp.path().join("result.kore");
        let mut command = Command::new(self.kompiled().join("interpreter"));
        if let Some(path_fastthrow) = fastthrow() {
            command.env("LD_PRELOAD", path_fastthrow);
        }
        let status = command
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
            return Ok(None);
        }
        Ok(Some(fs::read_to_string(path_out)?.trim_end().to_owned()))
    }

    /// Writes the final configurations K allows from an initial term to `path_search`,
    /// as the kompiled `search` binary finds them.
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
        run(
            Command::new(self.kompiled().join("search"))
                .arg(path_init)
                .arg(arg_depth(depth))
                .arg(path_search),
            input,
            &format!("search {}", path_init.display()),
        )?;
        Ok(())
    }
}
