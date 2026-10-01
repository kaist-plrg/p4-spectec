//! Negative splice inputs rendered as diagnostic snapshots
//!
//! `.adoc` inputs exercise located marker failures through `splice_strings`.
//! File cases call `splice_files` in isolated directories with relative paths.
//! Every case requires rejection before the shared runner snapshots its report.

use std::{env, fs, path::PathBuf};

use p4spec_rust::diagnostic::Report;

use p4spec_rust::backend_specdoc::splicer;

use crate::Result;

use super::failure;

// == Rejection checks

/// Requires a product failure carrying a structured diagnostic.
fn rejected<T>(name: &str, result: std::result::Result<T, splicer::Error>) -> Result<Vec<Report>> {
    match result {
        Ok(_) => Err(failure(name, "splicer unexpectedly accepted negative input")),
        Err(report) => Ok(vec![*report]),
    }
}

// == Filesystem cases

/// Restores the fixture directory and removes temporary files on every exit.
struct Directory {
    path: PathBuf,
    path_previous: PathBuf,
}

impl Directory {
    /// Enters an exclusively created directory for one filesystem failure.
    fn enter(name: &str) -> Result<Self> {
        let path_previous = env::current_dir()?;
        let path = env::temp_dir().join(format!("p4spec-splice-{}-{name}", std::process::id()));
        fs::create_dir(&path)?;
        let directory = Self { path, path_previous };
        env::set_current_dir(&directory.path)?;
        Ok(directory)
    }
}

impl Drop for Directory {
    fn drop(&mut self) {
        let _ = env::set_current_dir(&self.path_previous);
        let _ = fs::remove_dir_all(&self.path);
    }
}

/// Runs the splice API and returns its rejection for snapshot rendering.
pub fn run(name: &str) -> Result<Vec<Report>> {
    // Skeleton paths also identify the on-disk source used by the report renderer
    if name.ends_with(".adoc") {
        let path = format!("splice/{name}");
        let text = fs::read_to_string(&path)?;
        return rejected(
            name,
            splicer::splice_strings(&Vec::new(), &Vec::new(), &[(&path, &text)]),
        );
    }
    // Relative paths prevent temporary directory names from entering snapshots
    let _directory = Directory::enter(name)?;
    match name {
        // No input file is created, so reading must fail before rendering
        "input-missing" => rejected(
            name,
            splicer::splice_files(
                &Vec::new(),
                &Vec::new(),
                &[("missing.adoc".into(), "out.adoc".into())],
            ),
        ),
        // A regular file cannot serve as the output's parent directory
        "output-parent-file" => {
            fs::write("input.adoc", "literal text")?;
            fs::write("blocked", "regular file")?;
            rejected(
                name,
                splicer::splice_files(
                    &Vec::new(),
                    &Vec::new(),
                    &[("input.adoc".into(), "blocked/out.adoc".into())],
                ),
            )
        }
        // Reject missing harness cases rather than silently omitting coverage
        _ => Err(failure(name, "unknown constructed splice case")),
    }
}
