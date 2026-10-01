use std::{path::Path, time::Instant};

use expect_test::expect_file;
use indicatif::{ProgressBar, ProgressStyle};

use p4spec_rust::lang::traits::print::Print;

use crate::{Error, Result, snapshot};

pub fn run() -> Result<()> {
    let start = Instant::now();
    let progress = ProgressBar::new(1).with_style(
        ProgressStyle::with_template("[{bar:24}] {pos}/{len} {elapsed_precise} {msg}")
            .map_err(|error| Error::Invalid(error.to_string()))?,
    );
    progress.set_message("algo: full specification");
    let spec_al = p4spec_rust::algo(["spec"]).map_err(|error| Error::Invalid(error.to_string()))?;
    let actual = Print::to_string(&spec_al) + "\n";
    let path = Path::new(env!("CARGO_MANIFEST_DIR")).join("expected/pass/algo.expected");
    snapshot::check(expect_file![path], &actual);
    progress.finish_with_message("complete");
    eprintln!(
        "algo: specification snapshot checked, elapsed={:.3}s",
        start.elapsed().as_secs_f64()
    );
    Ok(())
}
