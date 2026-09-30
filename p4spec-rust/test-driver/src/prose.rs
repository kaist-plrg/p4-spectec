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
    progress.set_message("prose: full specification");
    let spec_pl =
        p4spec_rust::prosify(["spec"]).map_err(|error| Error::Invalid(error.to_string()))?;
    let text_actual = Print::to_string(&spec_pl) + "\n";
    let path = Path::new(env!("CARGO_MANIFEST_DIR")).join("expected/pass/prose.expected");
    snapshot::check(expect_file![path], &text_actual);
    progress.finish_with_message("complete");

    eprintln!(
        "prose: specification snapshot checked, elapsed={:.3}s",
        start.elapsed().as_secs_f64()
    );
    Ok(())
}
