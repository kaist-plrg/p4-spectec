use std::time::Instant;

use expect_test::expect_file;
use indicatif::{ProgressBar, ProgressStyle};

use p4spec_rust::lang::traits::print::Print;

use crate::{
    Error, Result, snapshot,
    suite::{self, Registry, Stage},
};

/// Checks registered prose snapshots.
pub fn run(registry: &Registry) -> Result<()> {
    let start = Instant::now();
    let snapshots = registry.snapshots(Stage::Prose);
    if snapshots.is_empty() {
        return Err(Error::Invalid("no prose snapshots registered".into()));
    }
    let progress = ProgressBar::new(snapshots.len() as u64).with_style(
        ProgressStyle::with_template("[{bar:24}] {pos}/{len} {elapsed_precise} {msg}")
            .map_err(|error| Error::Invalid(error.to_string()))?,
    );
    for snapshot in snapshots {
        progress.set_message(format!("prose: {}", snapshot.name));
        let spec_pl = p4spec_rust::prosify(&snapshot.inputs)
            .map_err(|error| Error::Invalid(error.to_string()))?;
        let text_actual = Print::to_string(&spec_pl) + "\n";
        let path_expected = suite::expected_path(&snapshot.expected);
        snapshot::check(expect_file![path_expected], &text_actual);
        progress.inc(1);
    }
    progress.finish_with_message("complete");

    eprintln!(
        "prose: specification snapshot checked, elapsed={:.3}s",
        start.elapsed().as_secs_f64()
    );
    Ok(())
}
