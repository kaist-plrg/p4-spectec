use std::time::Instant;

use expect_test::expect_file;
use indicatif::{ProgressBar, ProgressStyle};

use p4spec_rust::lang::traits::print::Print;

use crate::{
    Error, Result, snapshot,
    suite::{self, StructureSnapshot},
};

/// Checks registered structured specification snapshots.
pub fn run(snapshots: &[StructureSnapshot]) -> Result<()> {
    let start = Instant::now();
    if snapshots.is_empty() {
        return Err(Error::Invalid("no structure snapshots registered".into()));
    }
    let num_snapshots = snapshots.len();
    let progress = ProgressBar::new(num_snapshots as u64).with_style(
        ProgressStyle::with_template("[{bar:24}] {pos}/{len} {elapsed_precise} {msg}")
            .map_err(|error| Error::Invalid(error.to_string()))?,
    );
    let mut num_defs = 0;
    for snapshot in snapshots {
        progress.set_message(format!("structure: {}", snapshot.name));
        let spec_sl = p4spec_rust::structure(&snapshot.inputs, snapshot.without_rule_groups)
            .map_err(|error| Error::Invalid(error.to_string()))?;
        num_defs = spec_sl.len();
        let text_actual = Print::to_string(&spec_sl) + "\n";
        let path_expected = suite::expected_path(&snapshot.expected);
        snapshot::check(expect_file![path_expected], &text_actual);
        progress.inc(1);
    }
    progress.finish_with_message("complete");
    eprintln!(
        "structure: {num_defs} definitions checked in {num_snapshots} snapshots, elapsed={:.3}s",
        start.elapsed().as_secs_f64()
    );
    Ok(())
}
