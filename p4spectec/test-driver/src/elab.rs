use std::time::Instant;

use expect_test::expect_file;

use p4spectec::lang::traits::print::Print;

use crate::{Result, snapshot, snapshot::Snapshot, util};

/// Checks registered elab snapshots.
pub fn run() -> Result<()> {
    let snapshots: Vec<Snapshot> = util::load("elab")?;
    let start = Instant::now();
    let progress = util::progress_bar(snapshots.len())?;
    for snapshot in &snapshots {
        progress.set_message(format!("elab: {}", snapshot.name));
        let spec_il = p4spectec::elab(&snapshot.inputs)?;
        let actual = Print::to_string(&spec_il) + "\n";
        let path_expected = util::expected_path(&snapshot.expected);
        snapshot::check(expect_file![path_expected], &actual);
        progress.inc(1);
    }
    progress.finish_with_message("complete");
    eprintln!(
        "elab: specification snapshot checked, elapsed={:.3}s",
        start.elapsed().as_secs_f64()
    );
    Ok(())
}
