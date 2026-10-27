use std::time::Instant;

use expect_test::expect_file;

use p4spectec::lang::traits::print::Print;

use crate::{Result, snapshot, snapshot::Snapshot, util};

/// Checks registered prose snapshots.
pub fn run() -> Result<()> {
    let snapshots: Vec<Snapshot> = util::load("prose")?;
    let start = Instant::now();
    let progress = util::progress_bar(snapshots.len())?;
    for snapshot in &snapshots {
        progress.set_message(format!("prose: {}", snapshot.name));
        let spec_pl = p4spectec::prosify(&snapshot.inputs)?;
        let text_actual = Print::to_string(&spec_pl) + "\n";
        let path_expected = util::expected_path(&snapshot.expected);
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
