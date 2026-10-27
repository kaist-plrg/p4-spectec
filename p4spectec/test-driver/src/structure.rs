use std::{path::PathBuf, time::Instant};

use expect_test::expect_file;

use p4spectec::lang::traits::print::Print;

use serde::Deserialize;

use crate::{Result, snapshot, util};

/// Registers a structured snapshot with its rule-group setting.
#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct StructureSnapshot {
    pub name: String,
    pub inputs: Vec<PathBuf>,
    pub expected: PathBuf,
    #[serde(default)]
    pub without_rule_groups: bool,
}

/// Checks registered structured specification snapshots.
pub fn run() -> Result<()> {
    let snapshots: Vec<StructureSnapshot> = util::load("structure")?;
    let start = Instant::now();
    let num_snapshots = snapshots.len();
    let progress = util::progress_bar(num_snapshots)?;
    let mut num_defs = 0;
    for snapshot in &snapshots {
        progress.set_message(format!("structure: {}", snapshot.name));
        let spec_sl = p4spectec::structure(&snapshot.inputs, snapshot.without_rule_groups)?;
        num_defs = spec_sl.len();
        let text_actual = Print::to_string(&spec_sl) + "\n";
        let path_expected = util::expected_path(&snapshot.expected);
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
