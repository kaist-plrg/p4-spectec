use std::{path::Path, time::Instant};

use expect_test::expect_file;
use indicatif::{ProgressBar, ProgressStyle};

use p4spec_rust::lang::traits::print::Print;

use crate::{Error, Result, snapshot};

pub fn run() -> Result<()> {
    let start = Instant::now();
    let progress = ProgressBar::new(2).with_style(
        ProgressStyle::with_template("[{bar:24}] {pos}/{len} {elapsed_precise} {msg}")
            .map_err(|error| Error::Invalid(error.to_string()))?,
    );
    progress.set_message("structure: full specification");
    let mut num_defs = 0;
    for (without_rule_groups, text_mode) in
        [(true, "without-rule-groups"), (false, "with-rule-groups")]
    {
        progress.set_message(format!("structure: {text_mode}"));
        let spec_sl = p4spec_rust::structure(&["spec".into()], without_rule_groups)
            .map_err(|error| Error::Invalid(error.to_string()))?;
        num_defs = spec_sl.len();
        let text_actual = Print::to_string(&spec_sl) + "\n";
        let path = Path::new(env!("CARGO_MANIFEST_DIR"))
            .join(format!("expected/pass/structure-{text_mode}.expected"));
        snapshot::check(expect_file![path], &text_actual);
        progress.inc(1);
    }
    progress.finish_with_message("complete");
    eprintln!(
        "structure: {num_defs} definitions checked in both modes, elapsed={:.3}s",
        start.elapsed().as_secs_f64()
    );
    Ok(())
}
