//! SL instruction coverage over STF tests
//!
//! `cover_sim` runs each P4/STF pair on one simulator,
//! counting hits whether the test passes, fails, or panics,
//! as `coverage::cover_run` does for programs.

use std::path::{Path, PathBuf};

use p4spectec::lang::sl;

use p4spectec::coverage::{
    Cover, CoverError, Coverage, Hits, InstrHandler, Progress, run_catching,
};

use p4spectec::runner::{self, Config};

/// Runs each P4/STF pair on the architecture and records SL instruction coverage.
///
/// `config` must disable `tco`.
pub fn cover_sim(
    spec_sl: &sl::ast::Spec,
    arch: &str,
    config: Config,
    includes: &[PathBuf],
    pairs: &[(PathBuf, PathBuf)],
    mut on_progress: impl FnMut(Progress),
) -> Result<Coverage, CoverError> {
    let mut cover = Cover::new(spec_sl);
    let spec = runner::Spec::Sl(spec_sl.clone());
    let mut simulator = super::build(spec, arch, config)?;
    let hits = Hits::new(cover.len());
    simulator
        .hooks_mut()
        .register(Box::new(InstrHandler::new(hits.clone())));

    let mut outcomes = Vec::with_capacity(pairs.len());
    for (index, (path_p4, path_stf)) in pairs.iter().enumerate() {
        on_progress(Progress::Start(index));
        // Each STF test resets the runner and arena before it starts
        let outcome =
            run_catching(|| simulator.run_stf_test(includes, path_p4, Path::new(path_stf), |_| {}));
        cover.add_hits(&hits);
        outcomes.push(outcome);
        on_progress(Progress::Finish(index, outcome));
    }
    Ok(Coverage { cover, outcomes })
}
