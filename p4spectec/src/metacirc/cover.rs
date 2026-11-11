//! SL instruction coverage of a tower's base
//!
//! `cover_run` evaluates P4 programs or scripts through a tower with an SL base,
//! and `cover_sim` runs STF tests through one below a P4 top level,
//! as `coverage` does on one runner.
//! Only the base is measured, at depth 0 and every reentry depth;
//! the levels it interprets are not.

use super::{
    BaseConfig, Repr, Result, TowerConfig,
    config::Lang,
    error::config_invalid,
    sim::build_observed_simulator,
    tower::{Observer, Tower},
};
use crate::{
    coverage::{Cover, Coverage, Hits, InstrHandler, Progress, run_catching},
    lang::{
        data::{arena::Arena, encoding::Encoding},
        sl,
    },
    runner,
    sim_plugin::dummy::Dummy,
};
use std::path::{Path, PathBuf};

/// Returns the base specification a tower's coverage measures,
/// checking that the base runs on SL.
pub fn base_specdir(config: &TowerConfig) -> Result<&Path> {
    if !matches!(config.base, BaseConfig::St { repr: Repr::Sl }) {
        return Err(config_invalid("coverage requires an sl tower"));
    }
    Ok(&config.levels[0].specdir)
}

/// Returns the file name suffix of the programs `cover_run` evaluates through a tower,
/// checking that its top level reads P4 programs or scripts.
pub fn program_suffix(config: &TowerConfig) -> Result<&'static str> {
    match config.top_lang() {
        Lang::P4 => Ok(".p4"),
        Lang::Al | Lang::AlExt | Lang::IlExt => Ok(".watsup"),
        Lang::Kore => Err(config_invalid("cover-run requires a P4 or script top level")),
    }
}

/// Makes handlers recording into `hits`.
fn observer(hits: &Hits) -> Observer {
    let hits = hits.clone();
    Box::new(move || Box::new(InstrHandler::new(hits.clone())))
}

/// Evaluates each program through the tower and records the base's SL instruction coverage.
///
/// `spec_sl` must be the base specification structured without rule groups,
/// and `options` must disable `tco`.
pub fn cover_run(
    config: &TowerConfig,
    spec_sl: &sl::ast::Spec,
    options: runner::Config,
    includes: &[PathBuf],
    paths_program: &[PathBuf],
    mut on_progress: impl FnMut(Progress),
) -> Result<Coverage> {
    base_specdir(config)?;
    program_suffix(config)?;
    let mut cover = Cover::new(spec_sl);
    let mut arena = Arena::new();
    let mut tower = Tower::build(&mut arena, config, options, Dummy)?;
    let hits = Hits::new(cover.len());
    tower.observe_base(observer(&hits))?;

    let mut outcomes = Vec::with_capacity(paths_program.len());
    for (index, path) in paths_program.iter().enumerate() {
        on_progress(Progress::Start(index));
        let outcome = run_catching(|| {
            let paths = std::slice::from_ref(path);
            let input = tower
                .parse_input(&mut arena, includes, paths, None)
                .map_err(drop)?;
            let values = tower.encode_input(&mut arena, &input).map_err(drop)?;
            tower.eval_input(&mut arena, &values).map_err(drop)
        });
        // Count hits up to the end or the panic, then start the next program afresh
        cover.add_hits(&hits);
        tower.reset();
        arena.reset_values();
        outcomes.push(outcome);
        on_progress(Progress::Finish(index, outcome));
    }
    Ok(Coverage { cover, outcomes })
}

/// Runs each P4/STF pair through the tower on the architecture
/// and records the base's SL instruction coverage.
///
/// `spec_sl` must be the base specification structured without rule groups,
/// and `options` must disable `tco`.
#[expect(clippy::too_many_arguments, reason = "mirrors coverage::cover_sim with the tower")]
pub fn cover_sim(
    config: &TowerConfig,
    spec_sl: &sl::ast::Spec,
    arch: &str,
    options: runner::Config,
    encoding: Encoding,
    includes: &[PathBuf],
    pairs: &[(PathBuf, PathBuf)],
    mut on_progress: impl FnMut(Progress),
) -> Result<Coverage> {
    base_specdir(config)?;
    let mut cover = Cover::new(spec_sl);
    let hits = Hits::new(cover.len());
    let mut simulator =
        build_observed_simulator(config, arch, options, encoding, vec![observer(&hits)])?;

    let mut outcomes = Vec::with_capacity(pairs.len());
    for (index, (path_p4, path_stf)) in pairs.iter().enumerate() {
        on_progress(Progress::Start(index));
        // Each STF test resets the tower and arena before it starts
        let outcome = run_catching(|| simulator.run_stf_test(includes, path_p4, path_stf, |_| {}));
        cover.add_hits(&hits);
        outcomes.push(outcome);
        on_progress(Progress::Finish(index, outcome));
    }
    Ok(Coverage { cover, outcomes })
}
