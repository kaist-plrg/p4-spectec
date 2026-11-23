//! Fixture directories under `tests/p4spectec/` and their snapshots
//!
//! `fixtures` enters `tests/p4spectec/` and lists a fixture directory,
//! so that reports show paths such as `elab/neg/foo.watsup`.
//! `check` renders a fixture's reports and compares them with the snapshot beside it;
//! a fixture with no reports and no snapshot passes,
//! so a passing fixture keeps a snapshot only when it warns.

use std::{
    fmt::Display,
    path::{Path, PathBuf},
};

use p4spectec::diagnostic::{DisplayStyle, RenderConfig, Renderer, Report};

use super::{collect, root};

/// Enters `tests/p4spectec/` and lists the files of `dir` ending in one of `suffixes`.
///
/// Paths are relative to `tests/p4spectec/`.
pub fn fixtures(dir: &str, suffixes: &[&str]) -> Vec<PathBuf> {
    std::env::set_current_dir(root().join("tests/p4spectec")).expect("enter tests/p4spectec");
    let mut paths: Vec<PathBuf> = suffixes
        .iter()
        .flat_map(|suffix| collect(Path::new(dir), suffix).expect("fixture directory"))
        .collect();
    assert!(!paths.is_empty(), "no fixtures in {dir}");
    paths.sort();
    paths
}

/// Returns the result of a stage that must pass for `path`.
pub fn must_pass<T>(path: &Path, stage: &str, result: Result<T, impl Display>) -> T {
    result.unwrap_or_else(|error| panic!("{}: {stage} failed: {error}", path.display()))
}

/// Renders `reports` under `config` and compares the text with `path_expected`.
///
/// No reports and no snapshot pass without comparing.
pub fn check(path_expected: &Path, reports: &[Report], config: &RenderConfig) {
    if reports.is_empty() && !path_expected.exists() {
        return;
    }
    let mut text = String::new();
    for report in reports {
        let rendered = Renderer::new(config.clone())
            .render_to_string(report)
            .expect("render report");
        text.push_str(&rendered);
    }
    let name = path_expected.file_stem().unwrap().to_str().unwrap();
    insta::with_settings!({
        snapshot_path => std::env::current_dir().unwrap().join(path_expected.parent().unwrap()),
        prepend_module_to_snapshot => false,
        omit_expression => true,
    }, {
        insta::assert_snapshot!(name, text);
    });
}

/// Renders runtime frames in the short style.
pub fn frames_short() -> RenderConfig {
    RenderConfig { frame_style: Some(DisplayStyle::Short), ..Default::default() }
}
