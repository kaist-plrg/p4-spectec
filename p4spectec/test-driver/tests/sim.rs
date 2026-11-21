//! STF simulation corpora under every interpreter language
//!
//! Each test pairs the P4 programs of one architecture with their STF tests:
//! a program under `dir_p4` that includes the architecture's model
//! pairs with each test under `dir_stf` of its stem,
//! either beside it or in a directory named after it,
//! and files under `dir_patches` replace those with the same stem.
//! Pairs naming a file listed under the test's exclusion directories are skipped.
//! Each test runs its pairs in one `p4spectec-test-worker`,
//! which gives each pair the timeout of `P4SPECTEC_TEST_TIMEOUT`, if set,
//! and checks that every pair not excluded passes.

use std::{
    fs,
    path::{Path, PathBuf},
};

use p4spectec_test::harness::{
    corpus::{self, Corpus, Outcome},
    interpreter::Language,
};

const P4C_SAMPLES: &str = "p4c/testdata/p4_16_samples";
const P4TESTGEN: &str = "testdata/p4testgen";
const CUSTOM: &str = "testdata/custom";
const REGRESSION: &str = "testdata/regression/sim";
const PATCHES_V1MODEL: &str = "patches/v1model";

const HEADERS: &str = "excludes/headers";
const STATIC: &str = "excludes/static";
const DYNAMIC: &str = "excludes/dynamic";

/// Worker executable, which Cargo builds before the tests.
const WORKER: &str = env!("CARGO_BIN_EXE_p4spectec-test-worker");

/// Worker options: specification, P4 includes, and the runner options enabled.
const OPTIONS: [&str; 7] =
    ["--spec", "spec", "--include", "p4c/p4include", "--cache", "--det", "--tco"];

/// A source file found under `dir`, at `path` relative to it.
struct Input {
    dir: PathBuf,
    path: PathBuf,
}

/// Collects the files ending in `suffix` under `dir`, relative to the repository root.
fn collect(dir: &str, suffix: &str) -> Vec<Input> {
    let dir_root = corpus::root().join(dir);
    corpus::collect(&dir_root, suffix)
        .unwrap_or_else(|error| panic!("{dir}: {error}"))
        .into_iter()
        .map(|path| {
            let path = path
                .strip_prefix(&dir_root)
                .expect("file under its directory");
            Input { dir: PathBuf::from(dir), path: path.to_owned() }
        })
        .collect()
}

/// Replaces each input by the patch of the same stem, if any.
fn patch(inputs: &mut [Input], patches: &[Input]) {
    for input in inputs {
        if let Some(patch) = patches
            .iter()
            .find(|patch| patch.path.file_stem() == input.path.file_stem())
        {
            input.dir.clone_from(&patch.dir);
            input.path.clone_from(&patch.path);
        }
    }
}

/// Pairs the `arch` programs of `dir_p4` with the STF tests of `dir_stf`.
fn pairs(arch: &str, dir_p4: &str, dir_stf: &str, dir_patches: Option<&str>) -> Vec<Vec<PathBuf>> {
    let include = match arch {
        "v1model" => "v1model.p4",
        "ebpf" => "ebpf_model.p4",
        "psa" => "bmv2/psa.p4",
        _ => panic!("unknown architecture {arch}"),
    };
    // Keep the programs that include the architecture's model
    let root = corpus::root();
    let mut inputs_p4 = Vec::new();
    for input in collect(dir_p4, ".p4") {
        let text = fs::read_to_string(root.join(&input.dir).join(&input.path)).expect("P4 program");
        if text.contains(&format!("#include <{include}>"))
            || text.contains(&format!("#include \"{include}\""))
        {
            inputs_p4.push(input);
        }
    }
    let mut inputs_stf = collect(dir_stf, ".stf");
    if let Some(dir) = dir_patches {
        patch(&mut inputs_p4, &collect(dir, ".p4"));
        patch(&mut inputs_stf, &collect(dir, ".stf"));
    }
    // Pair programs and tests by stem
    let mut pairs = Vec::new();
    for input_p4 in &inputs_p4 {
        for input_stf in &inputs_stf {
            let stem_p4 = input_p4.path.file_stem();
            let dir_stf = input_stf.path.parent().unwrap_or(Path::new(""));
            if stem_p4 == Some(dir_stf.as_os_str())
                || (input_p4.path.parent() == input_stf.path.parent()
                    && stem_p4 == input_stf.path.file_stem())
            {
                pairs.push(vec![
                    input_p4.dir.join(&input_p4.path),
                    input_stf.dir.join(&input_stf.path),
                ]);
            }
        }
    }
    pairs
}

/// One simulation corpus: the `arch` programs of `dir_p4` paired with the STF tests of `dir_stf`.
struct Suite {
    /// Names the run log `sim-<name>-<language>.log`.
    name: &'static str,
    /// Names the architecture, whose model the programs include.
    arch: &'static str,
    /// Holds the P4 programs, relative to the repository root.
    dir_p4: &'static str,
    /// Holds the STF tests, relative to the repository root.
    dir_stf: &'static str,
    /// Holds the files replacing programs and tests of the same stem, if any.
    dir_patches: Option<&'static str>,
    /// Holds the `.exclude` files of the pairs to skip.
    dirs_exclude: &'static [&'static str],
}

/// Simulates the pairs of `suite` under `language`
/// and checks that each passes.
///
/// Panics with the error text, so that its lines print unescaped.
fn sim(suite: &Suite, language: Language) {
    let result = Corpus::new(pairs(suite.arch, suite.dir_p4, suite.dir_stf, suite.dir_patches))
        .excludes(suite.dirs_exclude)
        .and_then(|corpus| {
            let args = [&OPTIONS[..], &["sim", language.name(), suite.arch]].concat();
            corpus.run(WORKER, &args, &format!("sim-{}-{}", suite.name, language.name()))
        });
    match result {
        Ok(report) => report.check(Outcome::Pass),
        Err(error) => panic!("{error}"),
    }
}

const V1MODEL_P4C: Suite = Suite {
    name: "v1model-p4c",
    arch: "v1model",
    dir_p4: P4C_SAMPLES,
    dir_stf: P4C_SAMPLES,
    dir_patches: Some(PATCHES_V1MODEL),
    dirs_exclude: &[HEADERS, STATIC, DYNAMIC],
};

#[test]
fn v1model_p4c_al() {
    sim(&V1MODEL_P4C, Language::Al);
}

#[test]
fn v1model_p4c_sl() {
    sim(&V1MODEL_P4C, Language::Sl);
}

#[test]
fn v1model_p4c_pl() {
    sim(&V1MODEL_P4C, Language::Pl);
}

const V1MODEL_P4TESTGEN: Suite = Suite {
    name: "v1model-p4testgen",
    arch: "v1model",
    dir_p4: P4C_SAMPLES,
    dir_stf: P4TESTGEN,
    dir_patches: Some(PATCHES_V1MODEL),
    dirs_exclude: &[HEADERS, STATIC, DYNAMIC],
};

#[test]
fn v1model_p4testgen_al() {
    sim(&V1MODEL_P4TESTGEN, Language::Al);
}

#[test]
fn v1model_p4testgen_sl() {
    sim(&V1MODEL_P4TESTGEN, Language::Sl);
}

#[test]
fn v1model_p4testgen_pl() {
    sim(&V1MODEL_P4TESTGEN, Language::Pl);
}

const V1MODEL_CUSTOM: Suite = Suite {
    name: "v1model-custom",
    arch: "v1model",
    dir_p4: CUSTOM,
    dir_stf: CUSTOM,
    dir_patches: Some(PATCHES_V1MODEL),
    dirs_exclude: &[STATIC, DYNAMIC],
};

#[test]
fn v1model_custom_al() {
    sim(&V1MODEL_CUSTOM, Language::Al);
}

#[test]
fn v1model_custom_sl() {
    sim(&V1MODEL_CUSTOM, Language::Sl);
}

#[test]
fn v1model_custom_pl() {
    sim(&V1MODEL_CUSTOM, Language::Pl);
}

const EBPF_P4C: Suite = Suite {
    name: "ebpf-p4c",
    arch: "ebpf",
    dir_p4: P4C_SAMPLES,
    dir_stf: P4C_SAMPLES,
    dir_patches: None,
    dirs_exclude: &[HEADERS, STATIC, DYNAMIC],
};

#[test]
fn ebpf_p4c_al() {
    sim(&EBPF_P4C, Language::Al);
}

#[test]
fn ebpf_p4c_sl() {
    sim(&EBPF_P4C, Language::Sl);
}

#[test]
fn ebpf_p4c_pl() {
    sim(&EBPF_P4C, Language::Pl);
}

const EBPF_P4TESTGEN: Suite = Suite {
    name: "ebpf-p4testgen",
    arch: "ebpf",
    dir_p4: P4C_SAMPLES,
    dir_stf: P4TESTGEN,
    dir_patches: None,
    dirs_exclude: &[HEADERS, STATIC, DYNAMIC],
};

#[test]
fn ebpf_p4testgen_al() {
    sim(&EBPF_P4TESTGEN, Language::Al);
}

#[test]
fn ebpf_p4testgen_sl() {
    sim(&EBPF_P4TESTGEN, Language::Sl);
}

#[test]
fn ebpf_p4testgen_pl() {
    sim(&EBPF_P4TESTGEN, Language::Pl);
}

const PSA_P4C: Suite = Suite {
    name: "psa-p4c",
    arch: "psa",
    dir_p4: P4C_SAMPLES,
    dir_stf: P4C_SAMPLES,
    dir_patches: None,
    dirs_exclude: &[HEADERS, STATIC, DYNAMIC],
};

#[test]
fn psa_p4c_al() {
    sim(&PSA_P4C, Language::Al);
}

#[test]
fn psa_p4c_sl() {
    sim(&PSA_P4C, Language::Sl);
}

#[test]
fn psa_p4c_pl() {
    sim(&PSA_P4C, Language::Pl);
}

const V1MODEL_REGRESSION: Suite = Suite {
    name: "v1model-regression",
    arch: "v1model",
    dir_p4: REGRESSION,
    dir_stf: REGRESSION,
    dir_patches: None,
    dirs_exclude: &[STATIC, DYNAMIC],
};

#[test]
fn v1model_regression_al() {
    sim(&V1MODEL_REGRESSION, Language::Al);
}

#[test]
fn v1model_regression_sl() {
    sim(&V1MODEL_REGRESSION, Language::Sl);
}

#[test]
fn v1model_regression_pl() {
    sim(&V1MODEL_REGRESSION, Language::Pl);
}

const EBPF_REGRESSION: Suite = Suite {
    name: "ebpf-regression",
    arch: "ebpf",
    dir_p4: REGRESSION,
    dir_stf: REGRESSION,
    dir_patches: None,
    dirs_exclude: &[STATIC, DYNAMIC],
};

#[test]
fn ebpf_regression_al() {
    sim(&EBPF_REGRESSION, Language::Al);
}

#[test]
fn ebpf_regression_sl() {
    sim(&EBPF_REGRESSION, Language::Sl);
}

#[test]
fn ebpf_regression_pl() {
    sim(&EBPF_REGRESSION, Language::Pl);
}
