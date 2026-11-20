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
//! and compares the outcomes with `expected/sim/<suite>.expected`,
//! which the languages share.

use std::{
    fs,
    path::{Path, PathBuf},
};

use p4spectec_test::harness::corpus::{self, Corpus};

const P4C_SAMPLES: &str = "p4c/testdata/p4_16_samples";
const P4TESTGEN: &str = "testdata/p4testgen";
const CUSTOM: &str = "testdata/custom";
const REGRESSION: &str = "testdata/regression/sim";

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

/// Simulates the `arch` pairs of `dir_p4` and `dir_stf` under `language`,
/// skipping those listed under `dirs_exclude`,
/// and compares the outcomes with the snapshot of `suite`.
///
/// Panics with the error text, so that its lines print unescaped.
fn sim(
    suite: &str,
    language: &str,
    arch: &str,
    dir_p4: &str,
    dir_stf: &str,
    dir_patches: Option<&str>,
    dirs_exclude: &[&str],
) {
    let result = Corpus::new(pairs(arch, dir_p4, dir_stf, dir_patches))
        .excludes(dirs_exclude)
        .and_then(|corpus| {
            let args = [&OPTIONS[..], &["sim", language, arch]].concat();
            corpus.run(WORKER, &args)
        });
    match result {
        Ok(report) => report.check(
            &Path::new(env!("CARGO_MANIFEST_DIR")).join(format!("expected/sim/{suite}.expected")),
        ),
        Err(error) => panic!("{error}"),
    }
}

#[test]
fn v1model_p4c_al() {
    sim(
        "v1model-p4c",
        "al",
        "v1model",
        P4C_SAMPLES,
        P4C_SAMPLES,
        Some("patches/v1model"),
        &[HEADERS, STATIC, DYNAMIC],
    );
}

#[test]
fn v1model_p4c_sl() {
    sim(
        "v1model-p4c",
        "sl",
        "v1model",
        P4C_SAMPLES,
        P4C_SAMPLES,
        Some("patches/v1model"),
        &[HEADERS, STATIC, DYNAMIC],
    );
}

#[test]
fn v1model_p4c_pl() {
    sim(
        "v1model-p4c",
        "pl",
        "v1model",
        P4C_SAMPLES,
        P4C_SAMPLES,
        Some("patches/v1model"),
        &[HEADERS, STATIC, DYNAMIC],
    );
}

#[test]
fn v1model_p4testgen_al() {
    sim(
        "v1model-p4testgen",
        "al",
        "v1model",
        P4C_SAMPLES,
        P4TESTGEN,
        Some("patches/v1model"),
        &[HEADERS, STATIC, DYNAMIC],
    );
}

#[test]
fn v1model_p4testgen_sl() {
    sim(
        "v1model-p4testgen",
        "sl",
        "v1model",
        P4C_SAMPLES,
        P4TESTGEN,
        Some("patches/v1model"),
        &[HEADERS, STATIC, DYNAMIC],
    );
}

#[test]
fn v1model_p4testgen_pl() {
    sim(
        "v1model-p4testgen",
        "pl",
        "v1model",
        P4C_SAMPLES,
        P4TESTGEN,
        Some("patches/v1model"),
        &[HEADERS, STATIC, DYNAMIC],
    );
}

#[test]
fn v1model_custom_al() {
    sim(
        "v1model-custom",
        "al",
        "v1model",
        CUSTOM,
        CUSTOM,
        Some("patches/v1model"),
        &[STATIC, DYNAMIC],
    );
}

#[test]
fn v1model_custom_sl() {
    sim(
        "v1model-custom",
        "sl",
        "v1model",
        CUSTOM,
        CUSTOM,
        Some("patches/v1model"),
        &[STATIC, DYNAMIC],
    );
}

#[test]
fn v1model_custom_pl() {
    sim(
        "v1model-custom",
        "pl",
        "v1model",
        CUSTOM,
        CUSTOM,
        Some("patches/v1model"),
        &[STATIC, DYNAMIC],
    );
}

#[test]
fn ebpf_p4c_al() {
    sim("ebpf-p4c", "al", "ebpf", P4C_SAMPLES, P4C_SAMPLES, None, &[HEADERS, STATIC, DYNAMIC]);
}

#[test]
fn ebpf_p4c_sl() {
    sim("ebpf-p4c", "sl", "ebpf", P4C_SAMPLES, P4C_SAMPLES, None, &[HEADERS, STATIC, DYNAMIC]);
}

#[test]
fn ebpf_p4c_pl() {
    sim("ebpf-p4c", "pl", "ebpf", P4C_SAMPLES, P4C_SAMPLES, None, &[HEADERS, STATIC, DYNAMIC]);
}

#[test]
fn ebpf_p4testgen_al() {
    sim("ebpf-p4testgen", "al", "ebpf", P4C_SAMPLES, P4TESTGEN, None, &[HEADERS, STATIC, DYNAMIC]);
}

#[test]
fn ebpf_p4testgen_sl() {
    sim("ebpf-p4testgen", "sl", "ebpf", P4C_SAMPLES, P4TESTGEN, None, &[HEADERS, STATIC, DYNAMIC]);
}

#[test]
fn ebpf_p4testgen_pl() {
    sim("ebpf-p4testgen", "pl", "ebpf", P4C_SAMPLES, P4TESTGEN, None, &[HEADERS, STATIC, DYNAMIC]);
}

#[test]
fn psa_p4c_al() {
    sim("psa-p4c", "al", "psa", P4C_SAMPLES, P4C_SAMPLES, None, &[HEADERS, STATIC, DYNAMIC]);
}

#[test]
fn psa_p4c_sl() {
    sim("psa-p4c", "sl", "psa", P4C_SAMPLES, P4C_SAMPLES, None, &[HEADERS, STATIC, DYNAMIC]);
}

#[test]
fn psa_p4c_pl() {
    sim("psa-p4c", "pl", "psa", P4C_SAMPLES, P4C_SAMPLES, None, &[HEADERS, STATIC, DYNAMIC]);
}

#[test]
fn v1model_regression_al() {
    sim("v1model-regression", "al", "v1model", REGRESSION, REGRESSION, None, &[STATIC, DYNAMIC]);
}

#[test]
fn v1model_regression_sl() {
    sim("v1model-regression", "sl", "v1model", REGRESSION, REGRESSION, None, &[STATIC, DYNAMIC]);
}

#[test]
fn v1model_regression_pl() {
    sim("v1model-regression", "pl", "v1model", REGRESSION, REGRESSION, None, &[STATIC, DYNAMIC]);
}

#[test]
fn ebpf_regression_al() {
    sim("ebpf-regression", "al", "ebpf", REGRESSION, REGRESSION, None, &[STATIC, DYNAMIC]);
}

#[test]
fn ebpf_regression_sl() {
    sim("ebpf-regression", "sl", "ebpf", REGRESSION, REGRESSION, None, &[STATIC, DYNAMIC]);
}

#[test]
fn ebpf_regression_pl() {
    sim("ebpf-regression", "pl", "ebpf", REGRESSION, REGRESSION, None, &[STATIC, DYNAMIC]);
}
