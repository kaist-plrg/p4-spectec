//! STF simulation corpora under every interpreter language
//!
//! Each test pairs the P4 programs of one architecture with their STF tests:
//! a program under `dir_p4` that includes the architecture's model
//! pairs with each test under `dir_stf` of its stem,
//! either beside it or in a directory named after it,
//! and files under `dirs_patch` replace those with the same stem.
//! Pairs naming a file listed under the test's exclusion directories are skipped.
//! Each corpus test prepares the simulator once, runs its pairs from the repository root,
//! and checks that every pair not excluded passes.
//! Each fixture test instead simulates each program under `tests/p4spectec/sim` with the STF test of its stem,
//! which must fail, and compares the error with the snapshot `<stem>.snap`.

use std::{
    fs,
    path::{Path, PathBuf},
};

use p4spectec::{SpecLang, runner::Config};
use p4spectec_test::harness::{
    self, Outcome, Result,
    corpus::{Corpus, Report},
    fixture::{check, fixtures, frames_short, must_pass},
};

const P4C_SAMPLES: &str = "p4c/testdata/p4_16_samples";
const P4TESTGEN: &str = "testdata/p4testgen";
const CUSTOM: &str = "testdata/custom";
const REGRESSION: &str = "testdata/regression/sim";
const PATCHES_V1MODEL: &str = "patches/v1model";

const HEADERS: &str = "excludes/headers";
const STATIC: &str = "excludes/static";
const DYNAMIC: &str = "excludes/dynamic";

/// Runner options of every corpus.
const CONFIG: Config = Config { cache: true, det: true, guard: false, tco: true };

// = Helpers

/// A source file found under `dir`, at `path` relative to it.
struct Input {
    dir: PathBuf,
    path: PathBuf,
}

/// Collects the files ending in `suffix` under `dir`, relative to the repository root.
fn collect(dir: &str, suffix: &str) -> Vec<Input> {
    let dir_root = harness::root().join(dir);
    harness::collect(&dir_root, suffix)
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
fn pairs(arch: &str, dir_p4: &str, dir_stf: &str, dirs_patch: &[&str]) -> Vec<Vec<PathBuf>> {
    let include = match arch {
        "v1model" => "v1model.p4",
        "ebpf" => "ebpf_model.p4",
        "psa" => "bmv2/psa.p4",
        _ => panic!("unknown architecture {arch}"),
    };
    // Keep the programs that include the architecture's model
    let root = harness::root();
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
    for dir in dirs_patch {
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

// = Suites

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
    /// Holds the files replacing programs and tests of the same stem.
    dirs_patch: &'static [&'static str],
    /// Holds the `.exclude` files of the pairs to skip.
    dirs_exclude: &'static [&'static str],
}

impl Suite {
    /// Simulates the pairs of the suite under `language`
    /// and checks that each passes.
    ///
    /// Panics with the error text, so that its lines print unescaped.
    fn sim(&self, language: SpecLang) {
        match self.report(language) {
            Ok(report) => report.check(Outcome::Pass),
            Err(error) => panic!("{error}"),
        }
    }

    /// Simulates the pairs of the suite under `language` from the repository root.
    ///
    /// A failing pair's details are its simulation error.
    fn report(&self, language: SpecLang) -> Result<Report> {
        std::env::set_current_dir(harness::root())?;
        let corpus = Corpus::new(pairs(self.arch, self.dir_p4, self.dir_stf, self.dirs_patch))
            .excludes(self.dirs_exclude)?;
        let spec = p4spectec::runner_spec_with_warnings(language, &[PathBuf::from("spec")]).0?;
        let mut simulator = p4spectec_sim::build(spec, self.arch, CONFIG)?;
        let paths_include = [PathBuf::from("p4c/p4include")];
        corpus.run(
            &format!("sim-{}-{}", self.name, format!("{language:?}").to_lowercase()),
            |paths| match simulator.run_stf_test(&paths_include, &paths[0], &paths[1], |_| {}) {
                Ok(()) => Ok((Outcome::Pass, String::new())),
                Err(error) => Ok((Outcome::Fail, error.to_string())),
            },
        )
    }
}

/// One directory of programs under `tests/p4spectec/`, each with the STF test of its stem.
struct Fixtures {
    /// Holds the programs and tests, relative to `tests/p4spectec/`.
    dir: &'static str,
    /// Names the architecture the programs are simulated on.
    arch: &'static str,
}

impl Fixtures {
    /// Simulates each program with its test under `language`, which must fail,
    /// and compares the error with the program's snapshot.
    fn check(&self, language: SpecLang) {
        let paths = fixtures(self.dir, &[".p4"]);
        let spec = must_pass(
            Path::new(self.dir),
            "conversion",
            p4spectec::runner_spec_with_warnings(language, &[PathBuf::from("../../spec")]).0,
        );
        let mut simulator = must_pass(
            Path::new(self.dir),
            "simulator build",
            p4spectec_sim::build(spec, self.arch, CONFIG),
        );
        let paths_include = [PathBuf::from("../../p4c/p4include")];
        for path in paths {
            match simulator.run_stf_test(&paths_include, &path, &path.with_extension("stf"), |_| {})
            {
                Ok(()) => panic!("{}: simulation unexpectedly passed", path.display()),
                Err(report) => check(&path.with_extension("snap"), &[*report], &frames_short()),
            }
        }
    }
}

// = Tests

const P4C_V1MODEL: Suite = Suite {
    name: "p4c-v1model",
    arch: "v1model",
    dir_p4: P4C_SAMPLES,
    dir_stf: P4C_SAMPLES,
    dirs_patch: &[PATCHES_V1MODEL],
    dirs_exclude: &[HEADERS, STATIC, DYNAMIC],
};

#[test]
fn p4c_v1model_al() {
    P4C_V1MODEL.sim(SpecLang::Al);
}

#[test]
fn p4c_v1model_sl() {
    P4C_V1MODEL.sim(SpecLang::Sl);
}

#[test]
fn p4c_v1model_pl() {
    P4C_V1MODEL.sim(SpecLang::Pl);
}

const P4C_EBPF: Suite = Suite {
    name: "p4c-ebpf",
    arch: "ebpf",
    dir_p4: P4C_SAMPLES,
    dir_stf: P4C_SAMPLES,
    dirs_patch: &[],
    dirs_exclude: &[HEADERS, STATIC, DYNAMIC],
};

#[test]
fn p4c_ebpf_al() {
    P4C_EBPF.sim(SpecLang::Al);
}

#[test]
fn p4c_ebpf_sl() {
    P4C_EBPF.sim(SpecLang::Sl);
}

#[test]
fn p4c_ebpf_pl() {
    P4C_EBPF.sim(SpecLang::Pl);
}

const P4C_PSA: Suite = Suite {
    name: "p4c-psa",
    arch: "psa",
    dir_p4: P4C_SAMPLES,
    dir_stf: P4C_SAMPLES,
    dirs_patch: &[],
    dirs_exclude: &[HEADERS, STATIC, DYNAMIC],
};

#[test]
fn p4c_psa_al() {
    P4C_PSA.sim(SpecLang::Al);
}

#[test]
fn p4c_psa_sl() {
    P4C_PSA.sim(SpecLang::Sl);
}

#[test]
fn p4c_psa_pl() {
    P4C_PSA.sim(SpecLang::Pl);
}

const P4TESTGEN_V1MODEL: Suite = Suite {
    name: "p4testgen-v1model",
    arch: "v1model",
    dir_p4: P4C_SAMPLES,
    dir_stf: P4TESTGEN,
    dirs_patch: &[PATCHES_V1MODEL],
    dirs_exclude: &[HEADERS, STATIC, DYNAMIC],
};

#[test]
fn p4testgen_v1model_al() {
    P4TESTGEN_V1MODEL.sim(SpecLang::Al);
}

#[test]
fn p4testgen_v1model_sl() {
    P4TESTGEN_V1MODEL.sim(SpecLang::Sl);
}

#[test]
fn p4testgen_v1model_pl() {
    P4TESTGEN_V1MODEL.sim(SpecLang::Pl);
}

const P4TESTGEN_EBPF: Suite = Suite {
    name: "p4testgen-ebpf",
    arch: "ebpf",
    dir_p4: P4C_SAMPLES,
    dir_stf: P4TESTGEN,
    dirs_patch: &[],
    dirs_exclude: &[HEADERS, STATIC, DYNAMIC],
};

#[test]
fn p4testgen_ebpf_al() {
    P4TESTGEN_EBPF.sim(SpecLang::Al);
}

#[test]
fn p4testgen_ebpf_sl() {
    P4TESTGEN_EBPF.sim(SpecLang::Sl);
}

#[test]
fn p4testgen_ebpf_pl() {
    P4TESTGEN_EBPF.sim(SpecLang::Pl);
}

const CUSTOM_V1MODEL: Suite = Suite {
    name: "custom-v1model",
    arch: "v1model",
    dir_p4: CUSTOM,
    dir_stf: CUSTOM,
    dirs_patch: &[PATCHES_V1MODEL],
    dirs_exclude: &[STATIC, DYNAMIC],
};

#[test]
fn custom_v1model_al() {
    CUSTOM_V1MODEL.sim(SpecLang::Al);
}

#[test]
fn custom_v1model_sl() {
    CUSTOM_V1MODEL.sim(SpecLang::Sl);
}

#[test]
fn custom_v1model_pl() {
    CUSTOM_V1MODEL.sim(SpecLang::Pl);
}

const REGRESSION_V1MODEL: Suite = Suite {
    name: "regression-v1model",
    arch: "v1model",
    dir_p4: REGRESSION,
    dir_stf: REGRESSION,
    dirs_patch: &[],
    dirs_exclude: &[STATIC, DYNAMIC],
};

#[test]
fn regression_v1model_al() {
    REGRESSION_V1MODEL.sim(SpecLang::Al);
}

#[test]
fn regression_v1model_sl() {
    REGRESSION_V1MODEL.sim(SpecLang::Sl);
}

#[test]
fn regression_v1model_pl() {
    REGRESSION_V1MODEL.sim(SpecLang::Pl);
}

const REGRESSION_EBPF: Suite = Suite {
    name: "regression-ebpf",
    arch: "ebpf",
    dir_p4: REGRESSION,
    dir_stf: REGRESSION,
    dirs_patch: &[],
    dirs_exclude: &[STATIC, DYNAMIC],
};

#[test]
fn regression_ebpf_al() {
    REGRESSION_EBPF.sim(SpecLang::Al);
}

#[test]
fn regression_ebpf_sl() {
    REGRESSION_EBPF.sim(SpecLang::Sl);
}

#[test]
fn regression_ebpf_pl() {
    REGRESSION_EBPF.sim(SpecLang::Pl);
}

const FIXTURE_NEG: Fixtures = Fixtures { dir: "sim/neg", arch: "ebpf" };

#[test]
fn fixture_neg_al() {
    FIXTURE_NEG.check(SpecLang::Al);
}

#[test]
fn fixture_neg_sl() {
    FIXTURE_NEG.check(SpecLang::Sl);
}

#[test]
fn fixture_neg_pl() {
    FIXTURE_NEG.check(SpecLang::Pl);
}
