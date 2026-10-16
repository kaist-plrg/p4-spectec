//! Corpus output suppression and expectation diagnostics

use std::{fs, path::Path, process::Command};

use serde_json::json;

/// Runs one empty P4 input through a specification with a debug premise.
fn run(language: &str, status: &str) -> std::process::Output {
    let dir = tempfile::tempdir().unwrap();
    let path_driver = Path::new(env!("CARGO_MANIFEST_DIR"));
    let path_spec = dir.path().join("spec.watsup");
    let path_corpus = dir.path().join("corpus");
    let path_expected = dir.path().join("results.expected");
    let path_config = dir.path().join("run.json");
    let path_index = dir.path().join("suites.json");

    // Match the parser's empty program constructor without loading the P4 spec
    fs::write(
        &path_spec,
        "syntax p4program = _EMPTY\n\n\
         relation R: p4program |- p4program\n  hint(input %0)\n\n\
         rule R: p4program |- p4program\n  -- debug \"hidden-corpus-output\"\n",
    )
    .unwrap();
    fs::create_dir(&path_corpus).unwrap();
    let path_program = path_corpus.join("empty.p4");
    fs::write(&path_program, "").unwrap();
    fs::write(&path_expected, format!("{status}\t{}\n", path_program.display())).unwrap();

    // Override only the execution registration in the normal driver index
    let config = json!({
        "spec": [path_spec],
        "includes": [dir.path()],
        "excludes": [],
        "suites": [{
            "name": "stdout", "root": path_corpus, "relation": "R",
            "expected": path_expected, "languages": ["al", "sl", "pl"]
        }]
    });
    fs::write(&path_config, config.to_string()).unwrap();
    let mut index: serde_json::Value =
        serde_json::from_str(&fs::read_to_string(path_driver.join("suites.json")).unwrap())
            .unwrap();
    index["suites"]["run"] = json!(path_config);
    fs::write(&path_index, index.to_string()).unwrap();

    // Capture the actual binary's descriptors, independently of the test harness
    Command::new(env!("CARGO_BIN_EXE_p4spectec-test"))
        .env_remove("UPDATE_EXPECT")
        .args(["--registry", path_index.to_str().unwrap(), "run", language])
        .output()
        .unwrap()
}

#[test]
fn execution_hides_debug_output_and_keeps_summary() {
    for language in ["al", "sl", "pl"] {
        let output = run(language, "pass");
        let text_stdout = String::from_utf8(output.stdout).unwrap();
        let text_stderr = String::from_utf8(output.stderr).unwrap();
        assert!(output.status.success(), "{language}: {text_stdout}\n{text_stderr}");
        assert!(text_stdout.is_empty(), "{language}: {text_stdout}");
        assert!(text_stderr.contains("all 1 file results matched expected"), "{text_stderr}");
    }
}

#[test]
fn expectation_failure_keeps_diff_visible() {
    let output = run("sl", "fail");
    let text_stdout = String::from_utf8(output.stdout).unwrap();
    let text_stderr = String::from_utf8(output.stderr).unwrap();
    assert!(!output.status.success());
    assert!(text_stderr.contains("executed=1 pass=1 fail=0"), "{text_stderr}");
    assert!(text_stdout.contains("fail") && text_stdout.contains("pass"), "{text_stdout}");
    assert!(!text_stdout.contains("hidden-corpus-output"), "{text_stdout}");
}
