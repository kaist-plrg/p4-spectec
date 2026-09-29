//! Splice admission, diagnostic transport, and document output

use std::{fs, process::Command};

use crate::directory::Directory;

const SPEC: &str = include_str!("../fixtures/splice/spec.watsup");
const DOCUMENTS: &[(&str, &str, &str)] = &[
    (
        "body",
        include_str!("../fixtures/splice/body.adoc"),
        include_str!("../fixtures/splice/body.expected"),
    ),
    (
        "titles",
        include_str!("../fixtures/splice/titles.adoc"),
        include_str!("../fixtures/splice/titles.expected"),
    ),
];

fn binary() -> Command {
    Command::new(env!("CARGO_BIN_EXE_p4spec-rust"))
}

/// Requires admission failure before the absent specification can be read.
fn rejected(args: &[&str], message: &str) {
    let directory = Directory::new("splice-admission");
    fs::write(directory.0.join("input.adoc"), "original input").unwrap();
    fs::write(directory.0.join("output.adoc"), "original output").unwrap();
    let output = binary()
        .current_dir(&directory.0)
        .args(["splice", "missing.watsup"])
        .args(args)
        .output()
        .unwrap();
    assert_eq!(output.status.code(), Some(1));
    assert!(output.stdout.is_empty());
    assert_eq!(String::from_utf8(output.stderr).unwrap(), format!("{message}\n\n"));
    assert_eq!(fs::read_to_string(directory.0.join("input.adoc")).unwrap(), "original input");
    assert_eq!(fs::read_to_string(directory.0.join("output.adoc")).unwrap(), "original output");
    assert_eq!(fs::read_dir(&directory.0).unwrap().count(), 2);
}

#[test]
fn inplace_rejects_output_paths_before_other_checks() {
    for args in [
        vec!["--inplace", "--out", "output.adoc"],
        vec!["--inplace", "--splice", "input.adoc", "--out", "output.adoc"],
    ] {
        rejected(
            &args,
            "error[command/splice-output-conflict]: options `--inplace` and `--out` cannot be used together",
        );
    }
}

#[test]
fn splice_requires_inputs_in_both_output_modes() {
    for args in [vec![], vec!["--inplace"], vec!["--out", "output.adoc"]] {
        rejected(
            &args,
            "error[command/splice-input-required]: splice requires at least one input file",
        );
    }
}

#[test]
fn splice_reports_both_unpaired_file_counts() {
    for (args, message) in [
        (
            vec!["--splice", "input.adoc", "--splice", "other.adoc", "--out", "output.adoc"],
            "splice expects equal numbers of input and output files, but got 2 input files and 1 output file",
        ),
        (
            vec!["--splice", "input.adoc", "--out", "output.adoc", "--out", "other.adoc"],
            "splice expects equal numbers of input and output files, but got 1 input file and 2 output files",
        ),
        (
            vec!["--splice", "input.adoc"],
            "splice expects equal numbers of input and output files, but got 1 input file and 0 output files",
        ),
    ] {
        rejected(&args, &format!("error[command/splice-file-count-mismatch]: {message}"));
    }
}

#[test]
fn splice_writes_paired_files_and_accepts_inplace() {
    for inplace in [false, true] {
        let directory = Directory::new("splice-output");
        fs::write(directory.0.join("spec.watsup"), "var x : nat\n").unwrap();
        fs::write(directory.0.join("input.adoc"), "literal document\n").unwrap();
        let mut command = binary();
        command
            .current_dir(&directory.0)
            .args(["splice", "spec.watsup", "--splice", "input.adoc"]);
        let path_output = if inplace {
            command.arg("--inplace");
            "input.adoc"
        } else {
            command.args(["--out", "output.adoc"]);
            "output.adoc"
        };
        let output = command.output().unwrap();
        assert!(output.status.success(), "{}", String::from_utf8_lossy(&output.stderr));
        assert!(output.stdout.is_empty());
        assert_eq!(
            fs::read_to_string(directory.0.join(path_output)).unwrap(),
            "literal document\n"
        );
    }
}

#[test]
fn splice_preserves_source_math_prose_and_cross_document_links() {
    for inplace in [false, true] {
        let directory = Directory::new("splice-documents");
        fs::write(directory.0.join("spec.watsup"), SPEC).unwrap();
        let mut command = binary();
        command
            .current_dir(&directory.0)
            .args(["splice", "spec.watsup"]);
        for (name, text, _) in DOCUMENTS {
            let path = format!("{name}.adoc");
            fs::write(directory.0.join(&path), text).unwrap();
            command.args(["--splice", &path]);
            if !inplace {
                command.args(["--out", &format!("{name}.out")]);
            }
        }
        if inplace {
            command.arg("--inplace");
        }
        let output = command.output().unwrap();
        assert!(output.status.success(), "{}", String::from_utf8_lossy(&output.stderr));
        assert!(output.stdout.is_empty());
        for (name, text, text_expect) in DOCUMENTS {
            let extension = if inplace { "adoc" } else { "out" };
            assert_eq!(
                fs::read_to_string(directory.0.join(format!("{name}.{extension}"))).unwrap(),
                *text_expect,
            );
            if !inplace {
                assert_eq!(
                    fs::read_to_string(directory.0.join(format!("{name}.adoc"))).unwrap(),
                    *text
                );
            }
        }
        assert_eq!(
            String::from_utf8(output.stderr).unwrap(),
            include_str!("../fixtures/splice/warnings.expected"),
        );
    }
}

#[test]
fn splice_forwards_complete_pipeline_reports() {
    let path =
        std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("test-driver/expected/diagnostic");
    for name in [
        "prose/hint-prose-index-out-of-range",
        "prose/hint-prose-fields-arity",
        "prose/hint-prose-fields-shape",
        "elab/notation-variant-multiple-mismatches",
    ] {
        let output = binary()
            .current_dir(&path)
            .args([
                "splice",
                &format!("{name}.watsup"),
                "--splice",
                "input.adoc",
                "--out",
                "output.adoc",
            ])
            .output()
            .unwrap();
        assert_eq!(output.status.code(), Some(1));
        assert!(output.stdout.is_empty());
        assert_eq!(
            String::from_utf8(output.stderr).unwrap(),
            fs::read_to_string(path.join(format!("{name}.expect"))).unwrap(),
            "{name}",
        );
    }
}

#[test]
fn splice_keeps_warning_order_before_output_failure() {
    let directory = Directory::new("splice-warnings");
    fs::write(directory.0.join("spec.watsup"), "dec $missing : nat\n").unwrap();
    fs::write(directory.0.join("input.adoc"), "${func-prose: absent}\n").unwrap();
    fs::write(directory.0.join("blocked"), "regular file").unwrap();
    let output = binary()
        .current_dir(&directory.0)
        .args(["splice", "spec.watsup", "--splice", "input.adoc", "--out", "blocked/output.adoc"])
        .output()
        .unwrap();
    assert_eq!(output.status.code(), Some(1));
    assert!(output.stdout.is_empty());
    let text = String::from_utf8(output.stderr).unwrap();
    let codes: Vec<_> = text
        .lines()
        .filter_map(|line| line.split_once("]:"))
        .map(|(code, _)| code)
        .collect();
    assert_eq!(codes[0], "warning[elab/function-clause-missing");
    assert_eq!(codes[1], "warning[splice/key-not-found");
    assert_eq!(codes.last(), Some(&"error[splice/io"));
    assert_eq!(codes.len(), 21);
    assert!(
        codes[2..20]
            .iter()
            .all(|code| *code == "warning[splice/keys-unused")
    );
    assert!(text.contains(" = missing\n"), "{text}");
    assert_eq!(fs::read_dir(&directory.0).unwrap().count(), 3);
}

#[test]
fn splice_collects_adoc_warnings_before_later_io_failure() {
    let directory = Directory::new("splice-adoc-warning");
    let source =
        "dec $f : nat\n  hint(prose_in \"[x]<y>\")\ndef $f = 0\ndec $g : nat\ndef $g = $f\n";
    fs::write(directory.0.join("spec.watsup"), source).unwrap();
    fs::write(directory.0.join("input.adoc"), "${func-title-prose: f}\n${func-prose: g}\n")
        .unwrap();
    fs::write(directory.0.join("blocked"), "original").unwrap();
    let output = binary()
        .current_dir(&directory.0)
        .args(["splice", "spec.watsup", "--splice", "input.adoc", "--out", "blocked/output.adoc"])
        .output()
        .unwrap();
    assert_eq!(output.status.code(), Some(1));
    assert!(output.stdout.is_empty());
    let text = String::from_utf8(output.stderr).unwrap();
    let idx_warning = text.find("warning[adoc/link-text-invalid]").expect(&text);
    let idx_unused = text.find("warning[splice/keys-unused]").expect(&text);
    let idx_error = text.find("error[splice/io]").expect(&text);
    assert!(idx_warning < idx_unused && idx_unused < idx_error, "{text}");
    assert!(text.contains("spec.watsup:"), "{text}");
    assert!(text.contains(" = [x]<y>"), "{text}");
    assert!(!text.contains("Warning:"), "{text}");
    assert_eq!(fs::read_to_string(directory.0.join("blocked")).unwrap(), "original");
}
