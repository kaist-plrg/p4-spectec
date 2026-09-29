//! Command-line integration behavior

use std::{path::Path, process::Command};

fn binary() -> Command {
    Command::new(env!("CARGO_BIN_EXE_p4spec-rust"))
}

fn fixture(path: &str) -> std::path::PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures")
        .join(path)
}

fn repo() -> std::path::PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .to_owned()
}

/// Validates the specification warning and returns subsequent CLI diagnostics.
fn after_spec_warning(stderr: &[u8]) -> &str {
    let text = std::str::from_utf8(stderr).unwrap();
    let path = repo().join("spec/4-p4-ir/4.5-ir-to-surface.watsup");
    let warning = format!(
        concat!(
            "warning[elab/function-clause-missing]: ",
            "function `sink` has no clauses defined\n",
            "  ┌─ {}:1:1\n",
            "  │\n",
            "1 │ dec $sink<T>() : T\n",
            "  │ ^^^^^^^^^^^^^^^^^^\n\n",
        ),
        path.display(),
    );
    text.strip_prefix(&warning)
        .unwrap_or_else(|| panic!("missing specification warning: {text}"))
}

#[test]
fn test_elab_command_prints_the_intermediate_spec() {
    let output = binary()
        .arg("elab")
        .arg(fixture("cli/simple.watsup"))
        .output()
        .expect("run elab command");

    assert!(output.status.success());
    assert_eq!(String::from_utf8(output.stdout).unwrap(), "var x : nat\n");
    assert!(output.stderr.is_empty());
}

#[test]
fn test_algo_command_prints_the_algorithmic_spec() {
    let output = binary()
        .arg("algo")
        .arg(fixture("cli/simple.watsup"))
        .output()
        .expect("run algo command");

    assert!(output.status.success());
    assert_eq!(String::from_utf8(output.stdout).unwrap(), "var x : nat\n");
    assert!(output.stderr.is_empty());
}

#[test]
fn test_struct_command_prints_control_flow_without_rule_groups() {
    let output = binary()
        .arg("struct")
        .arg(fixture("structure/definitions.watsup"))
        .output()
        .expect("run struct command");

    assert!(output.status.success(), "{}", String::from_utf8_lossy(&output.stderr));
    let stderr = String::from_utf8(output.stderr).unwrap();
    let warnings: Vec<_> = stderr
        .lines()
        .filter(|line| line.starts_with("warning["))
        .collect();
    assert_eq!(
        warnings,
        [
            "warning[elab/relation-rule-missing]: relation `Empty` has no rules defined",
            "warning[elab/function-clause-missing]: function `empty` has no clauses defined",
        ]
    );
    let stdout = String::from_utf8(output.stdout).unwrap();
    assert!(stdout.contains("Return CONT"), "{stdout}");
    assert!(stdout.contains("Otherwise,"), "{stdout}");
    assert!(!stdout.contains("Group "), "{stdout}");
    assert!(stdout.ends_with('\n'));
}

#[test]
fn test_prose_command_prints_annotated_rule_groups() {
    let output = binary()
        .arg("prose")
        .arg(fixture("structure/definitions.watsup"))
        .output()
        .expect("run prose command");
    assert!(output.status.success(), "{}", String::from_utf8_lossy(&output.stderr));
    let stderr = String::from_utf8(output.stderr).unwrap();
    let warnings: Vec<_> = stderr
        .lines()
        .filter(|line| line.starts_with("warning["))
        .collect();
    assert_eq!(
        warnings,
        [
            "warning[elab/relation-rule-missing]: relation `Empty` has no rules defined",
            "warning[elab/function-clause-missing]: function `empty` has no clauses defined",
        ]
    );
    let stdout = String::from_utf8(output.stdout).unwrap();
    assert!(stdout.contains("Group ret:"), "{stdout}");
    assert!(stdout.contains("Group else:"), "{stdout}");
    assert!(stdout.contains("Return ((CONT) as flow)"), "{stdout}");
    assert!(stdout.ends_with('\n'));
}

#[test]
fn test_prose_command_reports_pipeline_errors_on_stderr() {
    for (path, message) in [
        ("frontend/negative/malformed-token.watsup", "error[parse/character-invalid]"),
        (
            "elaboration/operator_not_defined.watsup",
            "operator '+' is not defined for 'bool' and 'bool'",
        ),
        ("algorithmic/impure_else_premises.watsup", "error[algo/otherwise-condition-invalid]"),
    ] {
        let output = binary().arg("prose").arg(fixture(path)).output().unwrap();
        assert_eq!(output.status.code(), Some(1));
        assert!(output.stdout.is_empty());
        let stderr = String::from_utf8(output.stderr).unwrap();
        assert!(stderr.contains(message), "{stderr}");
    }
}

#[test]
fn test_struct_command_reports_pipeline_errors_on_stderr() {
    for (path, message) in [
        ("frontend/negative/malformed-token.watsup", "error[parse/character-invalid]"),
        (
            "elaboration/operator_not_defined.watsup",
            "operator '+' is not defined for 'bool' and 'bool'",
        ),
        ("algorithmic/impure_else_premises.watsup", "error[algo/otherwise-condition-invalid]"),
    ] {
        let output = binary().arg("struct").arg(fixture(path)).output().unwrap();
        assert_eq!(output.status.code(), Some(1));
        assert!(output.stdout.is_empty());
        let stderr = String::from_utf8(output.stderr).unwrap();
        assert!(stderr.contains(message), "{stderr}");
    }
}

#[test]
fn test_algo_command_reports_conversion_errors_on_stderr() {
    let output = binary()
        .arg("algo")
        .arg(fixture("algorithmic/impure_else_premises.watsup"))
        .output()
        .expect("run algo command");

    assert_eq!(output.status.code(), Some(1));
    assert!(output.stdout.is_empty());
    assert!(
        String::from_utf8(output.stderr)
            .unwrap()
            .contains("error[algo/otherwise-condition-invalid]")
    );
}

#[test]
fn test_elab_command_reports_frontend_errors_on_stderr() {
    let output = binary()
        .arg("elab")
        .arg(fixture("frontend/negative/malformed-token.watsup"))
        .output()
        .expect("run elab command");

    assert_eq!(output.status.code(), Some(1));
    assert!(output.stdout.is_empty());
    assert!(
        String::from_utf8(output.stderr)
            .unwrap()
            .contains("error[parse/character-invalid]")
    );
}

#[test]
fn test_elab_command_reports_elaboration_errors_on_stderr() {
    let output = binary()
        .arg("elab")
        .arg(fixture("elaboration/operator_not_defined.watsup"))
        .output()
        .expect("run elab command");

    assert_eq!(output.status.code(), Some(1));
    assert!(output.stdout.is_empty());
    assert!(
        String::from_utf8(output.stderr)
            .unwrap()
            .contains("operator '+' is not defined for 'bool' and 'bool'")
    );
}

#[test]
fn test_commands_require_at_least_one_path() {
    for command in ["elab", "algo", "struct", "prose", "splice"] {
        let output = binary().arg(command).output().expect("run command");
        assert_eq!(output.status.code(), Some(2));
        assert!(output.stdout.is_empty());
        let stderr = String::from_utf8(output.stderr).unwrap();
        assert!(stderr.contains("Usage:"));
        assert!(stderr.contains("<PATH>"));
    }
}

#[test]
fn test_help_prints_commands() {
    let output = binary().arg("--help").output().expect("run help");
    assert!(output.status.success());
    assert!(output.stderr.is_empty());
    let stdout = String::from_utf8(output.stdout).unwrap();
    assert!(stdout.contains("Commands:"));
    assert!(stdout.contains("elab"));
    assert!(stdout.contains("algo"));
    assert!(stdout.contains("struct"));
    assert!(stdout.contains("prose"));
    assert!(stdout.contains("splice"));
}

#[test]
fn test_subcommand_help_prints_paths_without_processing_inputs() {
    let output = binary()
        .args(["elab", "missing.watsup", "--help"])
        .output()
        .expect("run command help");
    assert!(output.status.success());
    assert!(output.stderr.is_empty());
    let stdout = String::from_utf8(output.stdout).unwrap();
    assert!(stdout.contains("Usage:"));
    assert!(stdout.contains("<PATH>"));
}

#[test]
fn test_invalid_arguments_report_usage_errors() {
    for args in [vec!["unknown"], vec!["elab", "--unknown"]] {
        let output = binary().args(args).output().expect("run invalid arguments");
        assert_eq!(output.status.code(), Some(2));
        assert!(output.stdout.is_empty());
        assert!(String::from_utf8(output.stderr).unwrap().contains("Usage:"));
    }
}

#[test]
fn test_commands_preserve_multiple_input_order() {
    for command in ["elab", "algo", "struct"] {
        let output = binary()
            .arg(command)
            .arg(fixture("cli/second.watsup"))
            .arg(fixture("cli/simple.watsup"))
            .output()
            .expect("run multiple inputs");
        assert!(output.status.success());
        assert!(output.stderr.is_empty());
        assert_eq!(String::from_utf8(output.stdout).unwrap(), "var y : nat\n\nvar x : nat\n");
    }
}

#[test]
fn test_commands_accept_hyphenated_paths_after_separator() {
    for command in ["elab", "algo", "struct"] {
        let output = binary()
            .current_dir(fixture("cli"))
            .args([command, "--", "-input.watsup"])
            .output()
            .expect("run hyphenated input");
        assert!(output.status.success());
        assert!(output.stderr.is_empty());
        assert_eq!(String::from_utf8(output.stdout).unwrap(), "var z : nat\n");
    }
}

fn run_command(relation: &str, program: &str) -> Command {
    run_command_with("--al", relation, program)
}

fn run_command_with(stage: &str, relation: &str, program: &str) -> Command {
    let mut command = binary();
    command
        .args(["run", stage])
        .arg(fixture("cli/run/types.watsup"))
        .arg(fixture("cli/run/relations.watsup"))
        .args(["--rel", relation, "-p"])
        .arg(fixture(program));
    command
}

#[test]
fn test_run_sl_and_pl_native_success_and_multiple_spec_paths() {
    for stage in ["--sl", "--pl"] {
        let output = run_command_with(stage, "Pass", "cli/run/empty.p4")
            .output()
            .unwrap();
        assert!(output.status.success(), "{stage}: {}", String::from_utf8_lossy(&output.stderr));
        assert_eq!(output.stdout, b"passed\n", "{stage}");
        assert!(output.stderr.is_empty(), "{stage}");
    }
}

#[test]
fn test_run_interpreters_use_compact_context_and_rich_causes() {
    for stage in ["--al", "--sl", "--pl"] {
        let output = run_command_with(stage, "Reject", "cli/run/empty.p4")
            .output()
            .unwrap();
        assert_eq!(output.status.code(), Some(1));
        assert!(output.stdout.is_empty());
        let text = String::from_utf8(output.stderr).unwrap();
        assert!(
            text.starts_with("note: execution failed\n└─ note: while invoking Reject\n"),
            "{stage}: {text}"
        );
        assert_eq!(text.matches("┌─").count(), 1, "only the cause has a snippet: {text}");
        assert!(text.contains("error[runtime/condition-unmet]"), "{text}");
        assert!(text.contains("-- if false"), "{text}");
        assert!(text.contains("^^^^^"), "{text}");
        if stage == "--al" {
            assert!(!text.contains("while evaluating"), "{text}");
            assert!(text.contains("relations.watsup:7:9"), "{text}");
        }
    }
}

#[test]
fn test_execution_commands_keep_elaboration_frames_rich() {
    let path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("test-driver/expected/diagnostic/elab/type-shape-index-path.watsup");
    let output = binary().arg("elab").arg(&path).output().unwrap();
    assert_eq!(output.status.code(), Some(1));
    let text = std::str::from_utf8(&output.stderr).unwrap();
    let (context, _) = text.split_once("├─ error[").unwrap();
    assert!(context.contains("note: expression elaboration failed\n  ┌─"), "{text}");
    assert!(context.contains("def $f(nat) = nat[[0] = 1]"), "{text}");
    for stage in ["--al", "--sl", "--pl"] {
        for command in ["run", "sim"] {
            let mut process = binary();
            process
                .args([command, stage])
                .arg(&path)
                .args(["-p", "unused.p4"]);
            if command == "run" {
                process.args(["--rel", "Unused"]);
            } else {
                process.args(["--arch", "ebpf", "--stf", "unused.stf"]);
            }
            let output_execution = process.output().unwrap();
            assert_eq!(output_execution.status.code(), Some(1));
            assert_eq!(output_execution.stderr, output.stderr, "{command} {stage}");
        }
    }
}

#[test]
fn test_run_sl_and_pl_distinguish_syntax_and_runtime_failures() {
    for stage in ["--sl", "--pl"] {
        for (relation, program, category) in [
            ("Pass", "cli/run/invalid.p4", "syntax error:"),
            ("Reject", "cli/run/empty.p4", "note: execution failed"),
        ] {
            let output = run_command_with(stage, relation, program).output().unwrap();
            assert_eq!(output.status.code(), Some(1), "{stage}");
            assert!(output.stdout.is_empty(), "{stage}");
            let error = String::from_utf8(output.stderr).unwrap();
            assert!(error.starts_with(category), "{stage}: {error}");
        }
    }
}

#[test]
fn test_run_sl_and_pl_honor_cache_det_and_guard_controls() {
    for stage in ["--sl", "--pl"] {
        for (relation, flag) in [("Ambiguous", "--det"), ("Unchecked", "--guard")] {
            let output = run_command_with(stage, relation, "cli/run/empty.p4")
                .output()
                .unwrap();
            assert!(
                output.status.success(),
                "{stage}: {}",
                String::from_utf8_lossy(&output.stderr)
            );
            let output = run_command_with(stage, relation, "cli/run/empty.p4")
                .arg(flag)
                .arg("--no-cache")
                .output()
                .unwrap();
            assert_eq!(output.status.code(), Some(1), "{stage}");
            assert!(String::from_utf8_lossy(&output.stderr).contains("error[runtime/"));
        }
    }
}

#[test]
fn test_run_al_native_success_and_multiple_spec_paths() {
    let output = run_command("Pass", "cli/run/empty.p4").output().unwrap();
    assert!(output.status.success(), "{}", String::from_utf8_lossy(&output.stderr));
    assert_eq!(output.stdout, b"passed\n");
    assert!(output.stderr.is_empty());
}

#[test]
fn test_run_al_initializes_dummy_extern_objects() {
    let repo = repo();
    let output = binary()
        .args(["run", "--al"])
        .arg(repo.join("spec"))
        .args(["--rel", "Program_inst", "-p"])
        .arg(repo.join("p4c/testdata/p4_16_samples/action_profile-bmv2.p4"))
        .arg("-i")
        .arg(repo.join("p4c/p4include"))
        .output()
        .unwrap();

    assert!(output.status.success(), "{}", String::from_utf8_lossy(&output.stderr));
    assert_eq!(output.stdout, b"passed\n");
    assert!(after_spec_warning(&output.stderr).is_empty());
}

#[test]
fn test_run_al_distinguishes_syntax_and_runtime_failures() {
    for (relation, program, category) in [
        ("Pass", "cli/run/invalid.p4", "syntax error:"),
        ("Reject", "cli/run/empty.p4", "note: execution failed"),
    ] {
        let output = run_command(relation, program).output().unwrap();
        assert_eq!(output.status.code(), Some(1));
        assert!(output.stdout.is_empty());
        let error = String::from_utf8(output.stderr).unwrap();
        assert!(error.starts_with(category), "{error}");
        if category == "syntax error:" {
            assert!(error.contains("invalid.p4"), "{error}");
        } else {
            assert!(error.contains("Reject"), "{error}");
        }
    }
}

#[test]
fn test_run_al_repeated_include_directories() {
    let output = run_command("Pass", "cli/run/includes.p4")
        .arg("-i")
        .arg(fixture("cli/run/first"))
        .arg("-i")
        .arg(fixture("cli/run/second"))
        .output()
        .unwrap();
    assert!(output.status.success(), "{}", String::from_utf8_lossy(&output.stderr));
    assert_eq!(output.stdout, b"passed\n");
}

#[test]
fn test_run_al_det_and_guard_controls_change_execution() {
    for (relation, flag) in [("Ambiguous", "--det"), ("Unchecked", "--guard")] {
        let output = run_command(relation, "cli/run/empty.p4").output().unwrap();
        assert!(output.status.success(), "{}", String::from_utf8_lossy(&output.stderr));
        let output = run_command(relation, "cli/run/empty.p4")
            .arg(flag)
            .arg("--no-cache")
            .output()
            .unwrap();
        assert_eq!(output.status.code(), Some(1));
        assert!(String::from_utf8_lossy(&output.stderr).contains("error[runtime/"));
    }
    let output = run_command("Pass", "cli/run/empty.p4")
        .args(["--det", "--guard"])
        .output()
        .unwrap();
    assert!(output.status.success(), "{}", String::from_utf8_lossy(&output.stderr));
}

#[test]
fn test_run_al_requires_flags_and_rejects_unsupported_options() {
    for args in [
        vec!["run"],
        vec!["run", "--al"],
        vec!["run", "--al", "spec", "--rel", "Pass"],
        vec!["run", "--al", "spec", "-p", "empty.p4"],
        vec!["run", "spec", "--rel", "Pass", "-p", "empty.p4"],
        vec!["run", "--al", "--rel", "Pass", "-p", "empty.p4"],
        vec!["run", "--sl"],
    ] {
        let output = binary().args(args).output().unwrap();
        assert_eq!(output.status.code(), Some(2));
        assert!(String::from_utf8_lossy(&output.stderr).contains("Usage:"));
    }
    for flag in ["--trace", "--profile", "--unknown", "-i", "--rel", "-p"] {
        let output = run_command("Pass", "cli/run/empty.p4")
            .arg(flag)
            .output()
            .unwrap();
        assert_eq!(output.status.code(), Some(2), "{flag}");
    }
}

#[test]
fn test_run_al_reports_spec_load_failure() {
    let output = binary()
        .args(["run", "--al"])
        .arg(fixture("frontend/negative/malformed-token.watsup"))
        .args(["--rel", "Pass", "-p"])
        .arg(fixture("cli/run/empty.p4"))
        .output()
        .unwrap();
    assert_eq!(output.status.code(), Some(1));
    assert!(output.stdout.is_empty());
    assert!(String::from_utf8_lossy(&output.stderr).contains("error[parse/character-invalid]"));
}

#[test]
fn test_run_help_lists_only_implemented_controls() {
    let output = binary().args(["run", "--help"]).output().unwrap();
    assert!(output.status.success());
    assert!(output.stderr.is_empty());
    let usage = String::from_utf8(output.stdout).unwrap();
    for flag in ["--al", "--sl", "--pl", "--rel", "--det", "--guard", "--no-cache"] {
        assert!(usage.contains(flag));
    }
    for flag in ["--trace", "--profile", "--arch"] {
        assert!(!usage.contains(flag));
    }
}

#[test]
fn test_run_interpreters_input_guards_do_not_depend_on_cache() {
    for stage in ["--al", "--sl", "--pl"] {
        for cache in [false, true] {
            let mut command = run_command_with(stage, "Unchecked", "cli/run/empty.p4");
            command.arg("--guard");
            if !cache {
                command.arg("--no-cache");
            }
            let output = command.output().unwrap();
            assert_eq!(output.status.code(), Some(1), "{stage}");
            assert!(output.stdout.is_empty());
            assert!(
                String::from_utf8_lossy(&output.stderr)
                    .contains("error[runtime/relation-input-type-mismatch]"),
                "{stage}: {}",
                String::from_utf8_lossy(&output.stderr)
            );
        }
    }
}

fn sim_command(arch: &str) -> Command {
    sim_command_with("--al", arch)
}

fn sim_command_with(stage: &str, arch: &str) -> Command {
    let repo = repo();
    let path = repo.join("p4spec/test/micro").join(format!("sim-{arch}"));
    let mut command = binary();
    command
        .args(["sim", stage])
        .arg(repo.join("spec"))
        .args(["--arch", arch, "-p"])
        .arg(path.join(format!("{arch}.p4")))
        .arg("-i")
        .arg(repo.join("p4c/p4include"));
    command
}

#[test]
fn test_sim_sl_runs_all_native_architectures_and_plugin_encodings() {
    for arch in ["ebpf", "psa", "v1model"] {
        for encoding in [None, Some("arena-relative"), Some("arena-independent")] {
            let mut command = sim_command_with("--sl", arch);
            if let Some(encoding) = encoding {
                command.args(["--plugin-encoding", encoding]);
            }
            let output = command
                .arg("--stf")
                .arg(repo().join(format!("p4spec/test/micro/sim-{arch}/{arch}.stf")))
                .output()
                .unwrap();
            assert!(output.status.success(), "{arch}: {}", String::from_utf8_lossy(&output.stderr));
            assert!(after_spec_warning(&output.stderr).is_empty());
            let stdout = String::from_utf8(output.stdout).unwrap();
            let expected = std::fs::read_to_string(
                repo().join(format!("p4spec/test/micro/micro_sim_{arch}_al.expected")),
            )
            .unwrap();
            let mut lines: Vec<_> = expected
                .lines()
                .filter(|line| line.starts_with("[PASS] Transmitted "))
                .collect();
            lines.push("passed");
            assert_eq!(stdout, format!("{}\n", lines.join("\n")), "{arch}");
        }
    }
}

#[test]
fn test_sim_pl_runs_native_plugin() {
    let output = sim_command_with("--pl", "ebpf")
        .arg("--stf")
        .arg(repo().join("p4spec/test/micro/sim-ebpf/ebpf.stf"))
        .output()
        .unwrap();
    assert!(output.status.success(), "{}", String::from_utf8_lossy(&output.stderr));
    assert!(String::from_utf8_lossy(&output.stdout).contains("[PASS] Transmitted"));
}

#[test]
fn test_sim_sl_preserves_host_state_across_stf_statements() {
    let output = sim_command_with("--sl", "psa")
        .arg("--stf")
        .arg(repo().join("p4spec/test/micro/sim-psa/psa.stf"))
        .output()
        .unwrap();
    assert!(output.status.success(), "{}", String::from_utf8_lossy(&output.stderr));
    assert!(String::from_utf8_lossy(&output.stdout).contains("[PASS] Transmitted"));
}

#[test]
fn test_run_and_sim_require_exactly_one_interpreter_stage() {
    for args in [
        vec!["run", "--al", "--sl", "spec", "--rel", "Pass", "-p", "empty.p4"],
        vec!["run", "--sl", "--pl", "spec", "--rel", "Pass", "-p", "empty.p4"],
        vec![
            "sim",
            "--al",
            "--sl",
            "spec",
            "--arch",
            "ebpf",
            "-p",
            "empty.p4",
            "--stf",
            "input.stf",
        ],
    ] {
        let output = binary().args(args).output().unwrap();
        assert_eq!(output.status.code(), Some(2));
        assert!(String::from_utf8_lossy(&output.stderr).contains("cannot be used with"));
    }
}

#[test]
fn test_sim_help_lists_native_controls_without_processing_inputs() {
    let output = binary()
        .args(["sim", "missing.watsup", "--help"])
        .output()
        .unwrap();
    assert!(output.status.success());
    assert!(output.stderr.is_empty());
    let usage = String::from_utf8(output.stdout).unwrap();
    for flag in [
        "<PATH>",
        "--al",
        "--sl",
        "--pl",
        "--arch",
        "--plugin-encoding",
        "-p",
        "--stf",
        "-i",
        "--no-cache",
        "--det",
        "--guard",
    ] {
        assert!(usage.contains(flag), "{flag}: {usage}");
    }
    for flag in ["--rel", "--trace", "--profile"] {
        assert!(!usage.split_whitespace().any(|word| word == flag), "{flag}: {usage}");
    }
}

#[test]
fn test_sim_al_requires_flags() {
    let args = ["sim", "--al", "spec", "--arch", "ebpf", "-p", "empty.p4", "--stf", "input.stf"];
    for (idx, len) in [(1, 1), (2, 1), (3, 2), (5, 2), (7, 2)] {
        let output = binary()
            .args(&args[..idx])
            .args(&args[idx + len..])
            .output()
            .unwrap();
        assert_eq!(output.status.code(), Some(2), "missing {}", args[idx]);
        assert!(output.stdout.is_empty());
        assert!(String::from_utf8_lossy(&output.stderr).contains("Usage:"));
    }
}

#[test]
fn test_sim_rejects_unknown_plugin_encoding() {
    let output = sim_command("psa")
        .args(["--stf", "input.stf", "--plugin-encoding", "unknown"])
        .output()
        .unwrap();
    assert_eq!(output.status.code(), Some(2));
    assert!(output.stdout.is_empty());
    assert!(
        String::from_utf8_lossy(&output.stderr)
            .contains("expected arena-relative or arena-independent")
    );
}

#[test]
fn test_sim_al_rejects_unknown_architectures() {
    let output = binary()
        .args(["sim", "--al"])
        .arg(fixture("cli/simple.watsup"))
        .args(["--arch", "unknown", "-p", "empty.p4", "--stf", "input.stf"])
        .output()
        .unwrap();
    assert_eq!(output.status.code(), Some(1));
    assert!(output.stdout.is_empty());
    let error = String::from_utf8(output.stderr).unwrap();
    assert!(error.contains("architecture") && error.contains("unknown"), "{error}");
}

#[test]
fn test_sim_al_runs_all_native_architectures() {
    for arch in ["ebpf", "psa", "v1model"] {
        for encoding in [None, Some("arena-relative"), Some("arena-independent")] {
            let mut command = sim_command(arch);
            if let Some(encoding) = encoding {
                command.args(["--plugin-encoding", encoding]);
            }
            let output = command
                .arg("--stf")
                .arg(repo().join(format!("p4spec/test/micro/sim-{arch}/{arch}.stf")))
                .arg("-i")
                .arg(fixture("cli/run/first"))
                .output()
                .unwrap();
            assert!(output.status.success(), "{arch}: {}", String::from_utf8_lossy(&output.stderr));
            assert!(after_spec_warning(&output.stderr).is_empty());
            let stdout = String::from_utf8(output.stdout).unwrap();
            let expected = std::fs::read_to_string(
                repo().join(format!("p4spec/test/micro/micro_sim_{arch}_al.expected")),
            )
            .unwrap();
            let mut lines: Vec<_> = expected
                .lines()
                .filter(|line| line.starts_with("[PASS] Transmitted "))
                .collect();
            lines.push("passed");
            assert_eq!(stdout, format!("{}\n", lines.join("\n")), "{arch}");
        }
    }
}

#[test]
fn test_sim_interpreters_distinguish_p4_syntax_and_runtime_failures() {
    for stage in ["--al", "--sl", "--pl"] {
        for (program, category) in [
            ("cli/run/invalid.p4", "syntax error:"),
            (
                "cli/run/empty.p4",
                "error[runtime/binding-undefined]: relation `EBPF_init` is undefined",
            ),
        ] {
            let output = binary()
                .args(["sim", stage])
                .arg(fixture("cli/run/types.watsup"))
                .arg(fixture("cli/run/relations.watsup"))
                .args(["--arch", "ebpf", "-p"])
                .arg(fixture(program))
                .args(["--stf", "missing.stf"])
                .output()
                .unwrap();
            assert_eq!(output.status.code(), Some(1));
            assert!(output.stdout.is_empty());
            let error = String::from_utf8(output.stderr).unwrap();
            assert!(error.starts_with(category), "{stage}: {error}");
        }
    }
}

#[test]
fn test_sim_al_reports_stf_failures_and_preserves_prior_matches() {
    let text = std::fs::read_to_string(repo().join("p4spec/test/micro/sim-ebpf/ebpf.stf")).unwrap();
    let packet = text
        .lines()
        .rfind(|line| line.starts_with("packet "))
        .unwrap();
    let text_mismatch = format!("{text}\n{packet}\nexpect 0 FF\n");
    for (name, text, detail, matches) in [
        ("syntax", "@\n", "invalid character '@'", 0),
        ("mismatch", text_mismatch.as_str(), "expected (0) FF but got (0)", 2),
    ] {
        let path =
            std::env::temp_dir().join(format!("p4spec-cli-sim-{name}-{}.stf", std::process::id()));
        std::fs::write(&path, text).unwrap();
        let output = sim_command("ebpf").arg("--stf").arg(&path).output();
        std::fs::remove_file(&path).unwrap();
        let output = output.unwrap();
        assert_eq!(output.status.code(), Some(1), "{name}");
        let error = after_spec_warning(&output.stderr);
        assert!(error.starts_with("runtime error:"), "{name}: {error}");
        assert!(error.contains(detail), "{name}: {error}");
        let stdout = String::from_utf8(output.stdout).unwrap();
        assert_eq!(stdout.lines().count(), matches, "{name}: {stdout}");
        assert!(
            stdout
                .lines()
                .all(|line| line.starts_with("[PASS] Transmitted "))
        );
    }
}

#[test]
fn test_elab_command_renders_declaration_locations() {
    let path =
        std::env::temp_dir().join(format!("p4spec-cli-declaration-{}.watsup", std::process::id()));
    std::fs::write(&path, "dec $f : nat\ndec $f : nat\n").unwrap();
    let output = binary().arg("elab").arg(&path).output().unwrap();
    std::fs::remove_file(path).unwrap();
    assert_eq!(output.status.code(), Some(1));
    let text = String::from_utf8(output.stderr).unwrap();
    assert!(text.contains("error[elab/function-repeated]"), "{text}");
    assert!(text.contains("first declaration"), "{text}");
}

#[test]
fn test_transformation_commands_keep_warnings_on_success() {
    let path =
        std::env::temp_dir().join(format!("p4spec-cli-warning-{}.watsup", std::process::id()));
    std::fs::write(&path, "dec $missing : nat\n").unwrap();
    for command in ["elab", "algo", "struct", "prose"] {
        let output = binary().arg(command).arg(&path).output().unwrap();
        assert!(output.status.success());
        assert!(!output.stdout.is_empty());
        let text = String::from_utf8(output.stderr).unwrap();
        assert_eq!(
            text.matches("warning[elab/function-clause-missing]")
                .count(),
            1,
            "{text}"
        );
    }
    std::fs::remove_file(path).unwrap();
}

#[test]
fn test_transformation_commands_keep_committed_warnings_before_failure() {
    let path = std::env::temp_dir()
        .join(format!("p4spec-cli-warning-before-error-{}.watsup", std::process::id()));
    std::fs::write(&path, "relation R: nat |- nat\ndef $missing = 0\n").unwrap();
    for command in ["elab", "algo", "struct", "prose"] {
        let output = binary().arg(command).arg(&path).output().unwrap();
        assert_eq!(output.status.code(), Some(1));
        assert!(output.stdout.is_empty());
        let text = String::from_utf8(output.stderr).unwrap();
        let pos_warning = text
            .find("warning[elab/relation-input-hint-missing]")
            .expect("render the committed declaration warning");
        let pos_error = text
            .find("error[elab/function-declaration-required]")
            .expect("render the later declaration failure");
        assert!(pos_warning < pos_error, "{text}");
        assert!(!text.contains("elab/relation-rule-missing"), "{text}");
    }
    std::fs::remove_file(path).unwrap();
}

#[test]
fn test_transformation_commands_keep_warnings_before_algorithmic_failure() {
    let path = std::env::temp_dir()
        .join(format!("p4spec-cli-warning-algo-error-{}.watsup", std::process::id()));
    std::fs::write(&path, "dec $missing : nat\n").unwrap();
    for command in ["algo", "struct", "prose"] {
        let output = binary()
            .arg(command)
            .arg(&path)
            .arg(fixture("algorithmic/impure_else_premises.watsup"))
            .output()
            .unwrap();
        assert_eq!(output.status.code(), Some(1));
        assert!(output.stdout.is_empty());
        let text = String::from_utf8(output.stderr).unwrap();
        let pos_warning = text
            .find("warning[elab/function-clause-missing]")
            .expect("render the elaboration warning");
        let pos_error = text
            .find("error[algo/otherwise-condition-invalid]")
            .expect("render the algorithmic failure");
        assert!(pos_warning < pos_error, "{text}");
    }
    std::fs::remove_file(path).unwrap();
}

#[test]
fn test_structuring_failures_render_source_locations() {
    for command in ["struct", "prose"] {
        let output = binary()
            .arg(command)
            .arg(fixture("structure/generic-subtype.watsup"))
            .output()
            .unwrap();
        assert_eq!(output.status.code(), Some(1));
        assert!(output.stdout.is_empty());
        let text = String::from_utf8(output.stderr).unwrap();
        assert!(text.contains("error[structure/type-operation-invalid]"), "{text}");
        assert!(text.contains("generic-subtype.watsup:4:14"), "{text}");
        assert!(text.contains("-- if T <: T"), "{text}");
        assert!(text.contains("^"), "{text}");
    }
}

#[test]
fn test_prose_and_splice_render_hint_reports_after_warnings() {
    let path =
        std::env::temp_dir().join(format!("p4spec-cli-prose-hint-{}.watsup", std::process::id()));
    std::fs::write(&path, "dec $missing : nat\nsyntax record = RECORD nat\n  hint(prose_fields \"first\" \"extra\")\n").unwrap();
    for command in ["prose", "splice"] {
        let mut process = binary();
        process.arg(command).arg(&path);
        if command == "splice" {
            process.args(["--splice", "input.adoc", "--out", "output.adoc"]);
        }
        let output = process.output().unwrap();
        assert_eq!(output.status.code(), Some(1));
        assert!(output.stdout.is_empty());
        let text = String::from_utf8(output.stderr).unwrap();
        let pos_warning = text
            .find("warning[elab/function-clause-missing]")
            .expect("retain warning");
        let pos_error = text
            .find("error[prose/field-hint-arity-mismatch]")
            .expect("render prose report");
        assert!(pos_warning < pos_error, "{text}");
        assert!(text.contains("hint(prose_fields \"first\" \"extra\")"), "{text}");
        assert!(text.contains("syntax case declared here"), "{text}");
    }
    std::fs::remove_file(path).unwrap();
}
