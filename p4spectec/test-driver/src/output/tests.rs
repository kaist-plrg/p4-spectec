//! Descriptor restoration in isolated test processes

use std::{io::Write, process::Command};

use super::SilenceStdout;

/// Writes buffered output under suppression and exercises each cleanup path.
fn suppress(mode: &str) -> std::io::Result<()> {
    let guard_stdout = SilenceStdout::new()?;
    print!("hidden-buffered-output");
    eprintln!("visible-stderr");
    match mode {
        "finish" => guard_stdout.finish(),
        "error" => Err(std::io::Error::other("test execution failed")),
        "panic" => panic!("test execution panicked"),
        _ => unreachable!(),
    }
}

#[test]
fn restores_stdout_on_every_exit_path() {
    // Run descriptor mutations in a child with the Rust output capture disabled
    if let Ok(mode) = std::env::var("P4SPECTEC_STDOUT_TEST_MODE") {
        print!("visible-before");
        let result = std::panic::catch_unwind(|| suppress(&mode));
        match mode.as_str() {
            "finish" => assert!(result.unwrap().is_ok()),
            "error" => assert!(result.unwrap().is_err()),
            "panic" => assert!(result.is_err()),
            _ => unreachable!(),
        }
        print!("visible-after");
        std::io::stdout().flush().unwrap();
        return;
    }

    // Missing flushes or restoration must be observable at the process boundary
    for mode in ["finish", "error", "panic"] {
        let output = Command::new(std::env::current_exe().unwrap())
            .args(["--exact", "output::tests::restores_stdout_on_every_exit_path", "--nocapture"])
            .env("P4SPECTEC_STDOUT_TEST_MODE", mode)
            .output()
            .unwrap();
        let text_stdout = String::from_utf8(output.stdout).unwrap();
        let text_stderr = String::from_utf8(output.stderr).unwrap();
        assert!(output.status.success(), "{mode}: {text_stdout}\n{text_stderr}");
        assert!(text_stdout.contains("visible-beforevisible-after"), "{mode}: {text_stdout}");
        assert!(!text_stdout.contains("hidden-buffered-output"), "{mode}: {text_stdout}");
        assert!(text_stderr.contains("visible-stderr"), "{mode}: {text_stderr}");
    }
}
