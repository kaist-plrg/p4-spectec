//! Program instantiation through the AL metacircular specification
//!
//! The standalone driver exercises the metacirc CLI with the Rust SL backend,
//! the AL specification, the P4 specification, and one P4 program.
//! A successful synthetic entry is returned only after Program_inst succeeds.

use std::{error::Error, path::Path, process::Command};

/// The result of the metacircular execution check.
pub type Result<T> = std::result::Result<T, Box<dyn Error>>;

/// Checks the SL → AL specification → P4 specification → Program_inst path.
pub fn run(path_cli: &Path) -> Result<()> {
    let path_root = Path::new(env!("CARGO_MANIFEST_DIR")).join("metacirc");
    let output = Command::new(path_cli)
        .arg("tower")
        .arg(path_root.join("program-inst.json"))
        .arg("-p")
        .arg(path_root.join("program-inst.p4"))
        .output()?;
    if !output.status.success() {
        return Err(format!(
            "metacirc Program_inst failed: {}\n{}\n{}",
            output.status,
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr),
        )
        .into());
    }
    if !String::from_utf8_lossy(&output.stdout)
        .lines()
        .any(|line| line == "passed")
    {
        return Err("metacirc did not report successful Program_inst execution".into());
    }
    eprintln!("metacirc: SL > AL spec > P4 spec > P4 program: Program_inst passed");
    Ok(())
}
