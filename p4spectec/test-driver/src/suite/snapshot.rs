//! Specification snapshots registered in `suites/snapshot.toml`
//!
//! Each snapshot converts the registered `spec` sources to its `stage`
//! and compares the printed or rendered text with its `expected` file.
//! `adoc-pl` also checks that repeated rendering keeps the same anchors.

use std::{
    fs,
    path::{Path, PathBuf},
};

use expect_test::expect_file;
use serde::Deserialize;

use p4spectec::lang::traits::print::Print;

use p4spectec::diagnostic::{RenderConfig, Renderer};

use p4spectec::specdoc::adoc;

use crate::harness::{self, Error, Result};

/// Names the conversion a snapshot prints.
#[derive(Clone, Copy, Debug, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "kebab-case")]
pub enum Stage {
    Elab,
    Algo,
    Structure,
    Prose,
    AdocEl,
    AdocPl,
}

/// Registers one snapshot of the shared specification.
#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Snapshot {
    pub name: String,
    pub stage: Stage,
    pub expected: PathBuf,
    #[serde(default)]
    pub without_rule_groups: bool,
}

/// Supplies the specification sources and snapshots.
#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Registration {
    pub spec: Vec<PathBuf>,
    pub snapshots: Vec<Snapshot>,
}

/// Checks the registered snapshots of the given stages.
///
/// With `path_output`, AsciiDoc snapshots also save `el.adoc` and `pl.adoc` there.
pub fn run(stages: &[Stage], path_output: Option<&Path>) -> Result<()> {
    let registration: Registration = harness::load("snapshot")?;
    let spec = &registration.spec;
    for snapshot in &registration.snapshots {
        if !stages.contains(&snapshot.stage) {
            continue;
        }
        let text = match snapshot.stage {
            // Print each conversion, ending with a newline
            Stage::Elab => Print::to_string(&p4spectec::elab(spec)?) + "\n",
            Stage::Algo => Print::to_string(&p4spectec::algo(spec)?) + "\n",
            Stage::Structure => {
                let spec_sl = p4spectec::structure(spec, snapshot.without_rule_groups)?;
                Print::to_string(&spec_sl) + "\n"
            }
            Stage::Prose => Print::to_string(&p4spectec::prosify(spec)?) + "\n",
            // Render each EL definition as a separate AsciiDoc block
            Stage::AdocEl => p4spectec::parse(spec)?
                .iter()
                .map(adoc::el::render_def)
                .collect::<Vec<_>>()
                .join("\n\n"),
            // Render PL, reporting warnings and requiring deterministic anchors
            Stage::AdocPl => {
                let spec_pl = p4spectec::prosify(spec)?;
                let mut warnings = Vec::new();
                let text_pl = adoc::pl::render_spec(&mut warnings, &spec_pl);
                for report in warnings {
                    let text = Renderer::new(RenderConfig::default()).render_to_string(&report)?;
                    eprint!("{text}");
                }
                if text_pl != adoc::pl::render_spec(&mut Vec::new(), &spec_pl) {
                    return Err(Error::from("AsciiDoc arm anchors changed on repeated rendering"));
                }
                text_pl
            }
        };
        // Save raw AsciiDoc fragments for Asciidoctor validation
        let name_output = match snapshot.stage {
            Stage::AdocEl => Some("el.adoc"),
            Stage::AdocPl => Some("pl.adoc"),
            _ => None,
        };
        if let (Some(path_output), Some(name_output)) = (path_output, name_output) {
            fs::create_dir_all(path_output)?;
            fs::write(path_output.join(name_output), &text)?;
        }
        check(snapshot, &text)?;
    }
    Ok(())
}

/// Compares `text` with the snapshot's expected file.
///
/// Renderers leave decorative spaces after declarations and rule-group headers,
/// so trailing spaces and tabs are stripped from every line first.
fn check(snapshot: &Snapshot, text: &str) -> Result<()> {
    let text = text
        .split('\n')
        .map(|line| line.trim_end_matches([' ', '\t']))
        .collect::<Vec<_>>()
        .join("\n");
    expect_file![std::path::absolute(&snapshot.expected)?].assert_eq(&text);
    eprintln!("{}: snapshot checked", snapshot.name);
    Ok(())
}
