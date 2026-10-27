//! Specification snapshots of the `spec` directory
//!
//! `elab`, `algo`, `structure`, and `prose` print each converted specification;
//! `adoc` renders EL and PL AsciiDoc and checks that PL anchors are deterministic.
//! `check` compares each text with `expected/snapshot/<name>.expected`.

use std::{fs, path::Path};

use expect_test::expect_file;

use p4spectec::lang::traits::print::Print;

use p4spectec::diagnostic::{RenderConfig, Renderer};

use p4spectec::specdoc::adoc;

use crate::{Error, Result, util};

/// Checks the elaborated specification.
pub fn elab() -> Result<()> {
    let spec_il = p4spectec::elab(&util::spec())?;
    check("elab", &(Print::to_string(&spec_il) + "\n"));
    Ok(())
}

/// Checks the algorithmic specification.
pub fn algo() -> Result<()> {
    let spec_al = p4spectec::algo(&util::spec())?;
    check("algo", &(Print::to_string(&spec_al) + "\n"));
    Ok(())
}

/// Checks the structured specification without and with rule groups.
pub fn structure() -> Result<()> {
    for (name, without_rule_groups) in
        [("structure-without-rule-groups", true), ("structure-with-rule-groups", false)]
    {
        let spec_sl = p4spectec::structure(&util::spec(), without_rule_groups)?;
        check(name, &(Print::to_string(&spec_sl) + "\n"));
    }
    Ok(())
}

/// Checks the prose specification.
pub fn prose() -> Result<()> {
    let spec_pl = p4spectec::prosify(&util::spec())?;
    check("prose", &(Print::to_string(&spec_pl) + "\n"));
    Ok(())
}

/// Checks EL and PL AsciiDoc and optionally saves both documents.
pub fn adoc(path_output: Option<&Path>) -> Result<()> {
    let spec_el = p4spectec::parse(util::spec())?;
    let text_el = spec_el
        .iter()
        .map(adoc::el::render_def)
        .collect::<Vec<_>>()
        .join("\n\n");
    let spec_pl = p4spectec::prosify(&util::spec())?;
    let mut warnings = Vec::new();
    let text_pl = adoc::pl::render_spec(&mut warnings, &spec_pl);
    for report in warnings {
        let text = Renderer::new(RenderConfig::default()).render_to_string(&report)?;
        eprint!("{text}");
    }
    // Repeated rendering must start with fresh document anchor state
    if text_pl != adoc::pl::render_spec(&mut Vec::new(), &spec_pl) {
        return Err(Error::from("AsciiDoc arm anchors changed on repeated rendering"));
    }
    // Save raw fragments for Asciidoctor validation
    if let Some(path_output) = path_output {
        fs::create_dir_all(path_output)?;
        fs::write(path_output.join("el.adoc"), &text_el)?;
        fs::write(path_output.join("pl.adoc"), &text_pl)?;
    }
    check("adoc-el", &text_el);
    check("adoc-pl", &text_pl);
    Ok(())
}

/// Compares `text` with `expected/snapshot/<name>.expected`.
///
/// Renderers leave decorative spaces after declarations and rule-group headers,
/// so trailing spaces and tabs are stripped from every line first.
fn check(name: &str, text: &str) {
    let text = text
        .split('\n')
        .map(|line| line.trim_end_matches([' ', '\t']))
        .collect::<Vec<_>>()
        .join("\n");
    let path_expected =
        util::expected_path(Path::new(&format!("expected/snapshot/{name}.expected")));
    expect_file![path_expected].assert_eq(&text);
    eprintln!("{name}: snapshot checked");
}
