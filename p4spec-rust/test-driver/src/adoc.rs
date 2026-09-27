use crate::{Error, Result, snapshot};
use expect_test::expect_file;
use p4spec_rust::{
    backend_doc::adoc,
    frontend::parse::parse_files,
    pass::{algo, elaborate, prosify, structure},
};
use std::{fs, path::Path, time::Instant};

/// Checks full-specification snapshots and optionally exports AsciiDoc documents.
pub fn run(path_output: Option<&Path>) -> Result<()> {
    let start = Instant::now();
    // Render source definitions before elaboration consumes the EL specification
    let spec_el =
        parse_files([Path::new("spec")]).map_err(|error| Error::Invalid(error.to_string()))?;
    let text_el = spec_el
        .iter()
        .map(adoc::el::render_def)
        .collect::<Vec<_>>()
        .join("\n\n");
    // Preserve rule groups while producing the annotated PL specification
    let spec_il = elaborate::convert(spec_el).map_err(|error| Error::Invalid(error.to_string()))?;
    let spec_al = algo::convert(spec_il).map_err(|error| Error::Invalid(error.to_string()))?;
    let spec_sl =
        structure::convert(spec_al, false).map_err(|error| Error::Invalid(error.to_string()))?;
    let spec_pl = prosify::convert(spec_sl).map_err(|error| Error::Invalid(error.to_string()))?;

    // Rendering another document must not retain arm counters from the first
    let text_pl = adoc::pl::render_spec(&spec_pl);
    if text_pl != adoc::pl::render_spec(&spec_pl) {
        return Err(Error::Invalid("AsciiDoc arm anchors changed on repeated rendering".into()));
    }
    // Save raw fragments for comparison and Asciidoctor validation
    if let Some(path_output) = path_output {
        fs::create_dir_all(path_output)?;
        fs::write(path_output.join("el.adoc"), &text_el)?;
        fs::write(path_output.join("pl.adoc"), &text_pl)?;
    }
    // Keep native corpus acceptance independent of the OCaml toolchain
    let path_expected = Path::new(env!("CARGO_MANIFEST_DIR")).join("expected");
    snapshot::check(expect_file![path_expected.join("adoc-el.expected")], &text_el);
    snapshot::check(expect_file![path_expected.join("adoc-pl.expected")], &text_pl);
    eprintln!(
        "adoc: specification snapshots checked, EL={} bytes, PL={} bytes, elapsed={:.3}s",
        text_el.len(),
        text_pl.len(),
        start.elapsed().as_secs_f64(),
    );
    Ok(())
}
