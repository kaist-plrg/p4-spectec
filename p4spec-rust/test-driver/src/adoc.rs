use std::{fs, path::Path, time::Instant};

use expect_test::expect_file;

use p4spec_rust::diagnostic::{RenderConfig, Renderer};

use p4spec_rust::frontend::parse::parse_files;

use p4spec_rust::pass::elaborate;

use p4spec_rust::pass::algo;

use p4spec_rust::pass::structure;

use p4spec_rust::pass::prosify;

use p4spec_rust::specdoc::adoc;

use crate::{
    Error, Result, snapshot,
    suite::{self, Registry, Stage},
};

/// Checks full-specification snapshots and optionally exports AsciiDoc documents.
pub fn run(registry: &Registry, path_output: Option<&Path>) -> Result<()> {
    let start = Instant::now();
    let snapshots_el = registry.snapshots(Stage::AdocEl);
    let snapshots_pl = registry.snapshots(Stage::AdocPl);
    if snapshots_el.is_empty() && snapshots_pl.is_empty() {
        return Err(Error::Invalid("no AsciiDoc snapshots registered".into()));
    }
    let mut num_bytes_el = 0;
    let mut num_bytes_pl = 0;
    // Render each registered source snapshot with its own input files
    for snapshot in snapshots_el {
        let spec_el =
            parse_files(&snapshot.inputs).map_err(|error| Error::Invalid(error.to_string()))?;
        let text_el = spec_el
            .iter()
            .map(adoc::el::render_def)
            .collect::<Vec<_>>()
            .join("\n\n");
        num_bytes_el += text_el.len();
        if let Some(path_output) = path_output {
            fs::create_dir_all(path_output)?;
            fs::write(path_output.join("el.adoc"), &text_el)?;
        }
        let path_expected = suite::expected_path(&snapshot.expected);
        snapshot::check(expect_file![path_expected], &text_el);
    }
    // Preserve the registered rule-group mode while producing annotated PL
    for snapshot in snapshots_pl {
        let spec_el =
            parse_files(&snapshot.inputs).map_err(|error| Error::Invalid(error.to_string()))?;
        let spec_il =
            elaborate::convert(spec_el).map_err(|error| Error::Invalid(error.to_string()))?;
        let spec_al = algo::convert(spec_il).map_err(|error| Error::Invalid(error.to_string()))?;
        let spec_sl = structure::convert(spec_al, snapshot.without_rule_groups)
            .map_err(|error| Error::Invalid(error.to_string()))?;
        let spec_pl =
            prosify::convert(spec_sl).map_err(|error| Error::Invalid(error.to_string()))?;

        // Repeated rendering must start with fresh document anchor state
        let mut warnings = Vec::new();
        let text_pl = adoc::pl::render_spec(&mut warnings, &spec_pl);
        for report in warnings {
            let text = Renderer::new(RenderConfig::default())
                .render_to_string(&report)
                .map_err(|error| Error::Invalid(error.to_string()))?;
            eprint!("{text}");
        }
        if text_pl != adoc::pl::render_spec(&mut Vec::new(), &spec_pl) {
            return Err(Error::Invalid(
                "AsciiDoc arm anchors changed on repeated rendering".into(),
            ));
        }
        num_bytes_pl += text_pl.len();
        // Save raw fragments for comparison and Asciidoctor validation
        if let Some(path_output) = path_output {
            fs::create_dir_all(path_output)?;
            fs::write(path_output.join("pl.adoc"), &text_pl)?;
        }
        let path_expected = suite::expected_path(&snapshot.expected);
        snapshot::check(expect_file![path_expected], &text_pl);
    }
    eprintln!(
        "adoc: specification snapshots checked, EL={num_bytes_el} bytes, PL={num_bytes_pl} bytes, elapsed={:.3}s",
        start.elapsed().as_secs_f64(),
    );
    Ok(())
}
