//! Source-driven simulator rejection
//!
//! Architecture admission loads a registered SpecTec source before building.
//! P4 assertion cases use the full declared specification and include directory,
//! then evaluate Program_ok through the production runner with dummy externs.

use std::path::Path;

use p4spectec::lang::data::encoding::Encoding;

use p4spectec::diagnostic::{Report, ReportKind};

use p4spectec::runner::{self, Config, RunError, Spec};

use p4spectec::sim_plugin;

use crate::Result;

use super::{Case, failure};

/// Loads and compares this module's diagnostic fixtures.
pub fn run(path: &Path, path_cli: Option<&Path>) -> Result<()> {
    let groups = super::load(path, false)?;
    super::run_registered(
        "sim",
        &groups,
        path_cli,
        Some(run_case),
        p4spectec::diagnostic::RenderConfig {
            frame_style: Some(p4spectec::diagnostic::DisplayStyle::Short),
            ..Default::default()
        },
    )
}

/// Requires simulator admission or source program evaluation to reject.
fn run_case(case: &Case) -> Result<Vec<Report>> {
    let path = case.path_input();
    let config = Config::new(true, false, false);
    // A source specification exercises architecture admission normally
    if path.extension().is_some_and(|ext| ext == "watsup") {
        let [arch] = case.args.as_slice() else {
            return Err(failure(
                &case.name,
                "architecture case requires one architecture argument",
            ));
        };
        let spec_al = p4spectec::algo(&[path]).map_err(|report| {
            failure(&case.name, format!("conversion failed before simulator admission: {report}"))
        })?;
        let report = sim_plugin::build(Spec::Al(spec_al), arch, config, Encoding::default())
            .err()
            .ok_or_else(|| failure(&case.name, "simulator accepted negative architecture"))?;
        return Ok(vec![*report]);
    }
    let paths = case.paths_input();
    let [path_spec, path_include] = paths.as_slice() else {
        return Err(failure(&case.name, "simulation requires specification and include inputs"));
    };
    // Parsing and loading failures must not count as assertion failures
    let spec_al = p4spectec::algo(std::slice::from_ref(path_spec)).map_err(|report| {
        failure(&case.name, format!("conversion failed before simulation: {report}"))
    })?;
    let report = match runner::run(
        Spec::Al(spec_al),
        config,
        "Program_ok",
        std::slice::from_ref(path_include),
        &path,
    ) {
        Err(RunError::Eval(error)) => error.into_report(),
        Err(error) => {
            return Err(failure(&case.name, format!("setup failed before simulation: {error}")));
        }
        Ok(()) => return Err(failure(&case.name, "simulation accepted negative source")),
    };
    // Require the actual simulator assertion diagnostic beneath runtime frames
    let mut pending = vec![report.as_ref()];
    let mut assertion_failed = false;
    while let Some(report_inner) = pending.pop() {
        if let ReportKind::Cause(diagnostic) = &report_inner.kind
            && diagnostic.code.as_deref() == Some("sim/assertion-unmet")
        {
            assertion_failed = true;
            break;
        }
        pending.extend(&report_inner.children);
    }
    if !assertion_failed {
        return Err(failure(&case.name, "missing simulator assertion failure"));
    }
    Ok(vec![*report])
}
