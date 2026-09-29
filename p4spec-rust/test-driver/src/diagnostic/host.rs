//! Pinned extern diagnostics through returning AL, SL, and PL calls
//!
//! Synthetic hosts return structured failures through the production extern API.
//! Successful semantic preservation checks have empty rendered expectations.

use super::failure;
use crate::Result;
use p4spec_rust::{
    diagnostic::{Diagnostic, Label, Report, ReportKind, Severity},
    interp::shared::backtrack::Failure,
    lang::{
        common::source::{Position, Span},
        data::value::{Value, make},
        il::ast::Typ,
    },
    runner::{
        self, BuiltinInterface, Config, Extern, ExternError, Interface, Interpreter, Runner,
        RunnerContext,
    },
};

struct Host {
    mismatch: bool,
}

/// Constructs the complete pinned fatal payload, including related data.
fn diagnostic() -> Report {
    let span = Span {
        left: Position::new("external.p4", 4, 2),
        right: Position::new("external.p4", 4, 5),
    };
    Report::from(Diagnostic::new(
        "sim",
        Severity::Error,
        Some("sim/test".to_owned()),
        "external diagnostic",
        vec![
            Label::primary(&span, "external location"),
            Label::secondary(&Span::default(), "external related item"),
        ],
        vec!["external detail".to_owned()],
    ))
    .with_children(vec![Report::frame(
        Span::default(),
        "external trace root",
        vec![Report::frame(Span::default(), "external trace leaf", vec![])],
    )])
}

/// Constructs the ordered nested mismatch payload from the OCaml pin.
fn traces() -> Vec<Report> {
    vec![Report::frame(
        Span::default(),
        "external relation failed",
        vec![Report::frame(Span::default(), "external leaf", vec![])],
    )]
}

impl Host {
    fn error(&self) -> ExternError {
        if self.mismatch {
            ExternError::Mismatch(traces())
        } else {
            ExternError::Report(Box::new(diagnostic()))
        }
    }
}

impl Extern for Host {
    fn eval_rel<Interp, Iface>(
        &self,
        _ctx: &mut RunnerContext<'_, Interp, Iface, Self>,
        _name: &str,
        _values: &[Value],
    ) -> std::result::Result<(Vec<Value>, bool), Interp::Error>
    where
        Iface: Interface,
        Interp: Interpreter<Iface, Self>,
    {
        Err(self.error().into())
    }

    fn eval_func<Interp, Iface>(
        &self,
        _ctx: &mut RunnerContext<'_, Interp, Iface, Self>,
        _name: &str,
        _targs: &[Typ],
        _values: &[Value],
    ) -> std::result::Result<(Value, bool), Interp::Error>
    where
        Iface: Interface,
        Interp: Interpreter<Iface, Self>,
    {
        Err(self.error().into())
    }

    fn clear(&mut self) {}
}

/// Compares every semantic field and ordered child without rendering.
pub(super) fn reports_equal(report_a: &Report, report_b: &Report) -> bool {
    let same = match (&report_a.kind, &report_b.kind) {
        (
            ReportKind::Frame { span: span_a, message: message_a },
            ReportKind::Frame { span: span_b, message: message_b },
        ) => span_a == span_b && message_a == message_b,
        (ReportKind::Cause(cause_a), ReportKind::Cause(cause_b)) => {
            cause_a.severity == cause_b.severity
                && cause_a.code == cause_b.code
                && cause_a.message == cause_b.message
                && cause_a.labels == cause_b.labels
                && cause_a.notes == cause_b.notes
                && cause_a.source == cause_b.source
        }
        _ => false,
    };
    // Child order is part of host transport
    same && report_a.children.len() == report_b.children.len()
        && report_a
            .children
            .iter()
            .zip(&report_b.children)
            .all(|(report_a, report_b)| reports_equal(report_a, report_b))
}

/// Finds the original host node beneath interpreter context frames.
fn contains(report: &Report, report_expect: &Report) -> bool {
    reports_equal(report, report_expect)
        || report
            .children
            .iter()
            .any(|report| contains(report, report_expect))
}

/// Checks the returning failure kind before the final output conversion.
fn check<Interp>(
    name: &str,
    mut runner: Runner<Interp, BuiltinInterface, Host>,
    mismatch: bool,
) -> Result<Vec<Report>>
where
    Interp: Interpreter<BuiltinInterface, Host, Error = Failure>,
{
    let value = make::nat(runner.arena_mut(), 1.into(), Span::default())
        .map_err(|error| failure(name, error))?;
    let error = runner
        .context()
        .call_rel("R", &[value])
        .err()
        .ok_or_else(|| failure(name, "extern unexpectedly succeeded"))?;
    // Inspect fatal fields and mismatch trees at the returning product boundary
    match &error {
        Failure::Fatal(report) if !mismatch => {
            if !contains(report, &diagnostic()) {
                return Err(failure(name, "extern diagnostic fields changed"));
            }
            Ok(vec![])
        }
        Failure::Mismatch(reports) if mismatch => {
            if !traces()
                .iter()
                .all(|report_expect| reports.iter().any(|report| contains(report, report_expect)))
            {
                return Err(failure(name, "extern mismatch trace changed"));
            }
            Ok(vec![*error.into_report()])
        }
        _ => Err(failure(name, "extern fatal/mismatch identity changed")),
    }
}

/// Runs each pinned extern case with caching enabled through its real stage.
pub(super) fn run(name: &str) -> Result<Vec<Report>> {
    let mismatch = name.ends_with("-failtraces");
    let config = Config::new(true, false, false);
    let path = "interp/external.watsup";
    // Preceding pass failures must never count as transport acceptance
    if name.starts_with("interp-al-") {
        let spec_al = p4spec_rust::algo([path]).map_err(|error| failure(name, error))?;
        let runner = runner::build_al(spec_al, config, Host { mismatch })
            .map_err(|error| failure(name, error))?;
        check(name, runner, mismatch)
    } else if name.starts_with("interp-sl-") {
        let spec_sl = p4spec_rust::structure([path], true).map_err(|error| failure(name, error))?;
        let runner = runner::build_sl(spec_sl, config, Host { mismatch })
            .map_err(|error| failure(name, error))?;
        check(name, runner, mismatch)
    } else if name.starts_with("interp-pl-") {
        let spec_pl = p4spec_rust::prosify([path]).map_err(|error| failure(name, error))?;
        let runner = runner::build_pl(spec_pl, config, Host { mismatch })
            .map_err(|error| failure(name, error))?;
        check(name, runner, mismatch)
    } else {
        Err(failure(name, "unknown host stage"))
    }
}
