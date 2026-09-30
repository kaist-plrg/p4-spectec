//! Synthetic host payload transport through AL, SL, and PL

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

/// Constructs distinct ordered alternatives to catch reordering or loss.
fn traces() -> Vec<Report> {
    vec![
        Report::frame(
            Span::default(),
            "first alternative",
            vec![
                Report::frame(Span::default(), "first leaf", vec![]),
                Report::frame(Span::default(), "second leaf", vec![]),
            ],
        ),
        Report::frame(Span::default(), "second alternative", vec![]),
    ]
}

impl Host {
    fn error(&self) -> ExternError {
        if self.mismatch {
            Failure::Mismatch(traces()).into()
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
    ) -> std::result::Result<(Vec<Value>, bool), p4spec_rust::runner::ExternError>
    where
        Iface: Interface,
        Interp: Interpreter<Iface, Self>,
    {
        Err(self.error())
    }

    fn eval_func<Interp, Iface>(
        &self,
        _ctx: &mut RunnerContext<'_, Interp, Iface, Self>,
        _name: &str,
        _targs: &[Typ],
        _values: &[Value],
    ) -> std::result::Result<(Value, bool), p4spec_rust::runner::ExternError>
    where
        Iface: Interface,
        Interp: Interpreter<Iface, Self>,
    {
        Err(self.error())
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

/// Checks the entire host subtree beneath unary interpreter context frames.
fn check<Interp>(mut runner: Runner<Interp, BuiltinInterface, Host>, mismatch: bool)
where
    Interp: Interpreter<BuiltinInterface, Host, Error = Failure>,
{
    let value = make::nat(runner.arena_mut(), 1.into(), Span::default()).unwrap();
    let failure = runner.context().call_rel("R", &[value]).unwrap_err();
    let Failure::Fatal(report) = failure else { panic!("host failures must be fatal") };
    let report_expect = if mismatch {
        Report::frame(Span::default(), "execution failed", traces())
    } else {
        diagnostic()
    };
    let mut report = report.as_ref();
    while !reports_equal(report, &report_expect) {
        assert!(matches!(report.kind, ReportKind::Frame { .. }));
        assert_eq!(report.children.len(), 1, "interpreter context must not add or drop siblings");
        report = &report.children[0];
    }
    assert!(reports_equal(report, &report_expect));
}

#[test]
fn host_fatal_reports_preserve_all_fields_and_ordered_reentry_causes() {
    let source = r#"
var n : nat
var m : nat
extern relation Host: nat |- nat
  hint(input %0)
relation R: nat |- nat
  hint(input %0)
rule R/call: n |- m
  -- Host: n |- m
"#;
    let spec_el = crate::spec_fixture::parse(source).unwrap();
    let spec_il = p4spec_rust::pass::elaborate::convert(spec_el).unwrap();
    let spec_al = p4spec_rust::pass::algo::convert(spec_il).unwrap();
    let spec_sl = p4spec_rust::pass::structure::convert(spec_al.clone(), false).unwrap();
    let spec_pl = p4spec_rust::pass::prosify::convert(spec_sl.clone()).unwrap();
    for mismatch in [false, true] {
        let config = Config::new(true, false, false);
        check(runner::build_al(spec_al.clone(), config, Host { mismatch }).unwrap(), mismatch);
        check(runner::build_sl(spec_sl.clone(), config, Host { mismatch }).unwrap(), mismatch);
        check(runner::build_pl(spec_pl.clone(), config, Host { mismatch }).unwrap(), mismatch);
    }
}
