use crate::interp::report::ReportExt;
use p4spec_rust::{
    diagnostic::Report,
    lang::{common::source::Span, data::value::make},
    pass::{algo, elaborate, prosify, structure},
    runner::{self, BuiltinInterface, Config, Interpreter, NullExtern, Runner},
};

fn invoke<Interp>(mut runner: Runner<Interp, BuiltinInterface, NullExtern>) -> Report
where
    Interp: Interpreter<BuiltinInterface, NullExtern>,
{
    let value = make::nat(runner.arena_mut(), 1.into(), Span::default()).unwrap();
    let failure = runner.context().call_rel("R", &[value]).unwrap_err();
    *failure.into_report()
}

fn failures(source: &str) -> Vec<Report> {
    let spec_el = crate::spec_fixture::parse(source).unwrap();
    let spec_il = elaborate::convert(spec_el).unwrap();
    let spec_al = algo::convert(spec_il).unwrap();
    let spec_sl = structure::convert(spec_al.clone(), false).unwrap();
    let spec_pl = prosify::convert(spec_sl.clone()).unwrap();
    let config = Config::new(true, false, false);
    vec![
        invoke(runner::build_al(spec_al, config, NullExtern).unwrap()),
        invoke(runner::build_sl(spec_sl, config, NullExtern).unwrap()),
        invoke(runner::build_pl(spec_pl, config, NullExtern).unwrap()),
    ]
}

#[test]
fn failed_holds_keep_the_inner_relation_failure_in_every_stage() {
    for source in [
        include_str!("../../../test-driver/expected/diagnostic/interp/hold-failed.watsup"),
        include_str!("../../../test-driver/expected/diagnostic/interp/hold-iter-failed.watsup"),
    ] {
        for report in failures(source) {
            let report_hold = report.find_code("runtime/hold-condition-unmet").unwrap();
            assert!(report_hold.find_code("runtime/condition-unmet").is_some(), "{report:?}");
        }
    }
}

#[test]
fn failed_not_holds_have_no_inner_failure() {
    let source =
        include_str!("../../../test-driver/expected/diagnostic/interp/not-hold-failed.watsup");
    for report in failures(source) {
        let report_hold = report
            .find_code("runtime/not-hold-condition-unmet")
            .unwrap();
        assert!(report_hold.children.is_empty(), "{report:?}");
        assert!(report.find_code("runtime/condition-unmet").is_none(), "{report:?}");
    }
}

#[test]
fn fatal_hold_calls_do_not_become_condition_mismatches() {
    let source = include_str!("../../../test-driver/expected/diagnostic/interp/hold-fatal.watsup");
    for report in failures(source) {
        assert!(report.find_code("runtime/extern-unconfigured").is_some(), "{report:?}");
        assert!(report.find_code("runtime/hold-condition-unmet").is_none(), "{report:?}");
    }
}

#[test]
fn successful_not_hold_checks_do_not_leak_the_rejected_relation() {
    let source = r#"
var n : nat
relation Check: CHECK nat
  hint(input %0)
rule Check/zero: CHECK n
  -- if n = 0
relation R: nat |- nat
  hint(input %0)
rule R/not-hold: n |- $(n / 0)
  -- Check:/ CHECK n
"#;
    for report in failures(source) {
        assert!(report.find_code("runtime/numeric-invalid").is_some(), "{report:?}");
        assert!(report.find_code("runtime/condition-unmet").is_none(), "{report:?}");
        assert!(report.find_code("runtime/hold-condition-unmet").is_none(), "{report:?}");
    }
}

#[test]
fn choosing_a_not_hold_branch_discards_the_rejected_relation() {
    let source = r#"
var n : nat
relation Check: CHECK nat
  hint(input %0)
rule Check/zero: CHECK n
  -- if n = 0
dec $choose(nat) : nat
def $choose(n) = n
  -- Check: CHECK n
def $choose(n) = $(n / 0)
  -- Check:/ CHECK n
relation R: nat |- nat
  hint(input %0)
rule R/call: n |- $choose(n)
"#;
    for report in failures(source) {
        assert!(report.find_code("runtime/numeric-invalid").is_some(), "{report:?}");
        assert!(report.find_code("runtime/condition-unmet").is_none(), "{report:?}");
        assert!(report.find_code("runtime/hold-condition-unmet").is_none(), "{report:?}");
    }
}

#[test]
fn iterated_holds_stop_before_a_later_fatal_call() {
    let source = r#"
var n : nat
extern dec $unavailable() : bool
relation Check: CHECK nat
  hint(input %0)
rule Check/zero: CHECK 0
rule Check/later: CHECK 2
  -- if $unavailable()
dec $check(nat*) : nat
def $check(n*) = 0
  -- (Check: CHECK n)*
relation R: nat |- nat
  hint(input %0)
rule R/call: n |- $check([0, n, 2])
"#;
    for report in failures(source) {
        assert!(report.find_code("runtime/hold-condition-unmet").is_some(), "{report:?}");
        assert!(report.find_code("runtime/extern-unconfigured").is_none(), "{report:?}");
    }
}
