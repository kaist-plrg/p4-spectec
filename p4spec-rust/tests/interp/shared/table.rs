//! Table outcomes preserve mismatches, fatal failures, and function tail calls

use crate::interp::report::ReportExt;
use p4spec_rust::{
    interp::shared::backtrack::Failure,
    lang::{al::ast as al, data::value::get, pl::ast as pl, sl::ast as sl},
    pass::{algo, elaborate, prosify, structure},
    runner::{self, BuiltinInterface, Config, Interpreter, NullExtern, Runner},
};

/// Compiles complete table patterns with independently supplied row bodies.
fn specs(body_a: &str, body_b: &str, defs: &str) -> (al::Spec, sl::Spec, pl::Spec) {
    let source = format!(
        r#"
syntax choice =
  | A
  | B
{defs}
tbl dec $table(choice) : bool
tbl def $table =
  | A => {body_a}
  | B => {body_b}
dec $entry() : bool
def $entry() = $table(B)
"#
    );
    let spec_el = crate::spec_fixture::parse(&source).unwrap();
    let spec_il = elaborate::convert(spec_el).unwrap();
    let spec_al = algo::convert(spec_il).unwrap();
    let spec_sl = structure::convert(spec_al.clone(), false).unwrap();
    let spec_pl = prosify::convert(spec_sl.clone()).unwrap();
    (spec_al, spec_sl, spec_pl)
}

/// Calls the source entry and observes its boolean result or failure kind.
fn invoke<Interp>(mut runner: Runner<Interp, BuiltinInterface, NullExtern>) -> Result<bool, Failure>
where
    Interp: Interpreter<BuiltinInterface, NullExtern, Error = Failure>,
{
    let value = runner.context().call_func("entry", &[], &[])?;
    Ok(get::bool(runner.arena(), &value).unwrap())
}

/// Runs the corresponding source programs through all interpreter stages.
fn outcomes(specs: (al::Spec, sl::Spec, pl::Spec)) -> Vec<Result<bool, Failure>> {
    let (spec_al, spec_sl, spec_pl) = specs;
    let config = Config::new(true, false, true);
    vec![
        invoke(runner::build_al(spec_al, config, NullExtern).unwrap()),
        invoke(runner::build_sl(spec_sl, config, NullExtern).unwrap()),
        invoke(runner::build_pl(spec_pl, config, NullExtern).unwrap()),
    ]
}

#[test]
fn matching_table_rows_return_their_values() {
    for outcome in outcomes(specs("true", "false", "")) {
        assert!(!outcome.unwrap());
    }
}

#[test]
fn final_table_rows_forward_function_tail_calls() {
    let defs = "dec $answer() : bool\ndef $answer() = true";
    for outcome in outcomes(specs("false", "$answer()", defs)) {
        assert!(outcome.unwrap());
    }
}

#[test]
fn rejected_table_rows_remain_recoverable_mismatches() {
    let defs = "dec $reject() : bool\ndef $reject() = true\n  -- if false";
    for outcome in outcomes(specs("false", "$reject()", defs)) {
        let failure = outcome.unwrap_err();
        assert!(matches!(failure, Failure::Mismatch(_)), "{failure:?}");
        assert!(
            failure
                .into_report()
                .find_code("runtime/condition-unmet")
                .is_some()
        );
    }
}

#[test]
fn fatal_table_rows_preserve_the_original_failure() {
    let defs = "extern dec $unavailable() : bool";
    for outcome in outcomes(specs("false", "$unavailable()", defs)) {
        let failure = outcome.unwrap_err();
        assert!(matches!(failure, Failure::Fatal(_)), "{failure:?}");
        assert!(
            failure
                .into_report()
                .find_code("runtime/extern-unconfigured")
                .is_some()
        );
    }
}

#[test]
fn empty_table_ir_remains_a_recoverable_mismatch() {
    let (mut spec_al, mut spec_sl, mut spec_pl) = specs("false", "true", "");
    // Empty rows cannot pass AL pattern coverage, so exercise the evaluator IR
    for def in &mut spec_al {
        if let al::DefKind::MetaFunc(al::MetaFuncDef::Table(func)) = &mut def.node {
            func.table_rows.clear();
        }
    }
    for def in &mut spec_sl {
        if let sl::DefKind::MetaFunc(sl::MetaFuncDef::Table(func)) = &mut def.node {
            func.table_rows.clear();
        }
    }
    for def in &mut spec_pl {
        if let pl::DefKind::MetaFunc(pl::MetaFuncDef::Table(func)) = &mut def.node.node {
            func.rows.clear();
        }
    }
    for outcome in outcomes((spec_al, spec_sl, spec_pl)) {
        let failure = outcome.unwrap_err();
        assert!(matches!(failure, Failure::Mismatch(_)), "{failure:?}");
    }
}

#[test]
fn relation_results_in_table_ir_violate_the_function_invariant() {
    let defs = "relation R: bool |- bool\n  hint(input %0)\nrule R/one: true |- true";
    let (_, mut spec_sl, _) = specs("false", "true", defs);
    let rel_signature = spec_sl
        .iter()
        .find_map(|def| match &def.node {
            sl::DefKind::Rel(sl::RelDef::Defined(rel)) => Some(rel.rel_signature.clone()),
            _ => None,
        })
        .unwrap();
    // Replace a function conclusion with an impossible relation conclusion
    for def in &mut spec_sl {
        if let sl::DefKind::MetaFunc(sl::MetaFuncDef::Table(func)) = &mut def.node {
            let row = func.table_rows.last_mut().unwrap();
            row.block = vec![p4spec_rust::phrase! {
                node: sl::InstrKind::Result(sl::ResultInstr {
                    rel_signature: rel_signature.clone(),
                    exps: vec![row.exp.clone()],
                }),
                span: Default::default(),
            }];
        }
    }
    let spec_pl = prosify::convert(spec_sl.clone()).unwrap();
    let config = Config::new(false, false, true);
    macro_rules! check {
        ($build:ident, $spec:expr) => {{
            let runner = runner::$build($spec, config, NullExtern).unwrap();
            let panic = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| invoke(runner)))
                .expect_err("relation conclusions cannot occur in function tables");
            let message = panic
                .downcast_ref::<String>()
                .map(String::as_str)
                .or_else(|| panic.downcast_ref::<&str>().copied())
                .unwrap();
            assert!(message.contains("relation flow in table body"), "{message}");
        }};
    }
    check!(build_sl, spec_sl);
    check!(build_pl, spec_pl);
}
