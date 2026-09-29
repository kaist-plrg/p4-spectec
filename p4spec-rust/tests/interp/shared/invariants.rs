use crate::interp::report::ReportExt;
use p4spec_rust::{
    interp::{al, pl, sl},
    pass::{algo, elaborate, prosify, structure},
};

#[test]
fn loading_duplicate_global_definitions_violates_the_ir_precondition() {
    for (source, kind) in [
        ("syntax foo = nat", "type"),
        ("extern syntax foo", "type"),
        ("relation R: nat ~> nat\n  hint(input %0)\nrule R/one: 1 ~> 1", "relation"),
        ("extern relation R: nat ~> nat\n  hint(input %0)", "relation"),
        ("dec $f() : nat\ndef $f() = 1", "function"),
        ("extern dec $f() : nat", "function"),
        ("builtin dec $f() : nat", "function"),
    ] {
        let spec_el = crate::spec_fixture::parse(source).unwrap();
        let spec_il = elaborate::convert(spec_el).unwrap();
        let spec_al = algo::convert(spec_il).unwrap();
        let spec_sl = structure::convert(spec_al.clone(), false).unwrap();
        let spec_pl = prosify::convert(spec_sl.clone()).unwrap();
        macro_rules! check {
            ($stage:ident, $spec:expr) => {{
                let mut spec = $spec;
                assert_eq!(spec.len(), 1);
                $stage::context::Global::load(spec.clone()).unwrap();
                spec.push(spec[0].clone());
                let panic = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                    $stage::context::Global::load(spec)
                }))
                .expect_err("malformed IR must violate the loader precondition");
                let message = panic
                    .downcast_ref::<String>()
                    .map(String::as_str)
                    .or_else(|| panic.downcast_ref::<&str>().copied())
                    .unwrap();
                assert!(
                    message.contains(&format!("global {kind} definitions must be unique")),
                    "{message}"
                );
            }};
        }
        check!(al, spec_al);
        check!(sl, spec_sl);
        check!(pl, spec_pl);
    }
}

#[test]
fn repeated_source_types_remain_elaboration_diagnostics() {
    let spec_el = crate::spec_fixture::parse("syntax foo = nat\nsyntax foo = nat").unwrap();
    let report = elaborate::convert(spec_el).unwrap_err();
    assert!(report.find_code("elab/type-definition-repeated").is_some());
}

#[test]
fn repeated_source_callables_remain_elaboration_diagnostics() {
    for (source, code) in [
        (
            "relation R: nat ~> nat\n  hint(input %0)\nrelation R: nat ~> nat\n  hint(input %0)",
            "elab/relation-repeated",
        ),
        (
            "extern relation R: nat ~> nat\n  hint(input %0)\nextern relation R: nat ~> nat\n  hint(input %0)",
            "elab/relation-extern-repeated",
        ),
        ("dec $f() : nat\ndec $f() : nat", "elab/function-repeated"),
        ("extern dec $f() : nat\nextern dec $f() : nat", "elab/function-extern-repeated"),
        ("builtin dec $f() : nat\nbuiltin dec $f() : nat", "elab/function-builtin-repeated"),
    ] {
        let spec_el = crate::spec_fixture::parse(source).unwrap();
        let report = elaborate::convert(spec_el).unwrap_err();
        assert!(report.find_code(code).is_some(), "{report:?}");
    }
}

#[test]
fn mixed_function_and_relation_conclusions_violate_the_ir_precondition() {
    use p4spec_rust::{
        lang::{
            common::{prim::num::Number, source::Span},
            data::typ,
            sl::ast,
        },
        runner::{NullExtern, Runner, Spec},
    };

    let spec_el = crate::spec_fixture::parse(
        "relation R: nat ~> nat\n  hint(input %0)\nrule R/one: 1 ~> 1\ndec $f() : nat\ndef $f() = 1",
    )
    .unwrap();
    let spec_il = elaborate::convert(spec_el).unwrap();
    let spec_al = algo::convert(spec_il).unwrap();
    let mut spec_sl = structure::convert(spec_al, false).unwrap();
    let ast::DefKind::Rel(ast::RelDef::Defined(rel)) = &spec_sl[0].node else {
        panic!("expected a relation")
    };
    let instr_result = p4spec_rust::phrase! {
        node: ast::InstrKind::Result(ast::ResultInstr {
            rel_signature: rel.rel_signature.clone(),
            exps: vec![p4spec_rust::note_phrase! {
                node: ast::ExpKind::Num(Number::Nat(1_u64.into())),
                note: typ::make::nat().node,
                span: Span::default(),
            }],
        }),
        span: Span::default(),
    };
    let ast::DefKind::MetaFunc(ast::MetaFuncDef::Defined(func)) = &mut spec_sl[1].node else {
        panic!("expected a function")
    };
    func.block.push(instr_result);
    let spec_pl = prosify::convert(spec_sl.clone()).unwrap();

    macro_rules! check {
        ($stage:ident, $interp:ident, $spec:expr) => {{
            let mut runner = Runner::new(
                $stage::context::Global::load($spec).unwrap(),
                $stage::$interp::new($stage::Config::new(false, true, true)),
                p4spec_rust::interface::p4(&Spec::Sl(vec![])),
                NullExtern,
            );
            let panic = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                runner.context().call_func("f", &[], &[])
            }))
            .expect_err("mixed conclusion kinds must violate the IR precondition");
            let message = panic
                .downcast_ref::<String>()
                .map(String::as_str)
                .or_else(|| panic.downcast_ref::<&str>().copied())
                .unwrap();
            assert!(message.contains("function and relation conclusions cannot mix"), "{message}");
        }};
    }
    check!(pl, PlInterp, spec_pl);
    check!(sl, SlInterp, spec_sl);
}

#[test]
fn function_return_and_tail_call_remain_a_runtime_failure() {
    use p4spec_rust::runner::{NullExtern, Runner, Spec};

    let spec_el = crate::spec_fixture::parse(
        "dec $g() : nat\ndef $g() = 2\ndec $f() : nat\ndef $f() = 1\ndef $f() = $g()",
    )
    .unwrap();
    let spec_il = elaborate::convert(spec_el).unwrap();
    let spec_al = algo::convert(spec_il).unwrap();
    let spec_sl = structure::convert(spec_al, false).unwrap();
    let mut runner = Runner::new(
        sl::context::Global::load(spec_sl).unwrap(),
        sl::SlInterp::new(sl::Config::new(false, true, true)),
        p4spec_rust::interface::p4(&Spec::Sl(vec![])),
        NullExtern,
    );
    let report = runner
        .context()
        .call_func("f", &[], &[])
        .unwrap_err()
        .into_report();
    assert!(report.find_code("runtime/flow-invalid").is_some(), "{report:?}");
}
