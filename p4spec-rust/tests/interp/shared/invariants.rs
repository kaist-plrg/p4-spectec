use crate::interp::report::ReportExt;
use p4spec_rust::{
    interp::{al, pl, sl},
    pass::{algo, elaborate, prosify, structure},
};

#[test]
fn loading_duplicate_global_types_violates_the_ir_precondition() {
    for source in ["syntax foo = nat", "extern syntax foo"] {
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
                assert!(message.contains("global type definitions must be unique"), "{message}");
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
