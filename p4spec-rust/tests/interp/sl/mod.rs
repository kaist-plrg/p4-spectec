use p4spec_rust::lang::traits::print::Print;
use p4spec_rust::{
    interp::sl::{Config, SlInterp, context::Global},
    lang::{
        common::source::Span,
        data::value::{Value, get, make},
        sl::ast,
    },
    pass::{algo, elaborate, structure},
    runner::{BuiltinInterface, NullExtern, Runner},
};

fn spec(source: &str) -> ast::Spec {
    let spec_el = crate::spec_fixture::parse(source).unwrap();
    let spec_il = elaborate::convert(spec_el).unwrap();
    let spec_al = algo::convert(spec_il).unwrap();
    structure::convert(spec_al, false).unwrap()
}

fn runner(source: &str, det: bool) -> Runner<SlInterp, BuiltinInterface, NullExtern> {
    Runner::new(
        Global::load(spec(source)).unwrap(),
        SlInterp::new(Config::new(false, det, true)),
        p4spec_rust::interface::p4(&p4spec_rust::runner::Spec::Sl(Vec::new())),
        NullExtern,
    )
}

fn call(
    runner: &mut Runner<SlInterp, BuiltinInterface, NullExtern>,
    name: &str,
    num: u64,
) -> Value {
    let value = make::nat(runner.arena_mut(), num.into(), Span::default()).unwrap();
    runner.context().call_func(name, &[], &[value]).unwrap()
}

#[test]
fn executes_relation_bindings_and_else_fallthrough() {
    let source = r#"
var n : nat
relation Step: nat ~> nat
  hint(input %0)
rule Step/positive: n ~> $(n + 1)
  -- if $(n > 0)
rule Step/zero: n ~> 9
  -- otherwise
dec $use(nat) : nat
def $use(n) = n_result
  -- Step: n ~> n_result
"#;
    for det in [false, true] {
        let mut runner = runner(source, det);
        for (num, expected) in [(0, "9"), (2, "3")] {
            let value = call(&mut runner, "use", num);
            assert_eq!(get::num(runner.arena(), &value).unwrap().to_string(), expected);
        }
    }
}

#[test]
fn builtin_and_extern_failures_skip_otherwise() {
    let source = r#"
builtin dec $text_to_int(text) : int
extern dec $unavailable() : int
dec $recover() : int
def $recover() = $text_to_int("invalid")
def $recover() = +7
  -- otherwise
dec $fatal() : int
def $fatal() = $unavailable()
def $fatal() = +9
  -- otherwise
"#;
    for det in [false, true] {
        let mut runner = runner(source, det);
        assert!(matches!(
            runner.context().call_func("recover", &[], &[]),
            Err(p4spec_rust::interp::shared::backtrack::Failure::Fatal(_))
        ));
        assert!(runner.context().call_func("fatal", &[], &[]).is_err());
    }
}

mod execution;

mod control;

mod review;
