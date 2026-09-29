use crate::interp::report::ReportExt;
use p4spec_rust::interp::shared::backtrack::Failure;
use p4spec_rust::lang::traits::print::Print;
use p4spec_rust::{
    lang::{
        data::value::{Value, get},
        il::ast::Typ,
    },
    pass::{algo, elaborate, prosify, structure},
    runner::{self, Extern, Interface, Interpreter, RunnerContext},
};
use std::{cell::Cell, rc::Rc};

#[derive(Clone, Default)]
struct Reentry {
    calls: Rc<Cell<usize>>,
}

impl Extern for Reentry {
    fn eval_func<Interp, Iface>(
        &self,
        ctx: &mut RunnerContext<'_, Interp, Iface, Self>,
        name: &str,
        targs: &[Typ],
        values: &[Value],
    ) -> Result<(Value, bool), Interp::Error>
    where
        Iface: Interface,
        Interp: Interpreter<Iface, Self>,
    {
        self.calls.set(self.calls.get() + 1);
        if name == "abort" {
            return Err(runner::ExternError::Failure("host aborted".into()).into());
        }
        ctx.call_func("inner", targs, values)
            .map(|value| (value, false))
    }

    fn eval_rel<Interp, Iface>(
        &self,
        ctx: &mut RunnerContext<'_, Interp, Iface, Self>,
        _name: &str,
        values: &[Value],
    ) -> Result<(Vec<Value>, bool), Interp::Error>
    where
        Iface: Interface,
        Interp: Interpreter<Iface, Self>,
    {
        self.calls.set(self.calls.get() + 1);
        ctx.call_rel("Inner", values).map(|values| (values, false))
    }

    fn clear(&mut self) {
        self.calls.set(0);
    }
}

#[test]
fn mismatch_through_extern_reentry_reaches_otherwise() {
    let source = r#"
builtin dec $max_nat(nat*) : nat
extern dec $bridge() : nat
dec $inner() : nat
def $inner() = $max_nat([])
dec $outer() : nat
def $outer() = $bridge()
def $outer() = 7
  -- otherwise
dec $pair() : (nat, nat)
def $pair() = ($outer(), $outer())
"#;
    for det in [false, true] {
        let spec_el = crate::spec_fixture::parse(source).unwrap();
        let spec_il = elaborate::convert(spec_el).unwrap();
        let spec_al = algo::convert(spec_il).unwrap();
        let spec_sl = structure::convert(spec_al.clone(), false).unwrap();
        let spec_pl = prosify::convert(spec_sl.clone()).unwrap();
        let config = runner::Config::new(true, det, true);
        macro_rules! check {
            ($build:ident, $spec:expr) => {{
                let host = Reentry::default();
                let mut runner = runner::$build($spec, config, host.clone()).unwrap();
                let value = runner.context().call_func("pair", &[], &[]).unwrap();
                let values = get::tuple(runner.arena(), &value).unwrap();
                assert_eq!(values.len(), 2);
                for value in values {
                    assert_eq!(get::num(runner.arena(), value).unwrap().to_string(), "7");
                }
                assert_eq!(
                    host.calls.get(),
                    2,
                    "failed extern calls taint the enclosing cache entry"
                );
                runner.reset();
                assert_eq!(host.calls.get(), 0);
            }};
        }
        check!(build_al, spec_al);
        check!(build_sl, spec_sl);
        check!(build_pl, spec_pl);
    }
}

#[test]
fn fatal_through_extern_reentry_skips_otherwise() {
    let source = r#"
extern dec $abort() : nat
extern dec $bridge() : nat
dec $inner() : nat
def $inner() = $abort()
dec $outer() : nat
def $outer() = $bridge()
def $outer() = 7
  -- otherwise
"#;
    for det in [false, true] {
        let spec_el = crate::spec_fixture::parse(source).unwrap();
        let spec_il = elaborate::convert(spec_el).unwrap();
        let spec_al = algo::convert(spec_il).unwrap();
        let spec_sl = structure::convert(spec_al.clone(), false).unwrap();
        let spec_pl = prosify::convert(spec_sl.clone()).unwrap();
        let config = runner::Config::new(true, det, true);
        macro_rules! check {
            ($build:ident, $spec:expr) => {{
                let host = Reentry::default();
                let mut runner = runner::$build($spec, config, host.clone()).unwrap();
                let failure = runner.context().call_func("outer", &[], &[]).unwrap_err();
                let Failure::Fatal(report) = failure else { panic!("expected fatal") };
                let cause = report.find_code("runtime/extern-failed").unwrap();
                assert_eq!(cause.diagnostic().message, "host aborted");
                assert_eq!(host.calls.get(), 2);
            }};
        }
        check!(build_al, spec_al);
        check!(build_sl, spec_sl);
        check!(build_pl, spec_pl);
    }
}

#[test]
fn relation_mismatch_through_extern_reentry_reaches_otherwise() {
    let source = r#"
var n : nat
var m : nat
extern relation Bridge: nat |- nat
  hint(input %0)
relation Inner: nat |- nat
  hint(input %0)
rule Inner/zero: 0 |- 0
relation Outer: nat |- nat
  hint(input %0)
rule Outer/bridge: n |- m
  -- Bridge: n |- m
rule Outer/fallback: n |- 7
  -- otherwise
"#;
    for det in [false, true] {
        let spec_el = crate::spec_fixture::parse(source).unwrap();
        let spec_il = elaborate::convert(spec_el).unwrap();
        let spec_al = algo::convert(spec_il).unwrap();
        let spec_sl = structure::convert(spec_al.clone(), false).unwrap();
        let spec_pl = prosify::convert(spec_sl.clone()).unwrap();
        let config = runner::Config::new(true, det, true);
        macro_rules! check {
            ($build:ident, $spec:expr) => {{
                let host = Reentry::default();
                let mut runner = runner::$build($spec, config, host.clone()).unwrap();
                let value = p4spec_rust::lang::data::value::make::nat(
                    runner.arena_mut(),
                    1.into(),
                    Default::default(),
                )
                .unwrap();
                let values = runner.context().call_rel("Outer", &[value]).unwrap();
                assert_eq!(values.len(), 1);
                assert_eq!(get::num(runner.arena(), &values[0]).unwrap().to_string(), "7");
                assert_eq!(host.calls.get(), 1);
            }};
        }
        check!(build_al, spec_al);
        check!(build_sl, spec_sl);
        check!(build_pl, spec_pl);
    }
}
