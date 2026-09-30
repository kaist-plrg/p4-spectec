use crate::interp::report::ReportExt;
use p4spec_rust::interp::shared::backtrack::Failure;
use p4spec_rust::{
    lang::{data::value::Value, il::ast::Typ},
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
    ) -> Result<(Value, bool), p4spec_rust::runner::ExternError>
    where
        Iface: Interface,
        Interp: Interpreter<Iface, Self>,
    {
        self.calls.set(self.calls.get() + 1);
        if name == "abort" {
            return Err(runner::ExternError::Message("host aborted".into()));
        }
        ctx.call_func("inner", targs, values)
            .map(|value| (value, false))
            .map_err(Into::into)
    }

    fn eval_rel<Interp, Iface>(
        &self,
        ctx: &mut RunnerContext<'_, Interp, Iface, Self>,
        _name: &str,
        values: &[Value],
    ) -> Result<(Vec<Value>, bool), p4spec_rust::runner::ExternError>
    where
        Iface: Interface,
        Interp: Interpreter<Iface, Self>,
    {
        self.calls.set(self.calls.get() + 1);
        ctx.call_rel("Inner", values)
            .map(|values| (values, false))
            .map_err(Into::into)
    }

    fn clear(&mut self) {
        self.calls.set(0);
    }
}

#[test]
fn builtin_failure_through_extern_reentry_skips_otherwise() {
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
                let failure = runner.context().call_func("pair", &[], &[]).unwrap_err();
                assert!(matches!(failure, Failure::Fatal(_)));
                assert_eq!(host.calls.get(), 1);
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
                let mut cause = report.as_ref();
                while !cause.children.is_empty() {
                    cause = &cause.children[0];
                }
                assert_eq!(cause.code(), None);
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
fn relation_mismatch_through_extern_reentry_is_fatal() {
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
                let failure = runner.context().call_rel("Outer", &[value]).unwrap_err();
                assert!(matches!(failure, Failure::Fatal(_)));
                assert_eq!(host.calls.get(), 1);
            }};
        }
        check!(build_al, spec_al);
        check!(build_sl, spec_sl);
        check!(build_pl, spec_pl);
    }
}

#[test]
fn function_mismatch_through_extern_reentry_is_fatal() {
    let source = r#"
extern dec $bridge() : nat
dec $inner() : nat
def $inner() = 0
  -- if false
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
                let failure = runner.context().call_func("pair", &[], &[]).unwrap_err();
                assert!(matches!(failure, Failure::Fatal(_)));
                assert_eq!(host.calls.get(), 1);
                runner.reset();
                assert_eq!(host.calls.get(), 0);
            }};
        }
        check!(build_al, spec_al);
        check!(build_sl, spec_sl);
        check!(build_pl, spec_pl);
    }
}
