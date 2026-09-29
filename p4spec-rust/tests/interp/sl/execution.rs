use super::*;
use crate::interp::report::ReportExt;
use p4spec_rust::{diagnostic::ReportKind, interp::shared::backtrack::Failure};
use p4spec_rust::{
    lang::data::{typ, value::ValueArena},
    phrase,
};
use std::rc::Rc;
fn make_runner(spec_sl: ast::Spec, det: bool) -> Runner<SlInterp, BuiltinInterface, NullExtern> {
    Runner::new(
        Global::load(spec_sl).unwrap(),
        SlInterp::new(Config::new(false, det, true)),
        p4spec_rust::interface::p4(&p4spec_rust::runner::Spec::Sl(Vec::new())),
        NullExtern,
    )
}
fn nat(arena: &mut ValueArena, num: u64) -> Value {
    make::nat(arena, num.into(), Span::default()).unwrap()
}
fn number(arena: &ValueArena, value: &Value) -> String {
    get::num(arena, value).unwrap().to_string()
}
#[test]
fn test_higher_order_alias_is_resolved_in_the_caller() {
    let source = r#"
var n : nat
dec $add(nat) : nat
def $add(n) = $(n + 3)
dec $apply(nat, def $f(nat) : nat) : nat
def $apply(n, def $f) = $f(n)
dec $forward(nat, def $g(nat) : nat) : nat
def $forward(n, def $g) = $apply(n, def $g)
dec $entry(nat) : nat
def $entry(n) = $forward(n, def $add)
"#;
    let mut runner = make_runner(spec(source), true);
    assert_eq!(
        {
            let value = &{
                let (name, targs, values) = ("entry", &[], &[nat(runner.arena_mut(), 4)]);
                runner.context().call_func(name, targs, values)
            }
            .unwrap();
            number(runner.arena(), value)
        },
        "7"
    );
}

#[test]
fn test_iterated_premises_collect_results_and_do_not_leak_failed_bindings() {
    use p4spec_rust::lang::al::ast;
    let source = r#"
var n : nat
dec $map(nat*) : nat*
def $map(n*) = n_result*
  -- if (n_result = $(n + 1))*
dec $fallback(nat*) : nat*
def $fallback(n*) = n_result*
  -- if (n_result = $(n + 1))*
  -- if false
def $fallback(n*) = n*
  -- otherwise
"#;
    let mut spec_case_al = spec_al(source);
    for def in &mut spec_case_al {
        let ast::DefKind::MetaFunc(ast::MetaFuncDef::Defined(func)) = &mut def.node else {
            continue;
        };
        if func.id.node != "fallback" {
            continue;
        }
        let clause = &mut func.clauses[0];
        let ast::ArgKind::Exp(exp_l) = &clause.node.args[0].node else {
            panic!("expression argument")
        };
        // Force a branch-local overwrite before failure to expose scope leakage
        let prem = phrase!(node: ast::PremKind::Let(ast::LetPrem {
            exp_l: exp_l.as_ref().clone(), exp_r: clause.node.exp.clone(),
        }), span: clause.span.clone());
        clause.node.prems.insert(1, prem);
    }
    let mut runner = make_runner(structure::convert(spec_case_al, false).unwrap(), false);
    for values in [vec![], vec![nat(runner.arena_mut(), 2), nat(runner.arena_mut(), 4)]] {
        let value = make::list(
            runner.arena_mut(),
            (typ::make::iter(typ::make::nat(), p4spec_rust::lang::common::Iter::List))
                .node
                .clone()
                .into(),
            values.clone(),
            Span::default(),
        )
        .unwrap();
        let mapped = runner
            .context()
            .call_func("map", &[], std::slice::from_ref(&value))
            .unwrap();
        let expected = if values.is_empty() { vec![] } else { vec!["3", "5"] };
        assert_eq!(
            get::list(runner.arena(), &mapped)
                .unwrap()
                .iter()
                .map(|value| number(runner.arena(), value))
                .collect::<Vec<_>>(),
            expected
        );
        let fallback = runner
            .context()
            .call_func("fallback", &[], std::slice::from_ref(&value))
            .unwrap();
        assert_eq!(fallback.node, value.node);
    }
}

#[test]
fn test_optional_premises_collect_present_and_absent_bindings() {
    let source = r#"
var n : nat
dec $map_opt(nat?) : nat?
def $map_opt(n?) = n_result?
  -- if (n_result = $(n + 1))?
"#;
    let mut runner = make_runner(spec(source), true);
    for input in [None, Some(nat(runner.arena_mut(), 6))] {
        let present = input.is_some();
        let value = make::opt(
            runner.arena_mut(),
            (typ::make::opt(typ::make::nat())).node.clone().into(),
            input,
            Span::default(),
        )
        .unwrap();
        let output = runner
            .context()
            .call_func("map_opt", &[], &[value])
            .unwrap();
        assert_eq!(
            get::opt(runner.arena(), &output)
                .unwrap()
                .map(|value| number(runner.arena(), &value)),
            present.then(|| "7".to_owned())
        );
    }
}

#[test]
fn test_hold_and_not_hold_distinguish_unmatch_from_fatal_failure() {
    use p4spec_rust::lang::al::ast;
    for external in [false, true] {
        let relation = if external {
            "extern relation Check: CHECK nat\n  hint(input %0)\n"
        } else {
            "relation Check: CHECK nat\n  hint(input %0)\nrule Check/zero: CHECK n\n  -- if n = 0\n"
        };
        let source = format!(
            r#"
var n : nat
{relation}
dec $hold(nat) : bool
def $hold(n) = true
  -- Check: CHECK n
def $hold(n) = false
  -- otherwise
dec $not_hold(nat) : bool
def $not_hold(n) = true
  -- Check: CHECK n
def $not_hold(n) = false
  -- otherwise
"#
        );
        let mut spec_case_al = spec_al(&source);
        for def in &mut spec_case_al {
            let ast::DefKind::MetaFunc(ast::MetaFuncDef::Defined(func)) = &mut def.node else {
                continue;
            };
            let negate = func.id.node == "not_hold";
            let prem = &mut func.clauses[0].node.prems[0];
            let (id, not_exp) = match &prem.node {
                ast::PremKind::Rule(prem) => (prem.id.clone(), prem.not_exp.clone()),
                ast::PremKind::IfHold(prem) => (prem.id.clone(), prem.not_exp.clone()),
                other => panic!("relation premise: {other:?}"),
            };
            prem.node = if negate {
                ast::PremKind::IfNotHold(ast::IfNotHoldPrem { id, not_exp })
            } else {
                ast::PremKind::IfHold(ast::IfHoldPrem { id, not_exp })
            };
        }
        for det in [false, true] {
            let mut runner =
                make_runner(structure::convert(spec_case_al.clone(), false).unwrap(), det);
            for n in [0, 1] {
                for (name, expected) in [("hold", n == 0), ("not_hold", n != 0)] {
                    let result = {
                        let (name, targs, values) = (name, &[], &[nat(runner.arena_mut(), n)]);
                        runner.context().call_func(name, targs, values)
                    };
                    if external {
                        let error = result.unwrap_err().into_report();
                        assert!(matches!(error.kind, ReportKind::Frame { .. }));
                        assert!(error.render().contains("Check"), "{error}");
                    } else {
                        assert_eq!(get::bool(runner.arena(), &result.unwrap()).unwrap(), expected);
                    }
                }
            }
        }
    }
}

#[test]
fn test_native_function_calls_and_else_fallback() {
    let spec_sl = spec(
        r#"
var n : nat
dec $inc(nat) : nat
def $inc(n) = n + 1
dec $choose(nat) : nat
def $choose(n) = 0
  -- if false
def $choose(n) = $inc(n)
  -- otherwise
"#,
    );
    let mut runner = make_runner(spec_sl, false);
    assert_eq!(
        {
            let value = &{
                let (name, targs, values) = ("choose", &[], &[nat(runner.arena_mut(), 4)]);
                runner.context().call_func(name, targs, values)
            }
            .unwrap();
            number(runner.arena(), value)
        },
        "5"
    );
}

#[test]
fn test_program_evaluation_preserves_input_value() {
    let spec_sl =
        spec("var n : nat\nrelation Pass: nat ~> nat\n  hint(input %0)\nrule Pass/pass: n ~> n");
    let mut runner = make_runner(spec_sl, false);
    let program = nat(runner.arena_mut(), 8);
    let values = runner.eval_program("Pass", program).unwrap();
    assert!((values[0] == program));
}

#[test]
fn test_type_bindings_survive_clause_selection_and_else() {
    let spec_sl = spec(
        r#"
var b : bool
dec $identity<X>(X) : X
def $identity<X>(X) = X
dec $pick<X>(bool, X) : X
def $pick<X>(true, X) = $identity<X>(X)
  -- if false
def $pick<X>(true, X) = $identity<X>(X)
def $pick<X>(b, X) = $identity<X>(X)
  -- otherwise
"#,
    );
    for det in [false, true] {
        let mut runner = make_runner(spec_sl.clone(), det);
        for condition in [true, false] {
            let value = nat(runner.arena_mut(), 7);
            let values =
                [make::bool(runner.arena_mut(), condition, Span::default()).unwrap(), value];
            let output = runner
                .context()
                .call_func("pick", &[typ::make::nat()], &values)
                .unwrap();
            assert!((output == value));
        }
    }
}

#[test]
fn test_public_guard_rejects_malformed_function_input() {
    let mut runner =
        make_runner(spec("var n : nat\ndec $ignore(nat) : nat\ndef $ignore(n) = 1"), false);
    let error = {
        let (name, targs, values) =
            ("ignore", &[], &[make::bool(runner.arena_mut(), true, Span::default()).unwrap()]);
        runner.context().call_func(name, targs, values)
    }
    .unwrap_err()
    .into_report();
    assert!(error.render().contains("function argument of ignore"), "{error}");
}
struct Host {
    calls: Rc<std::cell::Cell<u64>>,
    value: fn(&mut ValueArena) -> Value,
    reenter: bool,
}

impl p4spec_rust::runner::Interface for Host {
    fn call_builtin(
        &mut self,
        arena: &mut ValueArena,
        _id: &ast::Id,
        _targs: &[ast::Typ],
        _values: &[Value],
    ) -> Result<(Value, bool), p4spec_rust::runner::InterfaceError> {
        self.calls.set(self.calls.get() + 1);
        Ok(((self.value)(arena), true))
    }

    fn clear(&mut self) {
        self.calls.set(0);
    }
}

impl p4spec_rust::runner::Extern for Host {
    fn eval_rel<Interp, Iface>(
        &self,
        ctx: &mut p4spec_rust::runner::RunnerContext<'_, Interp, Iface, Self>,
        _name: &str,
        values: &[Value],
    ) -> Result<(Vec<Value>, bool), Interp::Error>
    where
        Iface: p4spec_rust::runner::Interface,
        Interp: p4spec_rust::runner::Interpreter<Iface, Self>,
    {
        self.calls.set(self.calls.get() + 1);
        let values = if self.reenter {
            ctx.call_rel("Step", values)?
        } else {
            vec![(self.value)(ctx.arena_mut())]
        };
        Ok((values, true))
    }

    fn eval_func<Interp, Iface>(
        &self,
        ctx: &mut p4spec_rust::runner::RunnerContext<'_, Interp, Iface, Self>,
        _name: &str,
        targs: &[ast::Typ],
        _values: &[Value],
    ) -> Result<(Value, bool), Interp::Error>
    where
        Iface: p4spec_rust::runner::Interface,
        Interp: p4spec_rust::runner::Interpreter<Iface, Self>,
    {
        // The OCaml extern boundary erases type arguments
        assert!(targs.is_empty());
        self.calls.set(self.calls.get() + 1);
        let value = if self.reenter {
            let value = (self.value)(ctx.arena_mut());
            ctx.call_func("inner", &[], &[value])?
        } else {
            (self.value)(ctx.arena_mut())
        };
        Ok((value, true))
    }

    fn clear(&mut self) {
        self.calls.set(0);
    }
}

fn host(value: fn(&mut ValueArena) -> Value, reenter: bool) -> Host {
    Host { calls: Rc::new(std::cell::Cell::new(0)), value, reenter }
}

#[test]
fn test_guards_toggle_input_checks_and_substitute_type_arguments() {
    let spec_sl = spec("var n : nat\ndec $ignore<X>(X) : nat\ndef $ignore<X>(X) = 1");
    for det in [false, true] {
        for guard in [false, true] {
            let mut runner = Runner::<SlInterp, _, _>::new(
                Global::load(spec_sl.clone()).unwrap(),
                SlInterp::new(Config::new(false, det, guard)),
                p4spec_rust::interface::p4(&p4spec_rust::runner::Spec::Sl(Vec::new())),
                NullExtern,
            );
            let invalid = make::bool(runner.arena_mut(), true, Span::default()).unwrap();
            let result = runner.context().call_func(
                "ignore",
                &[typ::make::nat()],
                std::slice::from_ref(&invalid),
            );
            if guard {
                assert!(
                    result
                        .unwrap_err()
                        .into_report()
                        .render()
                        .contains("function argument of ignore")
                );
                assert!(
                    {
                        let (name, targs, values) = ("ignore", &[], &[nat(runner.arena_mut(), 1)]);
                        runner.context().call_func(name, targs, values)
                    }
                    .unwrap_err()
                    .into_report()
                    .render()
                    .contains("arity mismatch in type arguments")
                );
                assert!(
                    runner
                        .context()
                        .call_func("ignore", &[typ::make::nat()], &[])
                        .unwrap_err()
                        .into_report()
                        .find_code("runtime/function-input-arity-mismatch")
                        .is_some()
                );
            } else {
                assert_eq!(number(runner.arena(), &result.unwrap()), "1");
            }
            assert_eq!(
                {
                    let value = &runner
                        .context()
                        .call_func("ignore", &[typ::make::bool()], &[invalid])
                        .unwrap();
                    number(runner.arena(), value)
                },
                "1"
            );
        }
    }
}

#[test]
fn test_host_output_guards_are_fatal_and_substitute_return_types() {
    let source = r#"
builtin dec $builtin<X>() : X
extern dec $external<X>() : X
dec $pick<X>() : X
def $pick<X>() = $builtin<X>()
def $pick<X>() = $external<X>()
  -- otherwise
"#;
    let spec_sl = spec(source);
    for det in [false, true] {
        for guard in [false, true] {
            let builtin = host(|arena| make::bool(arena, true, Span::default()).unwrap(), false);
            let external = host(|arena| make::bool(arena, false, Span::default()).unwrap(), false);
            let calls = external.calls.clone();
            let mut runner = Runner::<SlInterp, _, _>::new(
                Global::load(spec_sl.clone()).unwrap(),
                SlInterp::new(Config::new(false, det, guard)),
                builtin,
                external,
            );
            for name in ["builtin", "external", "pick"] {
                let result = runner.context().call_func(name, &[typ::make::nat()], &[]);
                if guard {
                    let error = result.unwrap_err().into_report();
                    assert!(error.render().contains("return value of function"), "{error}");
                } else {
                    assert!(get::bool(runner.arena(), &result.unwrap()).is_ok());
                }
                assert!(
                    {
                        let value = &runner
                            .context()
                            .call_func(name, &[typ::make::bool()], &[])
                            .unwrap();
                        get::bool(runner.arena(), value)
                    }
                    .is_ok()
                );
            }
            assert_eq!(calls.get(), 2, "fatal builtin output must not select else");
        }
    }
}

#[test]
fn test_uncached_extern_reentry_preserves_outer_scope_and_clear_policy() {
    let source = r#"
var n : nat
builtin dec $tick() : nat
extern dec $bridge(nat) : nat
dec $inner(nat) : nat
def $inner(n) = $(n + $tick())
dec $outer(nat) : nat
def $outer(n) = $(n + $bridge(n))
relation Step: nat ~> nat
  hint(input %0)
rule Step/step: n ~> $(n + 1)
extern relation Relay: nat ~> nat
  hint(input %0)
dec $ambiguous() : nat
def $ambiguous() = 1
def $ambiguous() = 2
"#;
    let builtin = host(|arena| nat(arena, 1), false);
    let external = host(|arena| nat(arena, 40), true);
    let calls_builtin = builtin.calls.clone();
    let calls_extern = external.calls.clone();
    let mut runner = Runner::<SlInterp, _, _>::new(
        Global::load(spec(source)).unwrap(),
        SlInterp::new(Config::new(false, true, true)),
        builtin,
        external,
    );
    for _ in 0..2 {
        for count in 1..=2 {
            assert_eq!(
                {
                    let value = &{
                        let (name, targs, values) = ("outer", &[], &[nat(runner.arena_mut(), 5)]);
                        runner.context().call_func(name, targs, values)
                    }
                    .unwrap();
                    number(runner.arena(), value)
                },
                "46"
            );
            assert_eq!(
                {
                    let value = &{
                        let (name, values) = ("Relay", &[nat(runner.arena_mut(), 8)]);
                        runner.context().call_rel(name, values)
                    }
                    .unwrap()[0];
                    number(runner.arena(), value)
                },
                "9"
            );
            assert_eq!(calls_builtin.get(), count);
            assert_eq!(calls_extern.get(), count * 2);
        }
        assert!(
            {
                let (name, targs, values) = (
                    "outer",
                    &[],
                    &[make::bool(runner.arena_mut(), true, Span::default()).unwrap()],
                );
                runner.context().call_func(name, targs, values)
            }
            .unwrap_err()
            .into_report()
            .render()
            .contains("function argument of outer")
        );
        assert!(
            runner
                .context()
                .call_func("ambiguous", &[], &[])
                .unwrap_err()
                .into_report()
                .render()
                .contains("nondeterministic")
        );
        runner.reset();
        assert_eq!(calls_builtin.get(), 0);
        assert_eq!(calls_extern.get(), 0);
    }
}

#[test]
fn test_extern_reentry_uses_public_input_guards() {
    let source =
        "extern dec $bridge(nat) : nat\nvar n : nat\ndec $inner(nat) : nat\ndef $inner(n) = 7";
    for guard in [false, true] {
        let mut runner = Runner::<SlInterp, _, _>::new(
            Global::load(spec(source)).unwrap(),
            SlInterp::new(Config::new(false, false, guard)),
            p4spec_rust::interface::p4(&p4spec_rust::runner::Spec::Sl(Vec::new())),
            host(|arena| make::bool(arena, true, Span::default()).unwrap(), true),
        );
        let result = {
            let (name, targs, values) = ("bridge", &[], &[nat(runner.arena_mut(), 1)]);
            runner.context().call_func(name, targs, values)
        };
        if guard {
            assert!(
                result
                    .unwrap_err()
                    .into_report()
                    .render()
                    .contains("function argument of inner")
            );
        } else {
            assert_eq!(number(runner.arena(), &result.unwrap()), "7");
        }
    }
}

#[derive(Clone, Default)]
struct CacheHost {
    calls: Rc<std::cell::RefCell<std::collections::HashMap<String, usize>>>,
}

impl CacheHost {
    fn count(&self, name: &str) -> usize {
        self.calls.borrow().get(name).copied().unwrap_or(0)
    }

    fn record(&self, name: &str) {
        *self.calls.borrow_mut().entry(name.to_owned()).or_default() += 1;
    }
}

impl p4spec_rust::runner::Interface for CacheHost {
    fn call_builtin(
        &mut self,
        arena: &mut ValueArena,
        id: &ast::Id,
        _targs: &[ast::Typ],
        values: &[Value],
    ) -> Result<(Value, bool), p4spec_rust::runner::InterfaceError> {
        self.record(&id.node);
        if id.node == "fail" {
            return Err(
                Box::new(p4spec_rust::interface::builtin::BuiltinError::new("failure")).into()
            );
        }
        let value = values.first().copied().unwrap_or_else(|| nat(arena, 7));
        Ok((value, id.node == "impure"))
    }

    fn clear(&mut self) {
        self.calls.borrow_mut().clear();
    }
}

impl p4spec_rust::runner::Extern for CacheHost {
    fn eval_func<Interp, Iface>(
        &self,
        ctx: &mut p4spec_rust::runner::RunnerContext<'_, Interp, Iface, Self>,
        name: &str,
        targs: &[ast::Typ],
        values: &[Value],
    ) -> Result<(Value, bool), Interp::Error>
    where
        Iface: p4spec_rust::runner::Interface,
        Interp: p4spec_rust::runner::Interpreter<Iface, Self>,
    {
        assert!(targs.is_empty());
        self.record(name);
        let value = if name == "bridge" { ctx.call_func("inner", &[], values)? } else { values[0] };
        Ok((value, false))
    }

    fn eval_rel<Interp, Iface>(
        &self,
        _ctx: &mut p4spec_rust::runner::RunnerContext<'_, Interp, Iface, Self>,
        name: &str,
        values: &[Value],
    ) -> Result<(Vec<Value>, bool), Interp::Error>
    where
        Iface: p4spec_rust::runner::Interface,
        Interp: p4spec_rust::runner::Interpreter<Iface, Self>,
    {
        self.record(name);
        Ok((values.to_vec(), false))
    }

    fn clear(&mut self) {
        self.calls.borrow_mut().clear();
    }
}

#[test]
fn test_cache_memoizes_zero_argument_calls_and_clears_on_entry() {
    let source = r#"
builtin dec $pure() : nat
dec $pair() : (nat, nat)
def $pair() = ($pure(), $pure())
"#;
    // Compare memoized calls with the uncached interpreter
    for cache in [false, true] {
        let host = CacheHost::default();
        let mut runner = Runner::<SlInterp, _, _>::new(
            Global::load(spec(source)).unwrap(),
            SlInterp::new(Config::new(cache, false, false)),
            host.clone(),
            NullExtern,
        );
        let value_expect = nat(runner.arena_mut(), 7);
        // Reuse the zero-argument result only within each public entry
        for num_calls in [1, 2] {
            let value = runner.context().call_func("pair", &[], &[]).unwrap();
            assert_eq!(get::tuple(runner.arena(), &value).unwrap(), &[value_expect, value_expect]);
            assert_eq!(host.count("pure"), if cache { num_calls } else { 2 * num_calls });
        }
    }
}

#[test]
fn test_cache_reuses_canonical_arguments_and_original_annotations() {
    let source = r#"
var ns : nat*
builtin dec $pure(nat*) : nat*
dec $pair(nat*, nat*) : (nat*, nat*)
def $pair(ns_1, ns_2) = ($pure(ns_1), $pure(ns_2))
"#;
    let host = CacheHost::default();
    let mut runner = Runner::<SlInterp, _, _>::new(
        Global::load(spec(source)).unwrap(),
        SlInterp::new(Config::new(true, false, false)),
        host.clone(),
        NullExtern,
    );
    let value_l = nat(runner.arena_mut(), 7);
    let span = Span::new(
        p4spec_rust::lang::common::source::Position::new("right.p4", 50, 0),
        p4spec_rust::lang::common::source::Position::new("right.p4", 50, 2),
    );
    let value_r =
        Value { span: make::bool(runner.arena_mut(), false, span).unwrap().span, ..value_l };
    let value_r = Value {
        note: make::new(
            runner.arena_mut(),
            p4spec_rust::lang::data::value::ValueKind::Bool(false),
            typ::make::int().node.clone().into(),
            Span::default(),
        )
        .unwrap()
        .note,
        ..value_r
    };
    let typ = typ::make::iter(typ::make::nat(), p4spec_rust::lang::common::Iter::List).node;
    let value_l =
        make::list(runner.arena_mut(), typ.clone().into(), vec![value_l], Span::default()).unwrap();
    let value_r =
        make::list(runner.arena_mut(), typ.clone().into(), vec![value_r], Span::default()).unwrap();
    assert_ne!(value_l.node, value_r.node);
    for expected in [1, 2] {
        let value = runner
            .context()
            .call_func("pair", &[], &[value_l, value_r])
            .unwrap();
        assert_eq!(get::tuple(runner.arena(), &value).unwrap(), &[value_l, value_l]);
        assert_eq!(host.count("pure"), expected, "public entry clears memo tables");
    }
}

#[test]
fn test_cache_propagates_effects_and_host_failures_but_caches_pure_children() {
    for det in [false, true] {
        for operation in ["impure", "fail"] {
            let source = format!(
                r#"
var n : nat
builtin dec $pure(nat) : nat
builtin dec ${operation}(nat) : nat
dec $recover(nat) : nat
def $recover(n) = ${operation}(n)
def $recover(n) = $pure(n)
  -- otherwise
dec $pair(nat) : (nat, nat)
def $pair(n) = ($recover(n), $recover(n))
"#
            );
            let host = CacheHost::default();
            let mut runner = Runner::<SlInterp, _, _>::new(
                Global::load(spec(&source)).unwrap(),
                SlInterp::new(Config::new(true, det, false)),
                host.clone(),
                NullExtern,
            );
            let value = nat(runner.arena_mut(), 5);
            let result = runner.context().call_func("pair", &[], &[value]).unwrap();
            assert_eq!(get::tuple(runner.arena(), &result).unwrap(), &[value, value]);
            assert_eq!(host.count(operation), 2, "tainted wrappers cannot be cached");
            assert_eq!(host.count("pure"), usize::from(operation == "fail"));
        }
    }
}

#[test]
fn test_cache_memoizes_relations_but_not_direct_extern_calls() {
    let source = r#"
var n : nat
extern dec $probe(nat) : nat
extern relation Probe: nat ~> nat
  hint(input %0)
relation Step: nat ~> nat
  hint(input %0)
rule Step/step: n ~> $probe(n)
dec $pair(nat) : (nat, nat, nat, nat)
def $pair(n) = (n_1, n_2, n_3, n_4)
  -- Step: n ~> n_1
  -- Step: n ~> n_2
  -- Probe: n ~> n_3
  -- Probe: n ~> n_4
dec $direct(nat) : (nat, nat)
def $direct(n) = ($probe(n), $probe(n))
"#;
    for cache in [false, true] {
        let host = CacheHost::default();
        let mut runner = Runner::<SlInterp, _, _>::new(
            Global::load(spec(source)).unwrap(),
            SlInterp::new(Config::new(cache, false, false)),
            host.clone(),
            host.clone(),
        );
        let value = nat(runner.arena_mut(), 5);
        let result = runner.context().call_func("pair", &[], &[value]).unwrap();
        assert_eq!(get::tuple(runner.arena(), &result).unwrap(), &[value; 4]);
        assert_eq!(host.count("probe"), if cache { 1 } else { 2 });
        assert_eq!(host.count("Probe"), 2);
        runner.reset();
        let value = nat(runner.arena_mut(), 5);
        runner.context().call_func("direct", &[], &[value]).unwrap();
        assert_eq!(host.count("probe"), 2);
    }
}

#[test]
fn test_cache_reentry_clears_results_and_preserves_outer_effects() {
    for operation in ["pure", "impure"] {
        let source = format!(
            r#"
var n : nat
builtin dec $pure(nat) : nat
builtin dec $impure(nat) : nat
extern dec $bridge(nat) : nat
dec $inner(nat) : nat
def $inner(n) = $pure(n)
dec $outer(nat) : (nat, nat, nat)
def $outer(n) = ($pure(n), $bridge(${operation}(n)), $pure(n))
dec $pair(nat) : ((nat, nat, nat), (nat, nat, nat))
def $pair(n) = ($outer(n), $outer(n))
"#
        );
        let host = CacheHost::default();
        let mut runner = Runner::<SlInterp, _, _>::new(
            Global::load(spec(&source)).unwrap(),
            SlInterp::new(Config::new(true, false, false)),
            host.clone(),
            host.clone(),
        );
        let value = nat(runner.arena_mut(), 5);
        runner.context().call_func("pair", &[], &[value]).unwrap();
        let impure = operation == "impure";
        assert_eq!(host.count("bridge"), if impure { 2 } else { 1 });
        assert_eq!(host.count("pure"), if impure { 3 } else { 2 });
        assert_eq!(host.count("impure"), if impure { 2 } else { 0 });
    }
}

#[test]
fn test_cache_excludes_local_and_higher_order_calls() {
    let source = r#"
var n : nat
extern dec $probe(nat) : nat
dec $forward(nat) : nat
def $forward(n) = $probe(n)
dec $apply(nat, def $f(nat) : nat) : (nat, nat)
def $apply(n, def $f) = ($f(n), $f(n))
dec $pair(nat) : ((nat, nat), (nat, nat))
def $pair(n) = ($apply(n, def $forward), $apply(n, def $forward))
"#;
    let host = CacheHost::default();
    let mut runner = Runner::<SlInterp, _, _>::new(
        Global::load(spec(source)).unwrap(),
        SlInterp::new(Config::new(true, false, false)),
        host.clone(),
        host.clone(),
    );
    let value = nat(runner.arena_mut(), 5);
    runner.context().call_func("pair", &[], &[value]).unwrap();
    assert_eq!(host.count("probe"), 4);
}

#[test]
fn test_cache_omits_type_arguments_and_runner_reset_discards_results() {
    let source = r#"
builtin dec $pure<X>(nat) : nat
dec $pair() : (nat, nat)
def $pair() = ($pure<nat>(7), $pure<bool>(7))
"#;
    let host = CacheHost::default();
    let mut runner = Runner::<SlInterp, _, _>::new(
        Global::load(spec(source)).unwrap(),
        SlInterp::new(Config::new(true, false, true)),
        host.clone(),
        NullExtern,
    );
    let id = phrase!(node: "pair".to_owned(), span: Span::default());
    let value = {
        let mut ctx_runner = runner.context();
        let ctx = p4spec_rust::interp::sl::context::Context::new(ctx_runner.spec());
        p4spec_rust::interp::sl::eval::call::invoke_func(&mut ctx_runner, &ctx, &id, &[], &[])
            .map_err(Failure::into_report)
            .unwrap()
    };
    assert_eq!(host.count("pure"), 1);
    assert_eq!(get::tuple(runner.arena(), &value).unwrap().len(), 2);
    runner.reset();
    let value_new = {
        let mut ctx_runner = runner.context();
        let ctx = p4spec_rust::interp::sl::context::Context::new(ctx_runner.spec());
        p4spec_rust::interp::sl::eval::call::invoke_func(&mut ctx_runner, &ctx, &id, &[], &[])
            .map_err(Failure::into_report)
            .unwrap()
    };
    assert_eq!(get::tuple(runner.arena(), &value_new).unwrap().len(), 2);
    assert_eq!(host.count("pure"), 1, "reset also discards internal call results");
}

#[test]
fn test_program_reset_isolates_cached_values_between_arenas() {
    let source = "var n : nat\nbuiltin dec $pure(nat) : nat\ndec $pair(nat) : (nat, nat)\ndef $pair(n) = ($pure(n), $pure(n))";
    let host = CacheHost::default();
    let mut runner = Runner::<SlInterp, _, _>::new(
        Global::load(spec(source)).unwrap(),
        SlInterp::new(Config::new(true, false, false)),
        host.clone(),
        NullExtern,
    );
    let id = phrase!(node: "pair".to_owned(), span: Span::default());
    for num in [7, 9, 7] {
        runner.reset();
        let value = nat(runner.arena_mut(), num);
        let value_pair = {
            let mut ctx_runner = runner.context();
            let ctx = p4spec_rust::interp::sl::context::Context::new(ctx_runner.spec());
            p4spec_rust::interp::sl::eval::call::invoke_func(
                &mut ctx_runner,
                &ctx,
                &id,
                &[],
                &[value],
            )
            .map_err(Failure::into_report)
            .unwrap()
        };
        assert_eq!(get::tuple(runner.arena(), &value_pair).unwrap(), &[value, value]);
        assert_eq!(host.count("pure"), 1, "one cached call within each independent program");
    }
}

#[test]
fn test_cache_distinguishes_call_names_and_argument_order() {
    let source = r#"
var n : nat
builtin dec $pure(nat, nat) : nat
builtin dec $other(nat, nat) : nat
dec $use(nat, nat) : (nat, nat, nat, nat)
def $use(n_1, n_2) = ($pure(n_1, n_2), $pure(n_2, n_1), $pure(n_1, n_2), $other(n_1, n_2))
"#;
    let host = CacheHost::default();
    let mut runner = Runner::<SlInterp, _, _>::new(
        Global::load(spec(source)).unwrap(),
        SlInterp::new(Config::new(true, false, true)),
        host.clone(),
        NullExtern,
    );
    let value_l = nat(runner.arena_mut(), 5);
    let value_r = nat(runner.arena_mut(), 7);
    let value = runner
        .context()
        .call_func("use", &[], &[value_l, value_r])
        .unwrap();
    assert_eq!(get::tuple(runner.arena(), &value).unwrap(), &[value_l, value_r, value_l, value_l]);
    assert_eq!(host.count("pure"), 2);
    assert_eq!(host.count("other"), 1);
}

fn spec_al(source: &str) -> p4spec_rust::lang::al::ast::Spec {
    algo::convert(elaborate::convert(crate::spec_fixture::parse(source).unwrap()).unwrap()).unwrap()
}

#[test]
fn clear_discards_memos_and_retains_the_live_arena() {
    let host = CacheHost::default();
    let mut runner = Runner::new(Global::load(spec("var n : nat\nbuiltin dec $pure(nat) : nat\ndec $pair(nat) : (nat, nat)\ndef $pair(n) = ($pure(n), $pure(n))")).unwrap(), SlInterp::new(Config::new(true, false, true)), host.clone(), NullExtern);
    let value = nat(runner.arena_mut(), 7);
    let id = phrase!(node: "pair".to_owned(), span: Span::default());
    for clear in [false, false, true] {
        let mut ctx_runner = runner.context();
        if clear {
            <SlInterp as p4spec_rust::runner::Interpreter<CacheHost, NullExtern>>::clear(
                ctx_runner.interp_mut(),
            );
        }
        let ctx = p4spec_rust::interp::sl::context::Context::new(ctx_runner.spec());
        let value_pair = p4spec_rust::interp::sl::eval::call::invoke_func(
            &mut ctx_runner,
            &ctx,
            &id,
            &[],
            &[value],
        )
        .map_err(Failure::into_report)
        .unwrap();
        assert_eq!(get::tuple(ctx_runner.arena(), &value_pair).unwrap(), &[value, value]);
        assert_eq!(host.count("pure"), if clear { 2 } else { 1 });
    }
}

#[test]
fn test_case_scrutinee_is_evaluated_once_across_guard_attempts() {
    use p4spec_rust::lang::common::prim::{bool as bool_op, num::Number};
    use p4spec_rust::note_phrase;

    let source = "builtin dec $probe() : nat\ndec $entry() : nat\ndef $entry() = 0";
    let mut spec_sl = spec(source);
    let ast::DefKind::MetaFunc(ast::MetaFuncDef::Defined(func)) =
        &mut spec_sl.last_mut().unwrap().node
    else {
        panic!("expected function");
    };
    let exp_num = |num: u64| -> ast::Exp {
        note_phrase!(node: ast::ExpKind::Num(Number::Nat(num.into())), note: typ::make::nat().node, span: Span::default())
    };
    func.block = vec![phrase!(node: ast::InstrKind::Case(ast::CaseInstr {
        exp: note_phrase!(node: ast::ExpKind::Call(
            phrase!(node: "probe".to_owned(), span: Span::default()), vec![], vec![]),
            note: typ::make::nat().node, span: Span::default()),
        cases: [6, 7].into_iter().map(|num| ast::Case {
            guard: ast::Guard::Cmp(ast::CmpOp::Bool(bool_op::CmpOp::Eq), ast::OpTyp::Nat, exp_num(num)),
            block: vec![phrase!(node: ast::InstrKind::Return(ast::ReturnInstr { exp: exp_num(num) }), span: Span::default())],
        }).collect(),
        dangle: false,
    }), span: Span::default())];
    for det in [false, true] {
        let builtin = host(|arena| nat(arena, 7), false);
        let calls = builtin.calls.clone();
        let mut runner = Runner::new(
            Global::load(spec_sl.clone()).unwrap(),
            SlInterp::new(Config::new(false, det, true)),
            builtin,
            NullExtern,
        );
        let value = runner.context().call_func("entry", &[], &[]).unwrap();
        assert_eq!(number(runner.arena(), &value), "7");
        assert_eq!(calls.get(), 1);
    }
}

#[test]
fn test_crossed_unifiers_preserve_clause_bindings_and_fallthrough() {
    let source = r#"
var x : nat
var y : nat
var pair : (nat, nat)
dec $flat((nat, nat), (nat, nat)) : nat
def $flat((x, y), pair) = x
  -- if $(x > 0)
def $flat(pair, pair_1) = 9
  -- if false
def $flat(pair, (x, y)) = y
  -- otherwise
dec $nested(((nat, nat), (nat, nat))) : nat
def $nested(((x, y), pair)) = x
  -- if $(x > 0)
def $nested((pair, pair_1)) = 9
  -- if false
def $nested((pair, (x, y))) = y
  -- otherwise
relation R: (nat, nat) |- (nat, nat) : nat
  hint(input %0 %1)
rule R/left: (x, y) |- pair : x
  -- if $(x > 0)
rule R/right: pair |- (x, y) : y
  -- otherwise
dec $rel_first() : nat
def $rel_first() = x
  -- R: (4, 5) |- (6, 7) : x
dec $rel_else() : nat
def $rel_else() = x
  -- R: (0, 1) |- (2, 3) : x
dec $flat_first() : nat
def $flat_first() = $flat((4, 5), (6, 7))
dec $flat_else() : nat
def $flat_else() = $flat((0, 1), (2, 3))
dec $nested_first() : nat
def $nested_first() = $nested(((4, 5), (6, 7)))
dec $nested_else() : nat
def $nested_else() = $nested(((0, 1), (2, 3)))
"#;
    let spec_al = spec_al(source);
    for without_rule_groups in [false, true] {
        let spec_sl = structure::convert(spec_al.clone(), without_rule_groups).unwrap();
        let mut runner = make_runner(spec_sl, true);
        for (name, expected) in
            [("flat_first", "4"), ("flat_else", "3"), ("nested_first", "4"), ("nested_else", "3")]
        {
            let value = runner.context().call_func(name, &[], &[]).unwrap();
            assert_eq!(number(runner.arena(), &value), expected, "{name}");
        }
    }
}

#[test]
fn test_crossed_unifiers_preserve_iteration_bindings() {
    let source = r#"
var x : nat
var y : nat
var pair : (nat, nat)
dec $f(((nat, nat), (nat, nat))*) : nat*
def $f(((x, y), pair)*) = x*
  -- if false
def $f((pair, (x, y))*) = y*
  -- otherwise
dec $entry() : nat*
def $entry() = $f([((1, 2), (3, 4)), ((5, 6), (7, 8))])
dec $empty() : nat*
def $empty() = $f([])
"#;
    let spec_al = spec_al(source);
    for without_rule_groups in [false, true] {
        let spec_sl = structure::convert(spec_al.clone(), without_rule_groups).unwrap();
        let mut runner = make_runner(spec_sl, true);
        for (name, expected) in [("entry", vec!["4", "8"]), ("empty", vec![])] {
            let value = runner.context().call_func(name, &[], &[]).unwrap();
            let values = get::list(runner.arena(), &value).unwrap();
            assert_eq!(
                values
                    .iter()
                    .map(|value| number(runner.arena(), value))
                    .collect::<Vec<_>>(),
                expected,
                "{name}"
            );
        }
    }
}

#[test]
fn test_unified_iterations_keep_unchanged_variables() {
    let source = r#"
var x : nat
var y : nat
dec $f((nat, nat)*) : nat*
def $f((x, y)*) = y*
  -- if false
def $f((x, nat)*) = x*
  -- otherwise
dec $entry() : nat*
def $entry() = $f([(1, 2), (3, 4)])
"#;
    let spec_al = spec_al(source);
    for without_rule_groups in [false, true] {
        let spec_sl = structure::convert(spec_al.clone(), without_rule_groups).unwrap();
        let mut runner = make_runner(spec_sl, true);
        let value = runner.context().call_func("entry", &[], &[]).unwrap();
        let values = get::list(runner.arena(), &value).unwrap();
        assert_eq!(
            values
                .iter()
                .map(|value| number(runner.arena(), value))
                .collect::<Vec<_>>(),
            vec!["1", "3"]
        );
    }
}

#[test]
fn test_crossed_unifiers_keep_nested_iteration_dimensions() {
    let source = r#"
var x : nat
var y : nat
var pair : (nat, nat)
dec $f(((nat, nat), (nat, nat)*)*) : nat**
def $f((pair, (x, y)*)*) = x**
  -- if false
def $f(((x, y), pair*)*) = [y*]
  -- otherwise
dec $entry() : nat**
def $entry() = $f([((1, 2), [(3, 4)]), ((5, 6), [])])
"#;
    let spec_al = spec_al(source);
    for without_rule_groups in [false, true] {
        let spec_sl = structure::convert(spec_al.clone(), without_rule_groups).unwrap();
        let mut runner = make_runner(spec_sl, true);
        let value = runner.context().call_func("entry", &[], &[]).unwrap();
        let values = get::list(runner.arena(), &value).unwrap();
        let nums = values
            .iter()
            .map(|value| {
                get::list(runner.arena(), value)
                    .unwrap()
                    .iter()
                    .map(|value| number(runner.arena(), value))
                    .collect::<Vec<_>>()
            })
            .collect::<Vec<_>>();
        assert_eq!(nums, vec![vec!["2", "6"]]);
    }
}

#[test]
fn test_total_case_grouping_failure_preserves_order_and_fallthrough() {
    let source = r#"
dec $f(nat*) : nat
def $f([]) = 0
def $f(nat_h :: nat_t*) = 1
  -- if $(nat_h > 0)
def $f([nat, nat_1]) = 2
dec $empty() : nat
def $empty() = $f([])
dec $nonempty() : nat
def $nonempty() = $f([1])
dec $overlap() : nat
def $overlap() = $f([1, 2])
dec $fallback() : nat
def $fallback() = $f([0, 2])
"#;
    let spec_al = spec_al(source);
    for without_rule_groups in [false, true] {
        let spec_sl = structure::convert(spec_al.clone(), without_rule_groups).unwrap();
        let mut runner = make_runner(spec_sl, false);
        for (name, expected) in
            [("empty", "0"), ("nonempty", "1"), ("overlap", "1"), ("fallback", "2")]
        {
            let value = runner.context().call_func(name, &[], &[]).unwrap();
            assert_eq!(number(runner.arena(), &value), expected, "{name}");
        }
    }
}

#[test]
fn test_total_case_grouping_failure_preserves_matching_branch_bodies() {
    let source = r#"
dec $f(nat*, bool) : nat
def $f(nat*, bool_b) = 10
  -- if true = bool_b
  -- if [] = nat*
  -- if false
def $f(nat*, bool_b) = 11
  -- if true = bool_b
  -- if nat_h :: nat_t* = nat*
  -- if false
def $f(nat*, bool_b) = 20
  -- if bool_b = true
  -- if [] = nat*
def $f(nat*, bool_b) = 30
  -- if bool_b = true
  -- if [nat_a, nat_b] = nat*
def $f(nat*, bool_b) = 40
  -- if bool_b = false
dec $empty() : nat
def $empty() = $f([], true)
dec $fixed() : nat
def $fixed() = $f([1, 2], true)
dec $other() : nat
def $other() = $f([], false)
"#;
    let spec_al = spec_al(source);
    for without_rule_groups in [false, true] {
        let spec_sl = structure::convert(spec_al.clone(), without_rule_groups).unwrap();
        let mut runner = make_runner(spec_sl, false);
        for (name, expected) in [("empty", "20"), ("fixed", "30"), ("other", "40")] {
            let value = runner.context().call_func(name, &[], &[]).unwrap();
            assert_eq!(number(runner.arena(), &value), expected, "{name}");
        }
    }
}
