//! Effect propagation through compiled specifications and native interpreters
//!
//! The host counts real invocations while the specification exercises
//! candidate selection, memoization, and reentry in AL, SL, and PL.

use p4spectec::lang::il::ast::Typ;
use p4spectec::lang::{
    common::source::Span,
    data::value::flat::{self as value, Value},
};
use p4spectec::{frontend::parse::parse_text, pass};

use p4spectec::runner::*;

use p4spectec::lang::data::arena::Arena;

#[derive(Default)]
struct Host {
    calls: u64,
}

impl Extern for Host {
    fn eval_rel<Interp, Iface>(
        _ctx: &mut RunnerContext<'_, Interp, Iface, Self>,
        name: &str,
        _values: &[Value],
    ) -> Eval<Vec<Value>, ExternError>
    where
        Iface: Interface,
        Interp: Interpreter<Iface, Self>,
    {
        match name {
            "WrongOutputs" => Eval::new(Ok(vec![]), SideEffect::Impure),
            "PureFailure" => {
                Eval::new(Err(ExternError::diagnostic_unconfigured()), SideEffect::Pure)
            }
            _ => Eval::new(Err(ExternError::diagnostic_unconfigured()), SideEffect::Impure),
        }
    }

    fn eval_func<Interp, Iface>(
        ctx: &mut RunnerContext<'_, Interp, Iface, Self>,
        name: &str,
        _targs: &[Typ],
        _values: &[Value],
    ) -> Eval<Value, ExternError>
    where
        Iface: Interface,
        Interp: Interpreter<Iface, Self>,
    {
        if name == "calls" {
            let calls = ctx.external().calls;
            return Eval::new(
                value::make::nat(ctx.arena_mut(), calls.into(), Span::default())
                    .map_err(Into::into),
                SideEffect::Pure,
            );
        }
        ctx.external_mut().calls += 1;
        match name {
            "tick" | "pure_host" => Eval::new(
                value::make::nat(ctx.arena_mut(), 1.into(), Span::default()).map_err(Into::into),
                if name == "tick" { SideEffect::Impure } else { SideEffect::Pure },
            ),
            "fail" => Eval::new(Err(ExternError::diagnostic_unconfigured()), SideEffect::Impure),
            "reenter" => ctx.call_func("effect", &[], &[]).map_err(Into::into),
            "reenter_result" | "reenter_error" => {
                let name = if name == "reenter_result" { "effect" } else { "mismatch" };
                // Reentry effects belong to the context, even if only the result is used
                let result = ctx.call_func(name, &[], &[]).result.map_err(Into::into);
                ctx.external_mut().calls += 1;
                Eval::new(result, SideEffect::Pure)
            }
            _ => unreachable!(),
        }
    }

    fn clear(&mut self) {
        self.calls = 0;
    }
}

const SOURCE: &str = r#"
var n : nat
extern dec $calls() : nat
extern dec $tick() : nat
extern dec $fail() : nat
extern dec $reenter() : nat
extern dec $reenter_result() : nat
extern dec $reenter_error() : nat
extern dec $pure_host() : nat
builtin dec $max_nat(nat*) : nat

dec $pure() : nat
def $pure() = 7

dec $effect() : nat
def $effect() = $tick()

dec $retry() : nat
def $retry() = 0
  -- if $tick() = 0
def $retry() = 7
  -- otherwise

dec $retry_normal() : nat
def $retry_normal() = 0
  -- if $tick() = 0
def $retry_normal() = 7

dec $twice_normal() : nat
def $twice_normal() = $retry_normal() + $retry_normal()

dec $twice() : nat
def $twice() = $retry() + $retry()

dec $fatal() : nat
def $fatal() = $tick() + $fail()

dec $builtin_error() : nat
def $builtin_error() = $max_nat([])

dec $mismatch() : nat
def $mismatch() = 0
  -- if $tick() = 0

dec $after_success() : nat
def $after_success() = 7
def $after_success() = 0
  -- if $tick() = 0

dec $overlap() : nat
def $overlap() = 7
def $overlap() = $tick() + 0

dec $debug() : nat
def $debug() = 7
  -- debug 7

dec $cached() : nat
def $cached() = $pure_host() + 0

dec $cached_twice() : nat
def $cached_twice() = $cached() + $cached()

dec $isolated() : nat
def $isolated() = $tick() + $cached() + $cached()

dec $effect_at(nat) : nat
def $effect_at(n) = $tick() + n

dec $iterate(nat*) : nat*
def $iterate(n*) = ($effect_at(n))*

dec $iterated() : nat*
def $iterated() = $iterate([0, 1, 2])

extern relation External: EXTERNAL nat
  hint(input %0)
extern relation WrongOutputs: WRONG nat |- nat
  hint(input %0)
extern relation PureFailure: PUREFAIL nat
  hint(input %0)

relation Check: CHECK nat
  hint(input %0)
rule Check/fail: CHECK n
  -- if $tick() = 0

relation Hold: HOLD nat
  hint(input %0)
rule Hold/test: HOLD n
  -- Check: CHECK n

relation NotHold: NOTHOLD nat
  hint(input %0)
rule NotHold/test: NOTHOLD n
  -- Check:/ CHECK n

relation Tail: TAIL nat
  hint(input %0)
rule Tail/test: TAIL n
  -- if $tick() = 1
  -- Check: CHECK n

"#;

/// Reads the invocation count through the public extern entry.
fn calls(arena: &mut Arena, runner: &mut Runner<BuiltinInterface, Host>) -> u64 {
    let eval = runner.eval_func(arena, "calls", &[], &[]);
    let value = eval.result.unwrap();
    p4spectec::lang::common::prim::num::to_int(value::get::num(arena, &value).unwrap())
        .try_into()
        .unwrap()
}

/// Exercises effects, caching, and mutable host reentry through real definitions.
fn check(arena: &mut Arena, runner: &mut Runner<BuiltinInterface, Host>, cache: bool, det: bool) {
    // A mismatching impure candidate must prevent caching its pure fallback
    let eval = runner.eval_func(arena, "twice", &[], &[]);
    assert_eq!(eval.effect, SideEffect::Impure);
    assert_eq!(
        value::get::num(arena, &eval.result.unwrap()).unwrap(),
        &p4spectec::lang::common::prim::num::Number::Nat(14.into())
    );
    assert_eq!(calls(arena, runner), 2, "det={det}");

    // Independent calls start pure, even after an impure evaluation
    let eval = runner.eval_func(arena, "pure", &[], &[]);
    assert_eq!(eval.effect, SideEffect::Pure);
    assert_eq!(
        value::get::num(arena, &eval.result.unwrap()).unwrap(),
        &p4spectec::lang::common::prim::num::Number::Nat(7.into())
    );

    // Host failures and same-runner reentry preserve their effects
    let eval = runner.eval_func(arena, "fatal", &[], &[]);
    assert!(matches!(eval.result, Err(InterpreterError::Fatal(_))));
    assert_eq!(eval.effect, SideEffect::Impure);
    let eval = runner.eval_func(arena, "reenter", &[], &[]);
    assert!(eval.result.is_ok());
    assert_eq!(eval.effect, SideEffect::Impure);
    for name in ["reenter_result", "reenter_error"] {
        let count = calls(arena, runner);
        let eval = runner.eval_func(arena, name, &[], &[]);
        assert_eq!(calls(arena, runner), count + 3);
        assert_eq!(eval.result.is_ok(), name == "reenter_result");
        assert_eq!(eval.effect, SideEffect::Impure, "{name}");
    }
    // Pure successes are cached independently of preceding impure siblings
    for (name, count, effect_expect) in [
        ("cached_twice", if cache { 1 } else { 2 }, SideEffect::Pure),
        ("isolated", if cache { 2 } else { 3 }, SideEffect::Impure),
        (
            "after_success",
            if det { 1 } else { 0 },
            if det { SideEffect::Impure } else { SideEffect::Pure },
        ),
        ("iterated", 3, SideEffect::Impure),
        ("twice_normal", 2, SideEffect::Impure),
        ("debug", 0, SideEffect::Impure),
    ] {
        runner.reset();
        let eval = runner.eval_func(arena, name, &[], &[]);
        assert!(eval.result.is_ok(), "{name}: {:?}", eval.result);
        assert_eq!(eval.effect, effect_expect, "{name}");
        assert_eq!(calls(arena, runner), count, "{name}: cache={cache} det={det}");
    }

    // Failed and overlapping candidates retain all effects they executed
    for name in ["mismatch", "builtin_error", "overlap"] {
        let eval = runner.eval_func(arena, name, &[], &[]);
        if name == "overlap" && !det {
            assert!(eval.result.is_ok());
            assert_eq!(eval.effect, SideEffect::Pure);
        } else {
            assert!(eval.result.is_err(), "{name}");
            assert_eq!(eval.effect, SideEffect::Impure, "{name}");
        }
    }

    // Hold, not-hold, and relation tail calls cannot discard failed effects
    let value = value::make::nat(arena, 1.into(), Span::default()).unwrap();
    for name in ["Hold", "NotHold", "Tail"] {
        let eval = runner.eval_rel(arena, name, &[value]);
        assert_eq!(eval.result.is_ok(), name == "NotHold", "{name}");
        assert_eq!(eval.effect, SideEffect::Impure, "{name}");
    }

    // Extern failures and post-call arity guards keep the host's classification
    for (name, effect_expect) in [
        ("External", SideEffect::Impure),
        ("WrongOutputs", SideEffect::Impure),
        ("PureFailure", SideEffect::Pure),
    ] {
        let eval = runner.eval_rel(arena, name, &[value]);
        assert!(matches!(eval.result, Err(InterpreterError::Fatal(_))), "{name}");
        assert_eq!(eval.effect, effect_expect, "{name}");
    }

    // Entry validation happens before an effectful definition is executed
    let eval = runner.eval_func(arena, "effect", &[], &[value]);
    assert!(eval.result.is_err());
    assert_eq!(eval.effect, SideEffect::Pure);

    // Reset starts an independent execution with fresh effect tracking
    runner.reset();
    let eval = runner.eval_func(arena, "pure", &[], &[]);
    assert!(eval.result.is_ok());
    assert_eq!(eval.effect, SideEffect::Pure);
}

/// Runs host contracts in every stage and cache/determinism mode.
pub(super) fn run() {
    for cache in [false, true] {
        for det in [false, true] {
            let spec_el = parse_text("effects.watsup".into(), SOURCE).unwrap();
            let spec_il = pass::elaborate::convert(spec_el).unwrap();
            let spec_al = pass::algo::convert(spec_il).unwrap();
            let spec_sl = pass::structure::convert(spec_al.clone(), true).unwrap();
            let spec_pl =
                pass::prosify::convert(pass::structure::convert(spec_al.clone(), false).unwrap())
                    .unwrap();
            let config = Config::new(cache, det, true);
            let mut arena = Arena::new();
            let mut runners = [
                build_al(&mut arena, spec_al, config, Host::default()).unwrap(),
                build_sl(&mut arena, spec_sl, config, Host::default()).unwrap(),
                build_pl(&mut arena, spec_pl, config, Host::default()).unwrap(),
            ];
            for runner in &mut runners {
                check(&mut arena, runner, cache, det);
            }
            for runner in &mut runners {
                runner.reset();
            }
            arena.reset_values();
            for runner in &mut runners {
                check(&mut arena, runner, cache, det);
            }
        }
    }
}
