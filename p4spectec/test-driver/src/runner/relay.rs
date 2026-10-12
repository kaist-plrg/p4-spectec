//! Nested runner calls through mutable extern state and a shared arena
//!
//! A host owns another native runner and passes its results and effects back.
//! The two specifications deliberately give the same function different bodies.

use p4spectec::lang::{
    common::source::Span,
    data::{
        arena::Arena,
        value::flat::{self as value, Value},
    },
    il::ast::Typ,
};
use p4spectec::runner::*;

use super::{build, check_value, compile, failure};
use crate::Result;

const SOURCE_TARGET: &str = r#"
var n : nat
extern dec $tick() : nat
dec $answer() : nat
def $answer() = 7
dec $fail() : nat
def $fail() = 0
  -- if $tick() = 0
relation R: nat |- nat
  hint(input %0)
rule R/result: n |- n + $tick()
"#;

const SOURCE_RELAY: &str = r#"
var n : nat
extern dec $tick() : nat
extern dec $relay() : nat
extern dec $fail() : nat
dec $answer() : nat
def $answer() = 99
relation R: nat |- nat
  hint(input %0)
rule R/local: n |- 99
extern relation Relay: nat |- nat
  hint(input %0)
"#;

#[derive(Default)]
struct Counter(u64);

impl Extern for Counter {
    fn eval_rel<Interp, Iface>(
        _ctx: &mut RunnerContext<'_, Interp, Iface, Self>,
        _name: &str,
        _values: &[Value],
    ) -> Eval<Vec<Value>, ExternError>
    where
        Iface: Interface,
        Interp: Interpreter<Iface, Self>,
    {
        Eval::new(Err(ExternError::diagnostic_unconfigured()), SideEffect::Impure)
    }

    fn eval_func<Interp, Iface>(
        ctx: &mut RunnerContext<'_, Interp, Iface, Self>,
        _name: &str,
        _targs: &[Typ],
        _values: &[Value],
    ) -> Eval<Value, ExternError>
    where
        Iface: Interface,
        Interp: Interpreter<Iface, Self>,
    {
        ctx.external_mut().0 += 1;
        let count = ctx.external().0;
        Eval::new(
            value::make::nat(ctx.arena_mut(), count.into(), Span::default()).map_err(Into::into),
            SideEffect::Impure,
        )
    }

    fn clear(&mut self) {
        self.0 = 0;
    }
}

struct Relay {
    runner: Runner<BuiltinInterface, Counter>,
}

impl Extern for Relay {
    fn eval_rel<Interp, Iface>(
        ctx: &mut RunnerContext<'_, Interp, Iface, Self>,
        _name: &str,
        values: &[Value],
    ) -> Eval<Vec<Value>, ExternError>
    where
        Iface: Interface,
        Interp: Interpreter<Iface, Self>,
    {
        let (arena, external) = ctx.arena_external_mut();
        external
            .runner
            .eval_rel(arena, "R", values)
            .map_err(Into::into)
    }

    fn eval_func<Interp, Iface>(
        ctx: &mut RunnerContext<'_, Interp, Iface, Self>,
        name: &str,
        targs: &[Typ],
        values: &[Value],
    ) -> Eval<Value, ExternError>
    where
        Iface: Interface,
        Interp: Interpreter<Iface, Self>,
    {
        let name = if name == "relay" { "answer" } else { name };
        let (arena, external) = ctx.arena_external_mut();
        external
            .runner
            .eval_func(arena, name, targs, values)
            .map_err(Into::into)
    }

    fn clear(&mut self) {
        self.runner.reset();
    }
}

/// Exercises every outer/inner stage pair across two independent programs.
pub(super) fn run() -> Result<()> {
    for cache in [false, true] {
        for det in [false, true] {
            let config = Config::new(cache, det, true);
            for idx in 0..3 {
                for spec in compile(SOURCE_RELAY)? {
                    let mut arena = Arena::new();
                    let spec_target = compile(SOURCE_TARGET)?.into_iter().nth(idx).unwrap();
                    let runner = build(&mut arena, spec_target, config, Counter::default())?;
                    let mut runner = build(&mut arena, spec, config, Relay { runner })?;
                    for num in [10, 20] {
                        // Inner calls use their own definitions, cache, and effect count
                        for (name, num_expect) in [("answer", 99), ("relay", 7)] {
                            let eval = runner.eval_func(&mut arena, name, &[], &[]);
                            check_value(&arena, eval, num_expect)?;
                        }
                        for count in [1, 2] {
                            let eval = runner.eval_func(&mut arena, "tick", &[], &[]);
                            assert_eq!(eval.effect, SideEffect::Impure);
                            check_value(&arena, Eval::new(eval.result, SideEffect::Pure), count)?;
                        }

                        // Relation calls retain their input handles and inner host effects
                        let value = value::make::nat(&mut arena, num.into(), Span::default())
                            .map_err(failure)?;
                        let eval = runner.eval_rel(&mut arena, "Relay", &[value]);
                        assert_eq!(eval.effect, SideEffect::Impure);
                        let values = eval.result.map_err(failure)?;
                        assert_eq!(values.len(), 1);
                        check_value(&arena, Eval::new(Ok(values[0]), SideEffect::Pure), num + 3)?;

                        // Failed nested calls retain effects, then independent calls start pure
                        let eval = runner.eval_func(&mut arena, "fail", &[], &[]);
                        assert!(eval.result.is_err());
                        assert_eq!(eval.effect, SideEffect::Impure);
                        let eval = runner.eval_func(&mut arena, "relay", &[], &[]);
                        check_value(&arena, eval, 7)?;

                        // The host resets its child before the caller clears shared values
                        runner.reset();
                        arena.reset_values();
                    }
                }
            }
        }
    }
    Ok(())
}
