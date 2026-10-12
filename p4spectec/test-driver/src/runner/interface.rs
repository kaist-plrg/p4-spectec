//! Custom interfaces assemble and reenter a runner through its public API
//!
//! A builtin mutates its state on both sides of a same-runner reentry.
//! Nested effects remain visible even when the outer builtin returns Pure.

use p4spectec::lang::{
    common::source::Span,
    data::{
        arena::Arena,
        value::flat::{self as value, Value},
    },
    il::ast::{Id, Typ},
};
use p4spectec::runner::*;

use super::{compile, failure};
use crate::Result;

const SOURCE: &str = r#"
builtin dec $tick() : nat
builtin dec $calls() : nat
builtin dec $reenter() : nat
builtin dec $reenter_fail() : nat
dec $effect() : nat
def $effect() = $tick()
dec $mismatch() : nat
def $mismatch() = 0
  -- if $tick() = 0
"#;

#[derive(Default)]
struct Host {
    calls: u64,
}

impl Interface for Host {
    fn call_builtin<Interp, Ext>(
        ctx: &mut RunnerContext<'_, Interp, Self, Ext>,
        id: &Id,
        _targs: &[Typ],
        _values: &[Value],
    ) -> Eval<Value, InterfaceError>
    where
        Ext: Extern,
        Interp: Interpreter<Self, Ext>,
    {
        match id.node.as_ref() {
            "tick" | "calls" => {
                // Only tick advances the state; calls observes it for acceptance
                let (arena, interface) = ctx.arena_interface_mut();
                let effect = if id.node.as_ref() == "tick" {
                    interface.calls += 1;
                    SideEffect::Impure
                } else {
                    SideEffect::Pure
                };
                let result = value::make::nat(arena, interface.calls.into(), Span::default())
                    .map_err(|error| InterfaceError(ExternError::from(error).into_report()));
                Eval::new(result, effect)
            }
            "reenter" | "reenter_fail" => {
                // Release each interface borrow before entering the interpreter again
                ctx.arena_interface_mut().1.calls += 1;
                let name = if id.node.as_ref() == "reenter" { "effect" } else { "mismatch" };
                let result = ctx
                    .call_func(name, &[], &[])
                    .result
                    .map_err(|error| InterfaceError(error.into_report()));
                ctx.arena_interface_mut().1.calls += 1;
                Eval::new(result, SideEffect::Pure)
            }
            _ => {
                // Undefined test hooks must not silently succeed
                Eval::new(Err(InterfaceError::diagnostic_unconfigured()), SideEffect::Impure)
            }
        }
    }

    fn clear(&mut self) {
        self.calls = 0;
    }
}

/// Checks a custom interface and the null preset with every native backend.
pub(super) fn run() -> Result<()> {
    for cache in [false, true] {
        for det in [false, true] {
            for spec in compile(SOURCE)? {
                let mut arena = Arena::new();
                let config = Config::new(cache, det, true);
                let mut runner = Runner::new(&mut arena, spec, config, Host::default(), NullExtern)
                    .map_err(failure)?;
                for _ in 0..2 {
                    // A caller can mutate the interface before and after successful reentry
                    let eval = runner.eval_func(&mut arena, "reenter", &[], &[]);
                    assert!(eval.result.is_ok());
                    assert_eq!(eval.effect, SideEffect::Impure);
                    let eval = runner.eval_func(&mut arena, "calls", &[], &[]);
                    super::check_value(&arena, eval, 3)?;

                    // Failed reentry also retains nested builtin effects
                    let eval = runner.eval_func(&mut arena, "reenter_fail", &[], &[]);
                    assert!(eval.result.is_err());
                    assert_eq!(eval.effect, SideEffect::Impure);
                    let eval = runner.eval_func(&mut arena, "calls", &[], &[]);
                    super::check_value(&arena, eval, 6)?;
                    runner.reset();
                    arena.reset_values();
                }
            }
        }
    }

    // NullInterface remains a usable public configuration with conservative failures
    for spec in compile(SOURCE)? {
        let mut arena = Arena::new();
        let mut runner =
            Runner::new(&mut arena, spec, Config::new(true, true, true), NullInterface, NullExtern)
                .map_err(failure)?;
        let eval = runner.eval_func(&mut arena, "tick", &[], &[]);
        assert!(eval.result.is_err());
        assert_eq!(eval.effect, SideEffect::Impure);
    }
    Ok(())
}
