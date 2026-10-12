//! Runner acceptance through compiled AL, SL, and PL specifications
//!
//! The same host-facing runner type executes function, relation, and program
//! entries in each language stage, preserving values and effects.

mod effects;
mod relay;

use p4spectec::lang::{
    common::{prim::num::Number, source::Span},
    data::{
        arena::Arena,
        value::flat::{self as value, Value},
    },
};
use p4spectec::runner::{
    self, BuiltinInterface, Config, Eval, Extern, NullExtern, Runner, SideEffect, Spec,
};
use p4spectec::{frontend::parse::parse_text, pass};

use crate::{Error, Result};

const SOURCE: &str = r#"
var n : nat
syntax token = TOKEN nat | OTHER
dec $wrap(nat) : token
def $wrap(n) = TOKEN n
dec $unwrap(token) : nat
def $unwrap(TOKEN n) = n

dec $increment(nat) : nat
def $increment(n) = n + 7

relation R: nat |- nat
  hint(input %0)
rule R/result: n |- $increment(n)
"#;

/// Converts setup and evaluation failures into test-driver failures.
fn failure(error: impl std::fmt::Display) -> Error {
    Error::Invalid(format!("runner acceptance: {error}"))
}

/// Checks the hand-derived result and its effect classification.
fn check_value(arena: &Arena, eval: Eval<Value>, num_expect: u64) -> Result<()> {
    let value = eval.result.map_err(failure)?;
    let num = value::get::num(arena, &value).map_err(failure)?;
    if num != &Number::Nat(num_expect.into()) || eval.effect != SideEffect::Pure {
        return Err(failure(format!(
            "expected pure nat {num_expect}, got {num:?}/{:?}",
            eval.effect
        )));
    }
    Ok(())
}

/// Exercises one runner through every public evaluation entry.
fn check(arena: &mut Arena, runner: &mut Runner<BuiltinInterface, NullExtern>) -> Result<()> {
    let value = value::make::nat(arena, 35.into(), Span::default()).map_err(failure)?;
    let eval = runner.eval_func(arena, "increment", &[], &[value]);
    check_value(arena, eval, 42)?;

    // Relation and program entries preserve the same output and effects
    let evals = [runner.eval_rel(arena, "R", &[value]), runner.eval_program(arena, "R", value)];
    for eval in evals {
        let values = eval.result.map_err(failure)?;
        let [value] = values.as_slice() else {
            return Err(failure("expected one relation output"));
        };
        check_value(arena, Eval::new(Ok(*value), eval.effect), 42)?;
    }

    // A failed lookup must not run an effectful host operation
    let eval = runner.eval_func(arena, "missing", &[], &[]);
    if eval.result.is_ok() || eval.effect != SideEffect::Pure {
        return Err(failure("missing function must fail without effects"));
    }
    Ok(())
}

/// Keeps distinct definitions and cached results valid across a shared reset.
fn check_shared(
    arena: &mut Arena,
    config: Config,
    runners: &mut [Runner<BuiltinInterface, NullExtern>],
) -> Result<()> {
    // A second spec must not reuse the first spec's same-named definition
    let source = SOURCE
        .replace("n + 7", "n + 11")
        .replace("OTHER", "ALTERNATE");
    let spec_el = parse_text("runner-other.watsup".into(), &source).map_err(failure)?;
    let spec_il = pass::elaborate::convert(spec_el).map_err(failure)?;
    let spec_al = pass::algo::convert(spec_il).map_err(failure)?;
    let mut runner_other = runner::build_al(arena, spec_al, config, NullExtern).map_err(failure)?;

    for num in [10, 20] {
        // Values returned by one runner are input handles for every other runner
        let value = value::make::nat(arena, num.into(), Span::default()).map_err(failure)?;
        let eval = runner_other.eval_func(arena, "increment", &[], &[value]);
        let value_other = eval.result.map_err(failure)?;
        let value_case = runner_other
            .eval_func(arena, "wrap", &[], &[value_other])
            .result
            .map_err(failure)?;
        for runner in runners.iter_mut() {
            let eval = runner.eval_func(arena, "increment", &[], &[value_other]);
            check_value(arena, eval, num + 18)?;
            let eval = runner.eval_func(arena, "unwrap", &[], &[value_case]);
            check_value(arena, eval, num + 11)?;
        }

        // Drop every cache and host state before invalidating shared value handles
        for runner in runners.iter_mut() {
            runner.reset();
        }
        runner_other.reset();
        arena.reset_values();
    }
    Ok(())
}

/// Compiles the same source through all three production interpreter passes.
fn compile(source: &str) -> Result<[Spec; 3]> {
    let spec_el = parse_text("runner.watsup".into(), source).map_err(failure)?;
    let spec_il = pass::elaborate::convert(spec_el).map_err(failure)?;
    let spec_al = pass::algo::convert(spec_il).map_err(failure)?;
    let spec_sl = pass::structure::convert(spec_al.clone(), true).map_err(failure)?;
    let spec_pl =
        pass::prosify::convert(pass::structure::convert(spec_al.clone(), false).map_err(failure)?)
            .map_err(failure)?;
    Ok([Spec::Al(spec_al), Spec::Sl(spec_sl), Spec::Pl(spec_pl)])
}

/// Prepares the selected stage with one concrete host implementation.
fn build<Ext: Extern>(
    arena: &mut Arena,
    spec: Spec,
    config: Config,
    external: Ext,
) -> Result<Runner<BuiltinInterface, Ext>> {
    match spec {
        Spec::Al(spec) => runner::build_al(arena, spec, config, external),
        Spec::Sl(spec) => runner::build_sl(arena, spec, config, external),
        Spec::Pl(spec) => runner::build_pl(arena, spec, config, external),
    }
    .map_err(failure)
}

/// Runs the runner contract in all native stages and cache/determinism modes.
pub fn run() -> Result<()> {
    for cache in [false, true] {
        for det in [false, true] {
            let config = Config::new(cache, det, true);
            let mut arena = Arena::new();

            // Runtime stage selection must not change the runner's public type
            let mut runners: Vec<Runner<BuiltinInterface, NullExtern>> = compile(SOURCE)?
                .into_iter()
                .map(|spec| build(&mut arena, spec, config, NullExtern))
                .collect::<Result<_>>()?;
            for runner in &mut runners {
                check(&mut arena, runner)?;
            }
            check_shared(&mut arena, config, &mut runners)?;
        }
    }
    effects::run();
    relay::run()?;
    eprintln!("runner: 12 stage/cache/determinism combinations passed");
    Ok(())
}
