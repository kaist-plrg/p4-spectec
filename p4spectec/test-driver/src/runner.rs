//! Runner acceptance through compiled AL, SL, and PL specifications
//!
//! The same host-facing runner type executes function, relation, and program
//! entries in each language stage, preserving values and effects.

use p4spectec::lang::{
    common::{prim::num::Number, source::Span},
    data::{
        arena::Arena,
        value::flat::{self as value, Value},
    },
};
use p4spectec::runner::{self, BuiltinInterface, Config, Eval, NullExtern, Runner, SideEffect};
use p4spectec::{frontend::parse::parse_text, pass};

use crate::{Error, Result};

const SOURCE: &str = r#"
var n : nat

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
fn check(runner: &mut Runner<BuiltinInterface, NullExtern>) -> Result<()> {
    let value =
        value::make::nat(runner.arena_mut(), 35.into(), Span::default()).map_err(failure)?;
    let eval = runner.eval_func("increment", &[], &[value]);
    check_value(runner.arena(), eval, 42)?;

    // Relation and program entries preserve the same output and effects
    let evals = [runner.eval_rel("R", &[value]), runner.eval_program("R", value)];
    for eval in evals {
        let values = eval.result.map_err(failure)?;
        let [value] = values.as_slice() else {
            return Err(failure("expected one relation output"));
        };
        check_value(runner.arena(), Eval::new(Ok(*value), eval.effect), 42)?;
    }

    // A failed lookup must not run an effectful host operation
    let eval = runner.eval_func("missing", &[], &[]);
    if eval.result.is_ok() || eval.effect != SideEffect::Pure {
        return Err(failure("missing function must fail without effects"));
    }
    Ok(())
}

/// Runs the runner contract in all native stages and cache/determinism modes.
pub fn run() -> Result<()> {
    for cache in [false, true] {
        for det in [false, true] {
            // Compile real definitions through each stage's production passes
            let spec_el = parse_text("runner.watsup".into(), SOURCE).map_err(failure)?;
            let spec_il = pass::elaborate::convert(spec_el).map_err(failure)?;
            let spec_al = pass::algo::convert(spec_il).map_err(failure)?;
            let spec_sl = pass::structure::convert(spec_al.clone(), true).map_err(failure)?;
            let spec_pl = pass::prosify::convert(
                pass::structure::convert(spec_al.clone(), false).map_err(failure)?,
            )
            .map_err(failure)?;
            let config = Config::new(cache, det, true);

            // Runtime stage selection must not change the runner's public type
            let mut runners: Vec<Runner<BuiltinInterface, NullExtern>> = vec![
                runner::build_al(spec_al, config, NullExtern).map_err(failure)?,
                runner::build_sl(spec_sl, config, NullExtern).map_err(failure)?,
                runner::build_pl(spec_pl, config, NullExtern).map_err(failure)?,
            ];
            for runner in &mut runners {
                check(runner)?;
            }
        }
    }
    eprintln!("runner: 12 stage/cache/determinism combinations passed");
    Ok(())
}
