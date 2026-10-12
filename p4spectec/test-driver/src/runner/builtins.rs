//! Builtin state belongs to one prepared runner
//!
//! Interleaved calls and resets must not advance or clear another runner's
//! fresh identifiers, including when both use the same arena.

use p4spectec::lang::data::{arena::Arena, value::flat as value};
use p4spectec::runner::{BuiltinInterface, Config, NullExtern, Runner, SideEffect};

use super::{build, check_value, compile, failure};
use crate::Result;

const SOURCE: &str = r#"
builtin dec $fresh_typeId() : text
builtin dec $sum_nat(nat*) : nat
dec $fresh() : text
def $fresh() = $fresh_typeId()
dec $sum() : nat
def $sum() = $sum_nat([2, 3])
"#;

/// Reads the observable fresh name and checks its effect classification.
fn fresh(
    arena: &mut Arena,
    runner: &mut Runner<BuiltinInterface, NullExtern>,
    count: u64,
) -> Result<()> {
    let eval = runner.eval_func(arena, "fresh", &[], &[]);
    assert_eq!(eval.effect, SideEffect::Impure);
    let value = eval.result.map_err(failure)?;
    assert_eq!(value::get::text(arena, &value).map_err(failure)?, format!("FRESH__{count}"));
    Ok(())
}

/// Interleaves all native stages and verifies local and whole-execution resets.
pub(super) fn run() -> Result<()> {
    for cache in [false, true] {
        for det in [false, true] {
            let mut arena = Arena::new();
            let config = Config::new(cache, det, true);
            let mut runners = compile(SOURCE)?
                .into_iter()
                .map(|spec| build(&mut arena, spec, config, NullExtern))
                .collect::<Result<Vec<_>>>()?;
            for runner in &mut runners {
                runner.reset();
            }

            // Each builtin instance starts its own sequence even when calls interleave
            for count in [0, 1] {
                for runner in &mut runners {
                    fresh(&mut arena, runner, count)?;
                    let eval = runner.eval_func(&mut arena, "sum", &[], &[]);
                    check_value(&arena, eval, 5)?;
                }
            }

            // A local reset must not restart another live runner's state
            runners[0].reset();
            fresh(&mut arena, &mut runners[0], 0)?;
            for runner in &mut runners[1..] {
                fresh(&mut arena, runner, 2)?;
            }

            // A new program resets every runner before invalidating the shared values
            for runner in &mut runners {
                runner.reset();
            }
            arena.reset_values();
            for runner in &mut runners {
                fresh(&mut arena, runner, 0)?;
            }

            // A stateful builtin failure remains impure without producing a value
            let source = "builtin dec $fresh_typeId(nat) : text";
            for spec in compile(source)? {
                let mut runner = build(&mut arena, spec, config, NullExtern)?;
                let value = value::make::nat(
                    &mut arena,
                    0.into(),
                    p4spectec::lang::common::source::Span::default(),
                )
                .map_err(failure)?;
                let eval = runner.eval_func(&mut arena, "fresh_typeId", &[], &[value]);
                assert!(eval.result.is_err());
                assert_eq!(eval.effect, SideEffect::Impure);
            }
        }
    }
    Ok(())
}
