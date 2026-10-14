//! Memoization across pure and effectful tail-call chains

use std::path::PathBuf;

use crate::{
    lang::{
        common::source::Span,
        data::{
            arena::Arena,
            value::flat::{Value, get, make},
        },
        il::ast::Typ,
    },
    pass::structure,
    runner::{
        Backend, Config, Eval, Extern, ExternError, Interface, Interpreter, NullInterface, Runner,
        RunnerContext, SideEffect, Spec,
    },
};

struct EffectExtern;

impl Extern for EffectExtern {
    fn eval_rel<Interp, Iface>(
        _ctx: &mut RunnerContext<'_, Interp, Iface, Self>,
        _name: &str,
        values: &[Value],
    ) -> Eval<Vec<Value>, ExternError>
    where
        Iface: Interface,
        Interp: Interpreter<Iface, Self>,
    {
        Eval::new(Ok(values.to_vec()), SideEffect::Impure)
    }

    fn eval_func<Interp, Iface>(
        _ctx: &mut RunnerContext<'_, Interp, Iface, Self>,
        _name: &str,
        _targs: &[Typ],
        values: &[Value],
    ) -> Eval<Value, ExternError>
    where
        Iface: Interface,
        Interp: Interpreter<Iface, Self>,
    {
        Eval::new(Ok(values[0]), SideEffect::Impure)
    }

    fn clear(&mut self) {}
}

/// Checks tail callers as well as the final callee, including cached suffixes.
#[test]
fn tail_call_cache_preserves_effect_boundaries() {
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/tail-cache.watsup");
    let spec = structure::convert(crate::algo(&[path]).unwrap(), true).unwrap();
    for guard in [false, true] {
        for cache in [false, true] {
            let mut arena = Arena::new();
            let mut runner = Runner::new(
                &mut arena,
                Spec::Sl(spec.clone()),
                Config::new(cache, false, guard),
                NullInterface,
                EffectExtern,
            )
            .unwrap();
            let value = make::nat(&mut arena, 1u64.into(), Span::default()).unwrap();
            // Both invokers must retain every pure caller, including cache hits
            for rel in [false, true] {
                for pure in [true, false] {
                    let eval = if rel {
                        runner.eval_rel(&mut arena, if pure { "Pure" } else { "Impure" }, &[value])
                    } else {
                        let eval = runner.eval_func(
                            &mut arena,
                            if pure { "pure" } else { "impure" },
                            &[],
                            &[],
                        );
                        Eval::new(eval.result.map(|value| vec![value]), eval.effect)
                    };
                    let values = eval.result.unwrap();
                    assert_eq!(
                        get::num(&arena, &values[0]).unwrap(),
                        &crate::lang::common::prim::num::Number::Nat(2u64.into())
                    );
                    assert_eq!(
                        eval.effect,
                        if pure { SideEffect::Pure } else { SideEffect::Impure }
                    );
                    let Backend::Sl { interp, .. } = &runner.backend else { unreachable!() };
                    // A pure suffix remains cacheable after an effectful prefix
                    let names = if pure {
                        vec!["leaf", "middle", "head", "other"]
                    } else {
                        vec!["leaf", "middle"]
                    };
                    for name in names {
                        let found = if rel {
                            let name = format!("{}{}", name[..1].to_uppercase(), &name[1..]);
                            interp.cache.find_rel(&arena, &name, &[value]).is_some()
                        } else {
                            interp.cache.find_func(&arena, name, &[value]).is_some()
                        };
                        assert_eq!(found, cache, "{name}: rel={rel}, pure={pure}");
                    }
                    // Neither effectful prefixes nor suffixes may be memoized
                    if !pure {
                        for name in ["prefix", "suffix", "outer"] {
                            assert!(interp.cache.find_func(&arena, name, &[value]).is_none());
                            let name = format!("{}{}", name[..1].to_uppercase(), &name[1..]);
                            assert!(interp.cache.find_rel(&arena, &name, &[value]).is_none());
                        }
                    }
                }
            }
        }
    }
}
