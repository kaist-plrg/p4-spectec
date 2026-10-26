//! The K runtime of the definition, as the extern of the K specification
//!
//! `Call_hook` (`spec-k/2-relation.watsup`) applies a hooked symbol
//! of a domain module by the K runtime of the definition (`llvm`):
//! the arguments, domain values and constructor terms, are decoded as KORE
//! text (`decode`), the runtime evaluates the application, and its answer,
//! binary KORE (`binary`), is encoded as a term again (`encode`). As the
//! relay codec of P4-SpecTec² (`metacirc/codec.rs`), `hook_args` and
//! `hook_result` take the arguments and give the result at this boundary.
//! `float_literal` has the runtime write a Float literal of the definition
//! in its output format, e.g. `0.5` as
//! `0.500000000000000000000000000000000000000e0` (`encode`).

use std::rc::Rc;

use crate::lang::{
    data::{
        arena::Arena,
        value::flat::{self as value, Value},
    },
    il::ast::Typ,
};

use crate::runner::{Eval, Extern, ExternError, Interface, Interpreter, RunnerContext, SideEffect};

use super::{
    Loaded,
    ast::{Pattern, Sort},
    binary, decode, encode,
    info::Info,
    llvm::Runtime,
};

/// The K runtime of one definition, with what writing terms to it needs.
#[derive(Default)]
pub struct KoreExtern {
    /// The loaded definition and its runtime, shared with the interface.
    loaded: Option<Rc<Loaded>>,
}

impl KoreExtern {
    /// Takes the definition the interface loaded.
    pub fn attach(&mut self, loaded: Option<Rc<Loaded>>) {
        self.loaded = loaded;
    }

    /// `Call_hook: |- symid sort* sort term* : res<term>`: the hooked symbol
    /// applied to the terms, by the K runtime; `ERROR` if the runtime fails.
    fn call_hook(&self, arena: &mut Arena, values: &[Value]) -> Result<Value, String> {
        let loaded = self
            .loaded
            .as_deref()
            .ok_or("Call_hook needs a loaded definition")?;
        let runtime = runtime(loaded.runtime.as_deref(), "Call_hook")?;
        let (pattern, sort) = hook_args(arena, loaded, values)?;
        let trace = std::env::var_os("KORE_TRACE").is_some();
        if trace {
            eprintln!("hook> {pattern} : {sort}");
        }
        // The runtime's answer as a term; a failure there is ERROR
        let answer = match runtime.simplify(&pattern.to_string(), &sort) {
            Ok(result) => {
                let result = unwrap_result(binary::decode(&result)?);
                if trace {
                    eprintln!("hook< {result}");
                }
                Some(result)
            }
            Err(message) => {
                if trace {
                    eprintln!("hook failed: {pattern}: {message}");
                }
                None
            }
        };
        hook_result(arena, loaded, answer.as_ref())
    }
}

/// The arguments of `Call_hook`, the hooked symbol, its sort parameters, its
/// result sort, and the terms it applies to, as the application to evaluate
/// and its sort.
fn hook_args(arena: &Arena, loaded: &Loaded, values: &[Value]) -> Result<(Pattern, Sort), String> {
    let [symbol, sorts, sort, terms] = values else {
        return Err(format!("Call_hook takes 4 inputs, not {}", values.len()));
    };
    let list = |value| value::get::list(arena, value).map_err(|e| e.to_string());
    let symbol = value::get::text(arena, symbol)
        .map_err(|e| e.to_string())?
        .to_owned();
    let sorts = list(sorts)?
        .iter()
        .map(|sort| decode::sort(arena, &loaded.mixops, sort))
        .collect::<Result<_, _>>()?;
    let sort = decode::sort(arena, &loaded.mixops, sort)?;
    let terms = list(terms)?
        .iter()
        .map(|term| decode::term(arena, &loaded.info, &loaded.mixops, term))
        .collect::<Result<_, _>>()?;
    Ok((Pattern::App(symbol, sorts, terms), sort))
}

/// The result of `Call_hook`, of `res<term>`: `OK` the runtime's answer as a
/// term, or `ERROR` if it failed.
fn hook_result(
    arena: &mut Arena,
    loaded: &Loaded,
    answer: Option<&Pattern>,
) -> Result<Value, String> {
    match answer {
        Some(answer) => {
            let term = encode::term(arena, &loaded.info, &loaded.mixops, answer)?;
            encode::ok(arena, &loaded.mixops, term)
        }
        None => encode::error(arena, &loaded.mixops),
    }
}

/// The runtime, which `what` needs.
fn runtime<'a>(runtime: Option<&'a Runtime>, what: &str) -> Result<&'a Runtime, String> {
    runtime.ok_or_else(|| format!("{what} needs the K runtime of the definition (--lib)"))
}

/// A Float literal in the runtime's output format: the runtime evaluates it
/// as a domain value.
pub fn float_literal(runtime: Option<&Runtime>, info: &Info, text: &str) -> Result<String, String> {
    let runtime = self::runtime(runtime, "a Float literal")?;
    let sort = Sort::Sort(info.sort_of_hook("FLOAT.Float")?.to_owned(), vec![]);
    let literal =
        Pattern::App("\\dv".to_owned(), vec![sort.clone()], vec![Pattern::Str(text.into())]);
    let bad = || format!("bad Float literal {text:?}");
    let result = runtime
        .simplify(&literal.to_string(), &sort)
        .map_err(|_| bad())?;
    match unwrap_result(binary::decode(&result)?) {
        Pattern::App(dv, _, args) if dv == "\\dv" => match args.as_slice() {
            [Pattern::Str(normal)] => Ok(normal.clone()),
            _ => Err(bad()),
        },
        _ => Err(bad()),
    }
}

/// The result of `kore_simplify` without its wrapping: the runtime gives
/// `rawTerm{}(inj{S, SortKItem{}}(v))` for a domain value `v` of sort `S`.
fn unwrap_result(pattern: Pattern) -> Pattern {
    let mut pattern = pattern;
    for wrapper in ["rawTerm", "inj"] {
        pattern = match pattern {
            Pattern::App(name, _, mut args) if name == wrapper && args.len() == 1 => args.remove(0),
            pattern => pattern,
        };
    }
    pattern
}

/// A failure of the boundary itself ends the run; the runtime's own
/// failures are `ERROR` values and pure.
fn eval<T>(result: Result<T, String>) -> Eval<T, ExternError> {
    let effect = if result.is_err() { SideEffect::Impure } else { SideEffect::Pure };
    Eval::new(result.map_err(ExternError::diagnostic_message), effect)
}

impl Extern for KoreExtern {
    fn eval_rel<Interp, Iface>(
        ctx: &mut RunnerContext<'_, Interp, Iface, Self>,
        name: &str,
        values: &[Value],
    ) -> Eval<Vec<Value>, ExternError>
    where
        Iface: Interface,
        Interp: Interpreter<Iface, Self>,
    {
        let (arena, external) = ctx.arena_and_external_mut();
        eval(match name {
            "Call_hook" => external.call_hook(arena, values).map(|result| vec![result]),
            _ => Err(format!("unknown extern relation {name}")),
        })
    }

    fn eval_func<Interp, Iface>(
        _ctx: &mut RunnerContext<'_, Interp, Iface, Self>,
        name: &str,
        _targs: &[Typ],
        _values: &[Value],
    ) -> Eval<Value, ExternError>
    where
        Iface: Interface,
        Interp: Interpreter<Iface, Self>,
    {
        eval(Err(format!("unknown extern function {name}")))
    }

    /// The definition and runtime stay for the next program.
    fn clear(&mut self) {}
}
