//! The K runtime of the definition, as the extern of the K specification
//!
//! `Call_hook` (`spec-k/2-relation.watsup`) applies a hooked symbol
//! of a domain module by the K runtime of the definition (`llvm`):
//! the arguments, domain values and constructor terms, are decoded as KORE
//! text (`codec`), the runtime evaluates the application, and its answer,
//! binary KORE (`binary`), is encoded as a term again (`codec`).
//! `$float_literal` has the runtime write a Float literal in its output
//! format, e.g. `0.5` as `0.500000000000000000000000000000000000000e0`.

use std::{path::Path, rc::Rc};

use crate::lang::{
    common::source::Span,
    data::{
        arena::Arena,
        typ,
        value::flat::{self as value, Value},
    },
    il::ast::Typ,
};

use crate::runner::{Eval, Extern, ExternError, Interface, Interpreter, RunnerContext, SideEffect};

use super::{
    Loaded,
    ast::{Pattern, Sort},
    binary, codec,
    llvm::Runtime,
};

/// The K runtime of one definition, with what writing terms to it needs.
#[derive(Default)]
pub struct KoreExtern {
    /// The loaded definition, shared with the interface that loaded it.
    loaded: Option<Rc<Loaded>>,
    /// The K runtime of the definition.
    runtime: Option<Runtime>,
}

impl KoreExtern {
    /// Opens the K runtime of the definition, a library made by
    /// `llvm-kompile definition.kore dt c`.
    pub fn open_runtime(&mut self, path: &Path) -> Result<(), String> {
        self.runtime = Some(Runtime::open(path)?);
        Ok(())
    }

    /// Takes the definition the interface loaded.
    pub fn attach(&mut self, loaded: Option<Rc<Loaded>>) {
        self.loaded = loaded;
    }

    fn ready(&self, what: &str) -> Result<(&Loaded, &Runtime), String> {
        match (&self.loaded, &self.runtime) {
            (Some(loaded), Some(runtime)) => Ok((loaded, runtime)),
            (None, _) => Err(format!("{what} needs a loaded definition")),
            (_, None) => Err(format!("{what} needs the K runtime of the definition (--lib)")),
        }
    }

    /// `Call_hook: |- symid sort* sort term* : res<term>`: the hooked symbol
    /// applied to the terms, by the K runtime; `ERROR` if the runtime fails.
    fn call_hook(&self, arena: &mut Arena, values: &[Value]) -> Result<Value, String> {
        let (loaded, runtime) = self.ready("Call_hook")?;
        let (pattern, sort) =
            codec::decode::hook_args(arena, &loaded.info, &loaded.shapes, values)?;
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
        codec::encode::hook_result(arena, &loaded.info, &loaded.shapes, answer.as_ref())
    }

    /// `$float_literal(text) : text?`: a Float literal in the runtime's
    /// output format.
    fn float_literal(&self, arena: &mut Arena, values: &[Value]) -> Result<Value, String> {
        let (loaded, runtime) = self.ready("$float_literal")?;
        let [text] = values else {
            return Err(format!("$float_literal takes 1 argument, not {}", values.len()));
        };
        let text = value::get::text(arena, text)
            .map_err(|e| e.to_string())?
            .to_owned();
        // The literal as a Float domain value, evaluated by the runtime
        let sort = Sort::Sort(loaded.info.sort_of_hook("FLOAT.Float")?.to_owned(), vec![]);
        let literal = Pattern::App("\\dv".to_owned(), vec![sort.clone()], vec![Pattern::Str(text)]);
        let normal = match runtime.simplify(&literal.to_string(), &sort) {
            Ok(result) => match unwrap_result(binary::decode(&result)?) {
                Pattern::App(dv, _, args) if dv == "\\dv" => match args.as_slice() {
                    [Pattern::Str(normal)] => Some(normal.clone()),
                    _ => None,
                },
                _ => None,
            },
            Err(_) => None,
        };
        let normal = normal
            .map(|normal| value::make::text(arena, normal, Span::default()))
            .transpose()
            .map_err(|e| e.to_string())?;
        let typ = typ::make::opt(typ::make::text()).node.into();
        value::make::opt(arena, typ, normal, Span::default()).map_err(|e| e.to_string())
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
        ctx: &mut RunnerContext<'_, Interp, Iface, Self>,
        name: &str,
        _targs: &[Typ],
        values: &[Value],
    ) -> Eval<Value, ExternError>
    where
        Iface: Interface,
        Interp: Interpreter<Iface, Self>,
    {
        let (arena, external) = ctx.arena_and_external_mut();
        eval(match name {
            "float_literal" => external.float_literal(arena, values),
            _ => Err(format!("unknown extern function {name}")),
        })
    }

    /// The definition and runtime stay for the next program.
    fn clear(&mut self) {}
}
