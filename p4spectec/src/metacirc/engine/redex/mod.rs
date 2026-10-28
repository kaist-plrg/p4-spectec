//! A `racket` process running spec-meta-redex, executing a Redex base
//!
//! ST-in-Redex runs spec-meta-redex, the AL semantics written in PLT Redex,
//! as the base. `RedexRuntime` runs its entry module in one `racket` process
//! per tower, serving every reentry depth; a depth's `RedexEngineState`
//! is lent as a `RedexEngine`.
//! A call writes a request to the process as one line of JSON text and reads
//! lines until its answer; a host request the process makes while serving
//! is answered through `UpperMut::host_call`, and the response written back.
//! Arena values never leave this process, only JSON text does.
//! `MetaRedexProtocol` makes the requests and reads the answers;
//! the engine only carries them, and loads C into the process when it holds
//! another specification.

mod runtime;

pub(in crate::metacirc) use runtime::RedexRuntime;

use super::Engine;
use crate::{
    diagnostic::{Diagnostic, Report, Severity},
    lang::{
        common::source::Span,
        data::{
            arena::Arena,
            value::flat::{Value, get, make},
        },
        il::ast::Typ,
    },
    metacirc::{Result, protocol::MetaRedexProtocol, tower::UpperMut},
    runner::{Eval, Extern, InterpreterError, SideEffect},
};
use runtime::Line;

/// The entry relation of spec-meta-redex, which runs a program's `$main()`.
const ENTRY: &str = "Eval_AL";

/// Reports a process that cannot serve a call.
fn failed(message: impl Into<String>) -> Box<Report> {
    let code = Some("metacirc/redex-engine-failed".into());
    Box::new(Diagnostic::new("metacirc", Severity::Error, code, message, vec![], vec![]).into())
}

// == Engine

/// spec-meta-redex at one depth, on the tower's process.
pub(in crate::metacirc) struct RedexEngine<'s> {
    runtime: &'s RedexRuntime,
}

impl<'s> RedexEngine<'s> {
    /// Lends a depth's state, running on the process of `runtime`.
    pub(in crate::metacirc) fn lend(
        runtime: &'s RedexRuntime,
        _state: &'s mut RedexEngineState,
    ) -> Self {
        Self { runtime }
    }
}

/// Every call sends JSON text to the process and returns JSON text:
/// `MetaRedexProtocol` makes the requests and reads the responses.
impl Engine for RedexEngine<'_> {
    /// Runs spec-meta-redex's entry `name` on `program`,
    /// the JSON text that `MetaRedexProtocol::encode_script` made.
    fn call_program<Ext: Extern>(
        &mut self,
        arena: &mut Arena,
        upper: &mut UpperMut<'_, Ext>,
        effects: &mut u64,
        name: &str,
        program: Value,
    ) -> Eval<Vec<Value>, InterpreterError> {
        let checkpoint = *effects;
        let result = self.run_program(arena, upper, effects, name, program);
        let effect = if *effects == checkpoint && *effects != u64::MAX {
            SideEffect::Pure
        } else {
            SideEffect::Impure
        };
        Eval::new(result, effect)
    }

    /// Sends relation `name` with JSON text `values` to the serve loop,
    /// answering each host request of the process with
    /// `upper.host_call(arena, "host_eval", &[request])` until the answer arrives.
    ///
    /// `load(request)` loads a specification and returns C, its key in the runtime;
    /// `serve(C, request)` sends a request under C and returns the answer.
    fn call_rel<Ext: Extern>(
        &mut self,
        arena: &mut Arena,
        upper: &mut UpperMut<'_, Ext>,
        effects: &mut u64,
        name: &str,
        values: &[Value],
    ) -> std::result::Result<Vec<Value>, InterpreterError> {
        match (name, values) {
            (MetaRedexProtocol::LOAD, [value_request]) => {
                let key = self.runtime.register(text(arena, value_request)?);
                self.ensure_loaded(arena, upper, effects, key)?;
                Ok(vec![make_text(arena, key.to_string())?])
            }
            (MetaRedexProtocol::SERVE, [value_ctx, value_request]) => {
                Ok(vec![self.serve(arena, upper, effects, *value_ctx, *value_request)?])
            }
            _ => Err(unsupported(name)),
        }
    }

    /// Sends function `name` to the serve loop, as `call_rel` does.
    fn call_func<Ext: Extern>(
        &mut self,
        arena: &mut Arena,
        upper: &mut UpperMut<'_, Ext>,
        effects: &mut u64,
        name: &str,
        targs: &[Typ],
        values: &[Value],
    ) -> std::result::Result<Value, InterpreterError> {
        match (name, targs, values) {
            (MetaRedexProtocol::SERVE, [], [value_ctx, value_request]) => {
                self.serve(arena, upper, effects, *value_ctx, *value_request)
            }
            _ => Err(unsupported(name)),
        }
    }

    /// The process memoizes nothing that a program must clear.
    fn clear(&mut self) {}

    /// Restarts the process if it stopped, which forgets what it loaded.
    ///
    /// A specification a running process loaded stays: C names a load request
    /// by its text, so the process holds what loading it again would give.
    fn reset(&mut self) {
        // A process that cannot start fails the next call instead
        let _ = self.runtime.start();
    }
}

impl RedexEngine<'_> {
    /// Runs `Eval_AL` on a program: its spec loaded, then `$main()`.
    fn run_program<Ext: Extern>(
        &mut self,
        arena: &mut Arena,
        upper: &mut UpperMut<'_, Ext>,
        effects: &mut u64,
        name: &str,
        program: Value,
    ) -> std::result::Result<Vec<Value>, InterpreterError> {
        if name != ENTRY {
            let message = format!("spec-meta-redex's entry is {ENTRY}, not {name}");
            return Err(InterpreterError::Fatal(failed(message)));
        }
        let request = MetaRedexProtocol::program_request(arena, program)?;
        let answer = self.exchange(arena, upper, effects, &request)?;
        let value_answer = make_text(arena, answer)?;
        MetaRedexProtocol::program_outputs(arena, value_answer, name)
    }

    /// Sends a request under C, loading C's specification into the process first if needed.
    fn serve<Ext: Extern>(
        &mut self,
        arena: &mut Arena,
        upper: &mut UpperMut<'_, Ext>,
        effects: &mut u64,
        value_ctx: Value,
        value_request: Value,
    ) -> std::result::Result<Value, InterpreterError> {
        let key = text(arena, &value_ctx)?.parse().map_err(|_| {
            InterpreterError::Fatal(failed("C of spec-meta-redex is not a load key"))
        })?;
        self.ensure_loaded(arena, upper, effects, key)?;
        let request = text(arena, &value_request)?.to_owned();
        let answer = self.exchange(arena, upper, effects, &request)?;
        make_text(arena, answer)
    }

    /// Loads the specification of load request `key` into the process unless it holds it.
    fn ensure_loaded<Ext: Extern>(
        &mut self,
        arena: &mut Arena,
        upper: &mut UpperMut<'_, Ext>,
        effects: &mut u64,
        key: usize,
    ) -> std::result::Result<(), InterpreterError> {
        if self.runtime.loaded() == Some(key) {
            return Ok(());
        }
        let request = self
            .runtime
            .request(key)
            .ok_or_else(|| InterpreterError::Fatal(failed(format!("no load request {key}"))))?;
        self.runtime.set_loaded(None);
        let answer = self.exchange(arena, upper, effects, &request)?;
        let value_answer = make_text(arena, answer)?;
        MetaRedexProtocol::load_answer(arena, value_answer)?;
        self.runtime.set_loaded(Some(key));
        Ok(())
    }

    /// Sends `request` and serves the process's host requests until its answer.
    ///
    /// A host call that fails fatally is answered with an error,
    /// which spec-meta-redex turns into ERROR; the call then fails with its report.
    /// The pipe is not borrowed across `upper.host_call`, which may nest an exchange.
    fn exchange<Ext: Extern>(
        &mut self,
        arena: &mut Arena,
        upper: &mut UpperMut<'_, Ext>,
        effects: &mut u64,
        request: &str,
    ) -> std::result::Result<String, InterpreterError> {
        self.runtime.send(request)?;
        let mut report_fatal = None;
        loop {
            match self.runtime.receive()? {
                Line::Serve(answer) => {
                    return match report_fatal {
                        Some(report) => Err(InterpreterError::Fatal(report)),
                        None => Ok(answer),
                    };
                }
                // The process waits for a response even if the host call fails
                Line::Eval(request) => {
                    let response =
                        host_call(arena, upper, effects, request).unwrap_or_else(|report| {
                            let response = MetaRedexProtocol::error_response(&report);
                            report_fatal.get_or_insert(report);
                            response
                        });
                    self.runtime.send(&response)?;
                }
                Line::Debug(value) => println!("{value}"),
                Line::Raise(message) => {
                    let message = format!("spec-meta-redex raised an error: {message}");
                    return Err(InterpreterError::Fatal(failed(message)));
                }
            }
        }
    }
}

/// Answers a request of the process through the levels above, recording its effect.
fn host_call<Ext: Extern>(
    arena: &mut Arena,
    upper: &mut UpperMut<'_, Ext>,
    effects: &mut u64,
    request: String,
) -> std::result::Result<String, Box<Report>> {
    let value_request = make_text(arena, request).map_err(InterpreterError::into_report)?;
    let eval = upper.host_call(arena, MetaRedexProtocol::HOST_EVAL, &[value_request]);
    if eval.effect == SideEffect::Impure {
        *effects = effects.saturating_add(1);
    }
    match &eval.result.map_err(|error| error.into_report())?[..] {
        [value_response] => Ok(text(arena, value_response)
            .map_err(InterpreterError::into_report)?
            .to_owned()),
        _ => Err(failed("a host call answers with one response")),
    }
}

/// Reads a JSON text value.
fn text<'a>(arena: &'a Arena, value: &Value) -> std::result::Result<&'a str, InterpreterError> {
    get::text(arena, value).map_err(|error| InterpreterError::Fatal(failed(error.to_string())))
}

/// Makes a JSON text value.
fn make_text(arena: &mut Arena, text: String) -> std::result::Result<Value, InterpreterError> {
    make::text(arena, text, Span::default())
        .map_err(|error| InterpreterError::Fatal(failed(error.to_string())))
}

/// Rejects a call that is not one of spec-meta-redex's engine calls.
fn unsupported(name: &str) -> InterpreterError {
    let message = format!(
        "spec-meta-redex's engine serves {} and {}, not {name}",
        MetaRedexProtocol::LOAD,
        MetaRedexProtocol::SERVE
    );
    InterpreterError::Fatal(failed(message))
}

// == Depth state

/// The state of one depth; the process belongs to the runtime, shared by every depth.
pub(in crate::metacirc) struct RedexEngineState;

impl RedexEngineState {
    /// Makes a depth's state on the process of `runtime`.
    pub(in crate::metacirc) fn new(_runtime: &RedexRuntime) -> Result<Self> {
        Ok(Self)
    }
}
