//! A `racket` process running spec-meta-redex, executing a Redex base
//!
//! ST-in-Redex runs spec-meta-redex, the AL semantics written in PLT Redex,
//! as the base. `RuntimeRedex` runs its entry module in one `racket` process
//! per tower, serving every reentry depth; a depth's `StateRedex`
//! is lent as a `EngineRedex`.
//! A call writes a request to the process as one line of s-expression text and
//! reads lines until its answer; a host request the process makes while serving
//! is answered through `UpperMut::host_call`, and the response written back.
//! Arena values never leave this process, only s-expression text does.
//! `ProtocolStRedex` makes the requests and reads the answers;
//! the engine only carries them, and loads C into the process when it holds
//! another specification.

use super::runtime::{Line, RuntimeRedex};
use crate::metacirc::engine::Engine;
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
    metacirc::{
        Result,
        protocol::ProtocolStRedex,
        tower::{HostLevel, UpperMut},
    },
    runner::{Extern, InterpreterError, SideEffect},
};

/// Reports a process that cannot serve a call.
pub(super) fn failed(message: impl Into<String>) -> Box<Report> {
    let code = Some("metacirc/redex-engine-failed".into());
    Box::new(Diagnostic::new("metacirc", Severity::Error, code, message, vec![], vec![]).into())
}

// == Engine

/// spec-meta-redex at one depth, on the tower's process.
pub(in crate::metacirc) struct EngineRedex<'s> {
    runtime: &'s RuntimeRedex,
}

impl<'s> EngineRedex<'s> {
    /// Lends a depth's state, running on the process of `runtime`.
    pub(in crate::metacirc) fn lend(runtime: &'s RuntimeRedex, _state: &'s mut StateRedex) -> Self {
        Self { runtime }
    }
}

/// Every call sends s-expression text to the process and returns s-expression text:
/// `ProtocolStRedex` makes the requests and reads the responses.
impl Engine for EngineRedex<'_> {
    /// Sends relation `name` with s-expression text `values` to the serve loop,
    /// answering each host request of the process with
    /// `upper.host_call(arena, "host_eval", &[request])` until the answer arrives.
    ///
    /// `load(request)` loads a specification and returns C, its key in the runtime;
    /// `serve(C, request)` sends a request under C and returns the answer.
    /// The entries `Eval_AL_rel(spec, id, args)` and `Eval_AL_func(spec, id, args)`
    /// load `spec` and call `id` on `args`, returning the outputs of the call.
    fn call_rel<Ext: Extern>(
        &mut self,
        arena: &mut Arena,
        upper: &mut UpperMut<'_, Ext>,
        effects: &mut u64,
        name: &str,
        values: &[Value],
    ) -> std::result::Result<Vec<Value>, InterpreterError> {
        match (name, values) {
            (ProtocolStRedex::LOAD, [value_request]) => {
                let key = self.runtime.register(text(arena, value_request)?);
                self.ensure_loaded(arena, upper, effects, key)?;
                Ok(vec![make_text(arena, key.to_string())?])
            }
            (ProtocolStRedex::SERVE, [value_ctx, value_request]) => {
                Ok(vec![self.serve(arena, upper, effects, *value_ctx, *value_request)?])
            }
            (ProtocolStRedex::ENTRY_REL | ProtocolStRedex::ENTRY_FUNC, _) => {
                self.enter(arena, upper, effects, name, values)
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
            (ProtocolStRedex::SERVE, [], [value_ctx, value_request]) => {
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

impl EngineRedex<'_> {
    /// Runs spec-meta-redex's entry `name`: loads its specification, then calls it.
    fn enter<Ext: Extern>(
        &mut self,
        arena: &mut Arena,
        upper: &mut UpperMut<'_, Ext>,
        effects: &mut u64,
        name: &str,
        values: &[Value],
    ) -> std::result::Result<Vec<Value>, InterpreterError> {
        let [request_load, request_call] = ProtocolStRedex::entry_requests(arena, name, values)?;
        let key = self.runtime.register(&request_load);
        self.ensure_loaded(arena, upper, effects, key)?;
        let answer = self.exchange(arena, upper, effects, &request_call)?;
        ProtocolStRedex::entry_outputs(arena, name, &answer)
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
        ProtocolStRedex::load_answer(arena, &answer)?;
        self.runtime.set_loaded(Some(key));
        Ok(())
    }

    /// Sends `request` and serves the process's host requests until its answer.
    ///
    /// A host call that fails fatally is answered with ERROR,
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
                            report_fatal.get_or_insert(report);
                            ProtocolStRedex::ERROR_RESPONSE.to_owned()
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

/// Answers a request of the process through the levels above, recording its effect;
/// the boundary caches the responses of pure calls that succeeded.
fn host_call<Ext: Extern>(
    arena: &mut Arena,
    upper: &mut UpperMut<'_, Ext>,
    effects: &mut u64,
    request: String,
) -> std::result::Result<String, Box<Report>> {
    // The same request to a pure call that succeeded gets its cached response
    if let Some(response) = upper.protocol(HostLevel::FIRST).host_response(&request) {
        return Ok(response);
    }
    let value_request = make_text(arena, request.clone()).map_err(InterpreterError::into_report)?;
    let eval = upper.host_call(arena, ProtocolStRedex::HOST_EVAL, &[value_request]);
    if eval.effect == SideEffect::Impure {
        *effects = effects.saturating_add(1);
    }
    let response = match &eval.result.map_err(|error| error.into_report())?[..] {
        [value_response] => text(arena, value_response)
            .map_err(InterpreterError::into_report)?
            .to_owned(),
        _ => return Err(failed("a host call answers with one response")),
    };
    if eval.effect == SideEffect::Pure {
        upper
            .protocol(HostLevel::FIRST)
            .cache_host_response(request, &response);
    }
    Ok(response)
}

/// Reads a text value.
fn text<'a>(arena: &'a Arena, value: &Value) -> std::result::Result<&'a str, InterpreterError> {
    get::text(arena, value).map_err(|error| InterpreterError::Fatal(failed(error.to_string())))
}

/// Makes a text value.
fn make_text(arena: &mut Arena, text: String) -> std::result::Result<Value, InterpreterError> {
    make::text(arena, text, Span::default())
        .map_err(|error| InterpreterError::Fatal(failed(error.to_string())))
}

/// Rejects a call that is not one of spec-meta-redex's engine calls.
fn unsupported(name: &str) -> InterpreterError {
    let message = format!(
        "spec-meta-redex's engine serves {}, {}, {}, and {}, not {name}",
        ProtocolStRedex::ENTRY_REL,
        ProtocolStRedex::ENTRY_FUNC,
        ProtocolStRedex::LOAD,
        ProtocolStRedex::SERVE
    );
    InterpreterError::Fatal(failed(message))
}

// == Depth state

/// The state of one depth; the process belongs to the runtime, shared by every depth.
pub(in crate::metacirc) struct StateRedex;

impl StateRedex {
    /// Makes a depth's state on the process of `runtime`.
    pub(in crate::metacirc) fn new(_runtime: &RuntimeRedex) -> Result<Self> {
        Ok(Self)
    }
}
