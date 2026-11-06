//! The K library compiled from spec-meta-k, executing a K base
//!
//! ST-in-K runs spec-meta-k, the AL semantics written in K, as the base.
//! `RuntimeK` loads the library `kompile` made of it, once per tower;
//! each reentry depth owns a `StateK`, a K thread running
//! spec-meta-k's `serve()` loop, and lends it as a `EngineK`.
//! A call sends a request to that thread as JSON text and waits;
//! a `#ffiCall` the thread makes while serving comes back to the calling thread,
//! which answers it through `UpperMut::host_call` and sends the reply back.
//! Arena values never cross threads, only JSON text does.
//! `ProtocolStK` makes the requests and reads the answers;
//! the engine only carries them, and loads C into a depth's thread on first use.

use super::{
    ffi::{self, Message, Worker},
    runtime::RuntimeK,
};
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
        protocol::ProtocolStK,
        tower::{HostLevel, UpperMut},
    },
    runner::{Extern, InterpreterError, SideEffect},
};
use std::{
    sync::mpsc::{Receiver, RecvError, TryRecvError},
    time::{Duration, Instant},
};

/// Reports a K thread that cannot serve a call.
fn failed(message: impl Into<String>) -> Box<Report> {
    let code = Some("metacirc/k-engine-failed".into());
    Box::new(Diagnostic::new("metacirc", Severity::Error, code, message, vec![], vec![]).into())
}

/// How long a receiver polls before it sleeps; waking a sleeping thread
/// costs about 110 µs, and a reply often arrives sooner.
const SPIN: Duration = Duration::from_micros(200);

/// Receives a message, polling briefly before blocking.
pub(super) fn receive<T>(receiver: &Receiver<T>) -> std::result::Result<T, RecvError> {
    let start = Instant::now();
    while start.elapsed() < SPIN {
        match receiver.try_recv() {
            Ok(message) => return Ok(message),
            Err(TryRecvError::Empty) => std::hint::spin_loop(),
            Err(TryRecvError::Disconnected) => return Err(RecvError),
        }
    }
    receiver.recv()
}

// == Engine

/// spec-meta-k at one depth: the loaded library and that depth's K thread.
pub(in crate::metacirc) struct EngineK<'s> {
    runtime: &'s RuntimeK,
    state: &'s mut StateK,
}

impl<'s> EngineK<'s> {
    /// Lends a depth's K thread, running the library of `runtime`.
    pub(in crate::metacirc) fn lend(runtime: &'s RuntimeK, state: &'s mut StateK) -> Self {
        Self { runtime, state }
    }
}

/// Every call sends JSON text to the depth's K thread and returns JSON text:
/// `ProtocolStK` makes the requests and reads the responses.
impl Engine for EngineK<'_> {
    /// Sends relation `name` with JSON text `values` to `serve()`,
    /// answering each `#ffiCall` of the K thread with
    /// `upper.host_call(arena, "host_eval", &[request])` until the response arrives.
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
            (ProtocolStK::LOAD, [value_request]) => {
                let key = self.runtime.register(text(arena, value_request)?);
                self.ensure_loaded(arena, upper, effects, key)?;
                Ok(vec![make_text(arena, key.to_string())?])
            }
            (ProtocolStK::SERVE, [value_ctx, value_request]) => {
                Ok(vec![self.serve(arena, upper, effects, *value_ctx, *value_request)?])
            }
            (ProtocolStK::ENTRY_REL | ProtocolStK::ENTRY_FUNC, _) => {
                self.enter(arena, upper, effects, name, values)
            }
            _ => Err(unsupported(name)),
        }
    }

    /// Sends function `name` to `serve()`, as `call_rel` does.
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
            (ProtocolStK::SERVE, [], [value_ctx, value_request]) => {
                self.serve(arena, upper, effects, *value_ctx, *value_request)
            }
            _ => Err(unsupported(name)),
        }
    }

    /// The K thread memoizes nothing within a program.
    fn clear(&mut self) {}

    /// Restarts the K thread if it stopped.
    ///
    /// A specification it loaded stays: C names a load request by its text,
    /// so the thread holds what loading it again would give.
    fn reset(&mut self) {
        // A thread that cannot start fails the next call instead
        let _ = self.state.worker(self.runtime);
    }
}

impl EngineK<'_> {
    /// Runs spec-meta-k's entry `name`: loads its specification, then calls it.
    fn enter<Ext: Extern>(
        &mut self,
        arena: &mut Arena,
        upper: &mut UpperMut<'_, Ext>,
        effects: &mut u64,
        name: &str,
        values: &[Value],
    ) -> std::result::Result<Vec<Value>, InterpreterError> {
        let [request_load, request_call] = ProtocolStK::entry_requests(arena, name, values)?;
        let key = self.runtime.register(&request_load);
        self.ensure_loaded(arena, upper, effects, key)?;
        let answer = self.exchange(arena, upper, effects, request_call)?;
        let value_answer = make_text(arena, answer)?;
        ProtocolStK::entry_outputs(arena, name, value_answer)
    }

    /// Sends a request under C, loading C's specification into the thread first if needed.
    fn serve<Ext: Extern>(
        &mut self,
        arena: &mut Arena,
        upper: &mut UpperMut<'_, Ext>,
        effects: &mut u64,
        value_ctx: Value,
        value_request: Value,
    ) -> std::result::Result<Value, InterpreterError> {
        let key = text(arena, &value_ctx)?
            .parse()
            .map_err(|_| InterpreterError::Fatal(failed("C of spec-meta-k is not a load key")))?;
        self.ensure_loaded(arena, upper, effects, key)?;
        let request = text(arena, &value_request)?.to_owned();
        let answer = self.exchange(arena, upper, effects, request)?;
        make_text(arena, answer)
    }

    /// Loads the specification of load request `key` into the thread unless it holds it.
    fn ensure_loaded<Ext: Extern>(
        &mut self,
        arena: &mut Arena,
        upper: &mut UpperMut<'_, Ext>,
        effects: &mut u64,
        key: usize,
    ) -> std::result::Result<(), InterpreterError> {
        if self.state.loaded == Some(key) {
            return Ok(());
        }
        let request = self
            .runtime
            .request(key)
            .ok_or_else(|| InterpreterError::Fatal(failed(format!("no load request {key}"))))?;
        self.state.loaded = None;
        let answer = self.exchange(arena, upper, effects, request.to_string())?;
        let value_answer = make_text(arena, answer)?;
        ProtocolStK::load_answer(arena, value_answer)?;
        self.state.loaded = Some(key);
        Ok(())
    }

    /// Sends `request` and serves the thread's host requests until its answer.
    ///
    /// A host call that fails fatally is answered with an error,
    /// which spec-meta-k turns into ERROR; the call then fails with its report.
    fn exchange<Ext: Extern>(
        &mut self,
        arena: &mut Arena,
        upper: &mut UpperMut<'_, Ext>,
        effects: &mut u64,
        request: String,
    ) -> std::result::Result<String, InterpreterError> {
        let worker = self.state.worker(self.runtime)?;
        if worker.to_k.send(request).is_err() {
            return Err(self.state.stop(None));
        }
        let mut report_fatal = None;
        loop {
            let worker = self
                .state
                .worker
                .as_ref()
                .expect("the worker answers this call");
            let response = match receive(&worker.from_k) {
                Ok(Message::Answer(answer)) => {
                    return match report_fatal {
                        Some(report) => Err(InterpreterError::Fatal(report)),
                        None => Ok(answer),
                    };
                }
                // The thread waits for a response even if the host call fails
                Ok(Message::Request(request)) => host_call(arena, upper, effects, request)
                    .unwrap_or_else(|report| {
                        let response = ProtocolStK::error_response(&report);
                        report_fatal.get_or_insert(report);
                        response
                    }),
                Ok(Message::Stopped(dump)) => return Err(self.state.stop(Some(dump))),
                Err(RecvError) => return Err(self.state.stop(None)),
            };
            let worker = self
                .state
                .worker
                .as_ref()
                .expect("the worker answers this call");
            if worker.to_k.send(response).is_err() {
                return Err(self.state.stop(None));
            }
        }
    }
}

/// Answers a request of a K thread through the levels above, recording its effect;
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
    let eval = upper.host_call(arena, ProtocolStK::HOST_EVAL, &[value_request]);
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

/// Reads a JSON text value.
fn text<'a>(arena: &'a Arena, value: &Value) -> std::result::Result<&'a str, InterpreterError> {
    get::text(arena, value).map_err(|error| InterpreterError::Fatal(failed(error.to_string())))
}

/// Makes a JSON text value.
fn make_text(arena: &mut Arena, text: String) -> std::result::Result<Value, InterpreterError> {
    make::text(arena, text, Span::default())
        .map_err(|error| InterpreterError::Fatal(failed(error.to_string())))
}

/// Rejects a call that is not one of spec-meta-k's engine calls.
fn unsupported(name: &str) -> InterpreterError {
    let message = format!(
        "spec-meta-k's engine serves {}, {}, {}, and {}, not {name}",
        ProtocolStK::ENTRY_REL,
        ProtocolStK::ENTRY_FUNC,
        ProtocolStK::LOAD,
        ProtocolStK::SERVE
    );
    InterpreterError::Fatal(failed(message))
}

// == Depth state

/// The K thread of one depth, serving requests until the depth is dropped.
///
/// Each thread initializes the K runtime for itself (`kllvm_init`),
/// since the K heap and its collector are per thread.
pub(in crate::metacirc) struct StateK {
    /// The thread, until it is found stopped.
    worker: Option<Worker>,
    /// The key of the load request the thread ran last, if its global layer still holds it.
    loaded: Option<usize>,
}

impl StateK {
    /// Starts a depth's K thread on the library of `runtime`.
    pub(in crate::metacirc) fn new(runtime: &RuntimeK) -> Result<Self> {
        let mut state = Self { worker: None, loaded: None };
        state
            .worker(runtime)
            .map_err(InterpreterError::into_report)?;
        Ok(state)
    }

    /// Returns the thread, starting it if there is none.
    fn worker(&mut self, runtime: &RuntimeK) -> std::result::Result<&Worker, InterpreterError> {
        if self.worker.is_none() {
            let path = runtime.path().display();
            let worker = ffi::spawn(runtime.library).map_err(|error| {
                InterpreterError::Fatal(failed(format!(
                    "cannot start a K thread on {path}: {error}"
                )))
            })?;
            // The configuration says it is ready before the first call
            match receive(&worker.from_k) {
                Ok(Message::Answer(answer)) if answer == r#"{"ready":null}"# => {}
                Ok(Message::Stopped(dump)) => {
                    let _ = worker.thread.join();
                    return Err(InterpreterError::Fatal(stopped(Some(dump))));
                }
                _ => {
                    let message = format!("the K thread on {path} did not start serving");
                    return Err(InterpreterError::Fatal(failed(message)));
                }
            }
            self.worker = Some(worker);
            self.loaded = None;
        }
        Ok(self.worker.as_ref().expect("the worker was just started"))
    }

    /// Forgets a thread that stopped, which the next call restarts.
    fn stop(&mut self, dump: Option<String>) -> InterpreterError {
        if let Some(worker) = self.worker.take() {
            drop(worker.to_k);
            let _ = worker.thread.join();
        }
        self.loaded = None;
        InterpreterError::Fatal(stopped(dump))
    }
}

/// Reports a K thread that stopped, with the head of its `<k>` cell if it got stuck.
fn stopped(dump: Option<String>) -> Box<Report> {
    let Some(dump) = dump else {
        return failed("the K thread stopped");
    };
    let start = dump.find("Lbl'-LT-'k'-GT-'").unwrap_or(0);
    let excerpt = dump[start..].chars().take(2000).collect::<String>();
    failed(format!("spec-meta-k got stuck: {excerpt}"))
}

impl Drop for StateK {
    /// Tells the thread to exit and waits for it.
    fn drop(&mut self) {
        if let Some(Worker { to_k, from_k, thread }) = self.worker.take() {
            let _ = to_k.send(ProtocolStK::exit_request());
            drop(to_k);
            drop(from_k);
            let _ = thread.join();
        }
    }
}
