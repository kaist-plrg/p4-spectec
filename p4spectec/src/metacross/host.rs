//! The host that K and Redex call back into: one runner for the process
//!
//! `init` builds the SL runner of a specification, and `eval` answers the
//! JSON requests of `wire`: builtins go to the interface directly, extern
//! functions and relations to the loaded specification. The runner holds
//! `Rc` data, so the host is thread-local; both consumers call from one
//! thread. The runner is never reset between requests: `fresh_typeId`'s
//! counter runs for the whole process, and the arena grows with it.

use std::{cell::RefCell, path::PathBuf};

use serde_json::Value as Json;

use crate::{
    diagnostic::{ColorChoice, DisplayStyle, RenderConfig, Renderer, Report},
    interface::p4::P4Interface,
    lang::data::arena::Arena,
    pass::SpecLang,
    runner::{self, Config, InterpreterError, Runner, Spec},
    sim_plugin::dummy::Dummy,
};

use super::wire::{self, Request};

/// The arena and the runner prepared in it.
struct Host {
    arena: Arena,
    runner: Runner<P4Interface, Dummy>,
}

thread_local! {
    static HOST: RefCell<Option<Host>> = const { RefCell::new(None) };
}

fn render(report: &Report) {
    if let Err(error) = Renderer::new(RenderConfig::default()).render_to_stderr(report) {
        eprintln!("{report}\ndiagnostic rendering failed: {error}");
    }
}

fn render_to_string(report: &Report) -> String {
    let config = RenderConfig {
        frame_style: Some(DisplayStyle::Short),
        color: ColorChoice::Never,
        ..RenderConfig::default()
    };
    Renderer::new(config)
        .render_to_string(report)
        .unwrap_or_else(|error| format!("{report}\ndiagnostic rendering failed: {error}"))
        .trim_end()
        .to_owned()
}

// == Initialization

/// Builds the runner for a `.watsup` file or a directory of them, replacing any earlier one.
///
/// Diagnostics are rendered to stderr; the error text names the path.
pub fn init(path_spec: &str) -> Result<(), String> {
    let (result, warnings) =
        crate::runner_spec_with_warnings(SpecLang::Sl, &[PathBuf::from(path_spec)]);
    for warning in &warnings {
        render(warning);
    }
    let spec = result.map_err(|report| {
        render(&report);
        format!("cannot lower {path_spec} to SL")
    })?;
    let Spec::Sl(spec_sl) = spec else {
        return Err(format!("{path_spec} did not lower to SL"));
    };
    let mut arena = Arena::new();
    let runner = runner::build_sl(&mut arena, spec_sl, Config::new(true, false, false), Dummy)
        .map_err(|report| {
            render(&report);
            format!("cannot build a runner for {path_spec}")
        })?;
    HOST.with(|host| *host.borrow_mut() = Some(Host { arena, runner }));
    Ok(())
}

// == Requests

impl Host {
    /// Answers one request; a host error becomes the `error` reply.
    fn answer(&mut self, request: &str) -> Result<Json, Box<Report>> {
        let arena = &mut self.arena;
        match wire::request_of_json(arena, request)? {
            Request::Builtin { id, targs, args } => {
                let value = self
                    .runner
                    .call_builtin(arena, &id, &targs, &args)
                    .map_err(InterpreterError::into_report)?;
                Ok(wire::response_ok(arena, value))
            }
            Request::ExternFunc { id, targs, args } => {
                match self.runner.eval_func(arena, &id.node, &targs, &args).result {
                    Ok(value) => Ok(wire::response_ok(arena, value)),
                    Err(error @ InterpreterError::Mismatch(_)) => {
                        eprintln!("ffi: extern func {} failed", id.node);
                        render(&error.into_report());
                        Ok(wire::response_fail())
                    }
                    Err(InterpreterError::Fatal(report)) => Err(report),
                }
            }
            Request::ExternRel { id, args } => {
                match self.runner.eval_rel(arena, &id.node, &args).result {
                    Ok(values) => Ok(wire::response_ok_values(arena, &values)),
                    Err(error @ InterpreterError::Mismatch(_)) => {
                        eprintln!("ffi: extern rel {} failed", id.node);
                        render(&error.into_report());
                        Ok(wire::response_fail())
                    }
                    Err(InterpreterError::Fatal(report)) => Err(report),
                }
            }
        }
    }
}

/// The reply for a failed request, also noted on stderr.
pub fn error_reply(message: &str) -> String {
    eprintln!("ffi: {message}");
    wire::response_error(message).to_string()
}

/// Answers one JSON request with one JSON reply; never panics on its own.
pub fn eval(request: &str) -> String {
    HOST.with(|host| {
        let mut host = host.borrow_mut();
        let Some(host) = host.as_mut() else {
            return error_reply("host_eval called before host_init");
        };
        match host.answer(request) {
            Ok(reply) => reply.to_string(),
            Err(report) => error_reply(&render_to_string(&report)),
        }
    })
}
