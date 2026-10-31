//! The host that K and Redex call back into: one runner for the process
//!
//! `init` builds the SL runner of a specification, and `eval` answers the
//! JSON requests of `json`: builtins go to the interface directly, extern
//! functions and relations to the loaded specification. The runner holds
//! `Rc` data, so the host is thread-local; both consumers call from one
//! thread. The runner is never reset between requests: `fresh_typeId`'s
//! counter runs for the whole process, and the arena grows with it.

use std::{cell::RefCell, path::PathBuf};

use crate::{
    diagnostic::{Diagnostic, Severity},
    interface::p4::P4Interface,
    lang::{data::arena::Arena, il::ast::Id},
    pass::SpecLang,
    runner::{self, Config, Eval, InterpreterError, Runner, Spec},
    sim_plugin::dummy::Dummy,
};

use super::{
    Error, Result,
    error::{render, render_to_string},
    json::{self, FromJson, Reply, Request, ToJson},
};

/// The arena and the runner prepared in it.
struct Host {
    arena: Arena,
    runner: Runner<P4Interface, Dummy>,
}

thread_local! {
    static HOST: RefCell<Option<Host>> = const { RefCell::new(None) };
}

// == Initialization

/// Builds the runner for a `.watsup` file or a directory of them, replacing any earlier one.
///
/// Warnings are rendered to stderr; the error carries the cause.
pub fn init(path_spec: &str) -> Result<()> {
    let failed = |cause| Error::Init { path: path_spec.into(), cause };
    let (result, warnings) =
        crate::runner_spec_with_warnings(SpecLang::Sl, &[PathBuf::from(path_spec)]);
    for warning in &warnings {
        render(warning);
    }
    let Spec::Sl(spec_sl) = result.map_err(failed)? else {
        let cause = Diagnostic::new(
            "metacross",
            Severity::Error,
            None,
            "the specification did not lower to SL",
            vec![],
            vec![],
        );
        return Err(failed(Box::new(cause.into())));
    };
    let mut arena = Arena::new();
    let runner = runner::build_sl(&mut arena, spec_sl, Config::new(true, false, false), Dummy)
        .map_err(failed)?;
    HOST.with(|host| *host.borrow_mut() = Some(Host { arena, runner }));
    Ok(())
}

// == Requests

/// A mismatch is the specification's own failure, a fatal error the host's.
fn settle<T>(what: &str, id: &Id, eval: Eval<T>, ok: impl FnOnce(T) -> Reply) -> Result<Reply> {
    match eval.result {
        Ok(output) => Ok(ok(output)),
        Err(error @ InterpreterError::Mismatch(_)) => {
            eprintln!("ffi: extern {what} {} failed", id.node);
            render(&error.into_report());
            Ok(Reply::Fail)
        }
        Err(InterpreterError::Fatal(report)) => Err(Error::Report(report)),
    }
}

impl Host {
    /// Answers one request.
    fn answer(&mut self, request: &str) -> Result<Reply> {
        let arena = &mut self.arena;
        match Request::from_json(arena, &serde_json::from_str(request)?)? {
            Request::Builtin { id, targs, args } => {
                let value = self.runner.call_builtin(arena, &id, &targs, &args);
                Ok(Reply::Ok(value.map_err(InterpreterError::into_report)?))
            }
            Request::ExternFunc { id, targs, args } => {
                let eval = self.runner.eval_func(arena, &id.node, &targs, &args);
                settle("func", &id, eval, Reply::Ok)
            }
            Request::ExternRel { id, args } => {
                let eval = self.runner.eval_rel(arena, &id.node, &args);
                settle("rel", &id, eval, Reply::OkAll)
            }
        }
    }
}

/// The reply for a failed request, also noted on stderr.
pub fn error_reply(message: &str) -> String {
    eprintln!("ffi: {message}");
    json::error(message).to_string()
}

/// Answers one JSON request with one JSON reply; never panics on its own.
pub fn eval(request: &str) -> String {
    HOST.with(|host| {
        let mut host = host.borrow_mut();
        let Some(host) = host.as_mut() else {
            return error_reply("host_eval called before host_init");
        };
        match host.answer(request) {
            Ok(reply) => reply.to_json(&host.arena).to_string(),
            Err(error) => error_reply(&render_to_string(&error.into_report())),
        }
    })
}
