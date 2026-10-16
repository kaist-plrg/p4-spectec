//! The host that K and Redex call back into: one runner for the process
//!
//! `init` builds the SL runner of a specification, and `eval` answers the
//! JSON requests of `spec-meta-k/al/4.1-extern-json.k` and
//! `spec-meta-redex/common/0.1-extern-json.rkt`. The runner holds `Rc`
//! data, so the host is thread-local; both consumers call from one thread.
//! The runner is never reset between requests: `fresh_typeId`'s counter
//! runs for the whole process, and the arena grows with it.

use std::{cell::RefCell, path::PathBuf};

use crate::{
    diagnostic::{RenderConfig, Renderer, Report},
    lang::data::arena::Arena,
    pass::SpecLang,
    runner::{self, Config, Runner, Spec},
    sim_plugin::dummy::Dummy,
};

use crate::interface::p4::P4Interface;

/// The arena and the runner prepared in it.
struct Host {
    #[allow(dead_code)]
    arena: Arena,
    #[allow(dead_code)]
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

/// The reply for a failed request, also noted on stderr.
pub fn error_reply(message: &str) -> String {
    eprintln!("ffi: {message}");
    serde_json::json!({ "error": message }).to_string()
}

/// Answers one JSON request with one JSON reply; never panics on its own.
pub fn eval(request: &str) -> String {
    let initialized = HOST.with(|host| host.borrow().is_some());
    if !initialized {
        return error_reply("host_eval called before host_init");
    }
    error_reply(&format!("not implemented: {}", request.chars().take(80).collect::<String>()))
}
