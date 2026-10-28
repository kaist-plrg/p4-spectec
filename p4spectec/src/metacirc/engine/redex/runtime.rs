//! The `racket` process running a PLT Redex base specification
//!
//! A `redex` tower names the base's entry module in its `module` field.
//! `RedexRuntime::load` starts `racket` on it once per tower, with piped
//! stdin and stdout, and every depth's `RedexEngineState` shares the process:
//! a nested call from a deeper depth goes to the same pipe while the outer call
//! waits for a host response, and Racket answers it in place.
//! The runtime keeps each load request under a key, which C names,
//! and the key the process loaded last.
//!
//! Each line from the process starts with a word saying what it is
//! (`Line`); each line to it is one JSON call or response.

use super::failed;
use crate::{
    metacirc::{Result, error, protocol::MetaRedexProtocol},
    runner::InterpreterError,
};
use std::{
    cell::{Cell, RefCell},
    io::{BufRead, BufReader, BufWriter, Write},
    path::{Path, PathBuf},
    process::{Child, ChildStdin, ChildStdout, Command, Stdio},
    rc::Rc,
};

/// A `racket` process running the base's entry module, shared by every depth.
pub(in crate::metacirc) struct RedexRuntime {
    /// The entry module, as the configuration resolved it.
    path: PathBuf,
    /// The process, until it is found stopped.
    process: RefCell<Option<Process>>,
    /// The load requests made so far, at their keys; equal requests share one.
    loads: RefCell<Vec<Rc<str>>>,
    /// The key of the load request the process ran last.
    loaded: Cell<Option<usize>>,
}

struct Process {
    child: Child,
    to_racket: BufWriter<ChildStdin>,
    from_racket: BufReader<ChildStdout>,
}

/// A line from the process.
pub(super) enum Line {
    /// `serve <answer>`: the answer to the call sent last.
    Serve(String),
    /// `eval <request>`: a host request, waiting for its response.
    Eval(String),
    /// `debug <val>`: the value of a `debug` premise.
    Debug(String),
    /// `raise <text>`: the Racket error that ended the call sent last.
    Raise(String),
}

impl RedexRuntime {
    /// Starts `racket` on the entry module at `path`, compiled by `make redex`,
    /// and waits until it serves.
    pub(in crate::metacirc) fn load(path: &Path) -> Result<Self> {
        let compiled = path.file_stem().map(|stem| {
            let mut name = stem.to_owned();
            name.push("_rkt.zo");
            path.with_file_name("compiled").join(name)
        });
        if !path.is_file() || !compiled.is_some_and(|compiled| compiled.is_file()) {
            return Err(error::config_invalid(format!(
                "Racket module {} not found or not compiled; build it with make redex first",
                path.display()
            )));
        }
        let runtime = Self {
            path: path.to_owned(),
            process: RefCell::default(),
            loads: RefCell::default(),
            loaded: Cell::default(),
        };
        runtime.start().map_err(InterpreterError::into_report)?;
        Ok(runtime)
    }

    /// Starts the process if it is not running, and waits until it serves.
    pub(super) fn start(&self) -> std::result::Result<(), InterpreterError> {
        if self.process.borrow().is_some() {
            return Ok(());
        }
        let path = self.path.display();
        let mut child = Command::new("racket")
            .arg(&self.path)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .spawn()
            .map_err(|error| {
                InterpreterError::Fatal(failed(format!("cannot run racket {path}: {error}")))
            })?;
        let to_racket = BufWriter::new(child.stdin.take().expect("stdin is piped"));
        let from_racket = BufReader::new(child.stdout.take().expect("stdout is piped"));
        *self.process.borrow_mut() = Some(Process { child, to_racket, from_racket });
        self.loaded.set(None);
        match self.receive()? {
            Line::Serve(answer) if answer == r#"{"ready":null}"# => Ok(()),
            _ => {
                self.stop();
                let message = format!("racket {path} did not start serving");
                Err(InterpreterError::Fatal(failed(message)))
            }
        }
    }

    /// Sends one call or response.
    pub(super) fn send(&self, message: &str) -> std::result::Result<(), InterpreterError> {
        let mut process = self.process.borrow_mut();
        let Some(Process { to_racket, .. }) = process.as_mut() else {
            return Err(stopped());
        };
        let sent = writeln!(to_racket, "{message}").and_then(|()| to_racket.flush());
        drop(process);
        sent.map_err(|_| {
            self.stop();
            stopped()
        })
    }

    /// Receives the next line.
    pub(super) fn receive(&self) -> std::result::Result<Line, InterpreterError> {
        let mut process = self.process.borrow_mut();
        let Some(Process { from_racket, .. }) = process.as_mut() else {
            return Err(stopped());
        };
        let mut line = String::new();
        let read = from_racket.read_line(&mut line);
        drop(process);
        if !matches!(read, Ok(len) if len > 0) {
            self.stop();
            return Err(stopped());
        }
        let line = line.strip_suffix('\n').unwrap_or(&line);
        let line = match line.split_once(' ') {
            Some(("serve", answer)) => Line::Serve(answer.to_owned()),
            Some(("eval", request)) => Line::Eval(request.to_owned()),
            Some(("debug", value)) => Line::Debug(value.to_owned()),
            Some(("raise", message)) => {
                Line::Raise(serde_json::from_str(message).unwrap_or_else(|_| message.to_owned()))
            }
            _ => {
                self.stop();
                let message = format!("racket {} sent a malformed line", self.path.display());
                return Err(InterpreterError::Fatal(failed(message)));
            }
        };
        Ok(line)
    }

    /// Kills a process that cannot serve any more; the next `start` restarts it.
    fn stop(&self) {
        if let Some(mut process) = self.process.borrow_mut().take() {
            let _ = process.child.kill();
            let _ = process.child.wait();
        }
        self.loaded.set(None);
    }

    /// Returns the key of a load request, keeping the request if it is new.
    pub(super) fn register(&self, request: &str) -> usize {
        let mut loads = self.loads.borrow_mut();
        if let Some(key) = loads.iter().position(|load| **load == *request) {
            return key;
        }
        loads.push(request.into());
        loads.len() - 1
    }

    /// Returns the load request at `key`, if one was registered.
    pub(super) fn request(&self, key: usize) -> Option<Rc<str>> {
        self.loads.borrow().get(key).cloned()
    }

    /// Returns the key of the load request the process ran last.
    pub(super) fn loaded(&self) -> Option<usize> {
        self.loaded.get()
    }

    /// Records the key of the load request the process ran last.
    pub(super) fn set_loaded(&self, key: Option<usize>) {
        self.loaded.set(key);
    }
}

fn stopped() -> InterpreterError {
    InterpreterError::Fatal(failed("the racket process stopped"))
}

impl Drop for RedexRuntime {
    /// Tells the process to exit and waits for it.
    fn drop(&mut self) {
        if let Some(Process { mut child, mut to_racket, from_racket }) = self.process.take() {
            let exit = MetaRedexProtocol::exit_request();
            let _ = writeln!(to_racket, "{exit}").and_then(|()| to_racket.flush());
            drop(to_racket);
            drop(from_racket);
            let _ = child.wait();
        }
    }
}
