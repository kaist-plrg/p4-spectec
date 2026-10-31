//! Per-test timeouts through a worker process
//!
//! A driver given a timeout runs its tests in a worker:
//! the same executable started with a hidden `worker` command,
//! which prepares its specification once and then answers one request per line on stdin.
//! `Worker` starts it, sends requests, and waits at most the timeout for each answer;
//! a test without an answer by then is a timeout, and the worker is killed
//! and started afresh for the next request.
//! `Channel` is the worker's end: it keeps the original stdout for answers,
//! each a status line and detail lines ended by a NUL byte,
//! and silences stdout for the interpreter.

use std::{
    fs::File,
    io::{self, BufRead, BufReader, BufWriter, Write},
    os::fd::AsFd,
    path::Path,
    process::{Child, ChildStdin, Command, Stdio},
    sync::mpsc::{self, Receiver, RecvTimeoutError},
    thread,
    time::Duration,
};

use super::{Error, Result, output::SilenceStdout};

/// Status of an answer that reports a test execution error as its details.
const STATUS_ERROR: &str = "error";

/// Ends each answer, which never contains it.
const END_ANSWER: u8 = b'\0';

/// The answer to one request.
pub enum Reply {
    /// The test finished with a status and detail lines.
    Done { status: String, details: String },
    /// The test did not finish within the timeout.
    Timeout,
}

/// Spells a path for a request, which is one line of tab-separated fields.
pub fn path_text(path: &Path) -> Result<&str> {
    path.to_str()
        .filter(|text| !text.contains(['\t', '\r', '\n']))
        .ok_or_else(|| Error::from(format!("invalid request path: {}", path.display())))
}

/// Runs requests in a worker process, restarting it after a timeout.
pub struct Worker {
    args: Vec<String>,
    timeout: Duration,
    running: Option<Running>,
}

/// A started worker and the receiver of its answers.
struct Running {
    child: Child,
    stdin: ChildStdin,
    answers: Receiver<Vec<u8>>,
}

impl Worker {
    /// Prepares a worker of this executable with `args`, started on its first request.
    pub fn new(args: Vec<String>, timeout: Duration) -> Self {
        Self { args, timeout, running: None }
    }

    /// Sends one request line and waits at most the timeout for its answer.
    ///
    /// Preparing a newly started worker does not count toward the timeout.
    pub fn request(&mut self, request: &str) -> Result<Reply> {
        if self.running.is_none() {
            self.running = Some(self.start()?);
        }
        let running = self.running.as_mut().expect("worker started");
        writeln!(running.stdin, "{request}")?;
        running.stdin.flush()?;
        match running.answers.recv_timeout(self.timeout) {
            Ok(answer) => {
                let answer = String::from_utf8(answer)?;
                let (status, details) = answer.split_once('\n').unwrap_or((&answer, ""));
                if status == STATUS_ERROR {
                    return Err(Error::from(details.to_owned()));
                }
                Ok(Reply::Done { status: status.to_owned(), details: details.to_owned() })
            }
            Err(RecvTimeoutError::Timeout) => {
                self.stop();
                Ok(Reply::Timeout)
            }
            Err(RecvTimeoutError::Disconnected) => {
                self.stop();
                Err(Error::from(format!("worker exited while answering {request}")))
            }
        }
    }

    /// Starts the worker and waits, without a timeout, until it is prepared.
    fn start(&self) -> Result<Running> {
        let mut child = Command::new(std::env::current_exe()?)
            .args(&self.args)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .spawn()?;
        let stdin = child.stdin.take().expect("piped stdin");
        let stdout = child.stdout.take().expect("piped stdout");
        // Forward answers until the worker exits or is killed
        let (sender, answers) = mpsc::channel();
        thread::spawn(move || {
            let mut reader = BufReader::new(stdout);
            loop {
                let mut answer = Vec::new();
                match reader.read_until(END_ANSWER, &mut answer) {
                    Ok(_) if answer.pop() == Some(END_ANSWER) => {
                        if sender.send(answer).is_err() {
                            break;
                        }
                    }
                    _ => break,
                }
            }
        });
        // The first answer announces that preparation finished
        if answers.recv().is_err() {
            let status = child.wait()?;
            return Err(Error::from(format!(
                "worker {} failed to start: {status}",
                self.args.join(" ")
            )));
        }
        Ok(Running { child, stdin, answers })
    }

    /// Kills the running worker, if any.
    fn stop(&mut self) {
        if let Some(mut running) = self.running.take() {
            let _ = running.child.kill();
            let _ = running.child.wait();
        }
    }
}

impl Drop for Worker {
    fn drop(&mut self) {
        self.stop();
    }
}

/// The worker's end of the requests and answers.
pub struct Channel {
    answers: BufWriter<File>,
    guard_stdout: SilenceStdout,
}

impl Channel {
    /// Keeps stdout for answers and silences it for everything else.
    ///
    /// Open before preparing the specification, whose debug output goes to stdout.
    pub fn open() -> Result<Self> {
        let answers = File::from(io::stdout().as_fd().try_clone_to_owned()?);
        let guard_stdout = SilenceStdout::new()?;
        Ok(Self { answers: BufWriter::new(answers), guard_stdout })
    }

    /// Answers each request line on stdin with `answer` until stdin closes.
    ///
    /// `answer` gives a status and detail lines;
    /// its error is sent as an error answer, and the worker keeps serving.
    pub fn serve(mut self, mut answer: impl FnMut(&str) -> Result<(String, String)>) -> Result<()> {
        // Announce that preparation finished
        self.send("ready", "")?;
        for request in io::stdin().lock().lines() {
            let request = request?;
            match answer(&request) {
                Ok((status, details)) => self.send(&status, &details)?,
                Err(error) => self.send(STATUS_ERROR, &error.to_string())?,
            }
        }
        self.guard_stdout.finish()?;
        Ok(())
    }

    /// Writes one answer.
    fn send(&mut self, status: &str, details: &str) -> Result<()> {
        writeln!(self.answers, "{status}")?;
        self.answers.write_all(details.as_bytes())?;
        self.answers.write_all(&[END_ANSWER])?;
        self.answers.flush()?;
        Ok(())
    }
}
