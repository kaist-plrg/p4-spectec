//! The C ABI between this process and the K library, ST-in-K's `unsafe` module
//!
//! `Library` holds the entry points of the library `kompile` made of spec-meta-k
//! (`kllvm-c.h`), and `spawn` runs its configuration on a K thread of its own.
//! The configuration calls back with `#ffiCall` at the addresses of these
//! host functions, which `spawn` passes as its configuration variables
//! (`spec-meta-k/0-aux/0.1-config.k`), so they resolve in any executable:
//!
//! ```c
//! char*       host_buffer(int64_t len);    // the send buffer, resized to len bytes
//! const char* host_eval(const char* req);  // a request; blocks for its response
//! const char* host_serve(const char* ans); // an answer; blocks for the next call
//! int64_t     host_strlen(const char* s);  // the length of a received message
//! ```
//!
//! The messages are those of spec-meta-k's JSON codec; a received one lives
//! until the next exchange. The host functions run on the K thread:
//! they hand each message to the thread waiting in `KEngine` and block for the reply.
//! Panics are contained, since unwinding out of `extern "C"` aborts.

#![allow(unsafe_code)]

use super::receive;
use std::{
    cell::RefCell,
    ffi::{CStr, CString, c_char, c_void},
    panic::{AssertUnwindSafe, catch_unwind},
    path::Path,
    ptr,
    sync::mpsc::{self, Receiver, Sender},
    thread::{self, JoinHandle},
};

/// K threads recurse deeply; only touched pages are committed.
const STACK: usize = 1 << 30;

/// The response of `host_eval` once `KEngine` is gone.
const RESPONSE_GONE: &CStr = c"{\"error\":\"the engine is gone\"}";

/// The call of `host_serve` once `KEngine` is gone, which ends the configuration.
const CALL_EXIT: &CStr = c"{\"exit\":null}";

// == The library

/// The C API of the library, from `kllvm-c.h`.
pub(super) struct Library {
    init: unsafe extern "C" fn(),
    parse: unsafe extern "C" fn(*const c_char) -> *mut c_void,
    construct: unsafe extern "C" fn(*const c_void) -> *mut c_void,
    take_steps: unsafe extern "C" fn(i64, *mut c_void) -> *mut c_void,
    free: unsafe extern "C" fn(*const c_void),
    dump: unsafe extern "C" fn(*mut c_void) -> *mut c_char,
    munmap: unsafe extern "C" fn(),
}

impl Library {
    /// Loads a library for the rest of the process.
    ///
    /// It is never closed: K's runtime installs process-wide hooks into it,
    /// such as GMP's allocator.
    pub(super) fn load(path: &Path) -> Result<&'static Self, String> {
        // libloading keeps the system's message, which names the file, as the source
        let failed = |error: libloading::Error| match std::error::Error::source(&error) {
            Some(source) => source.to_string(),
            None => format!("{}: {error}", path.display()),
        };
        // SAFETY: its initializers only set up K's runtime
        let library = unsafe { libloading::Library::new(path) }.map_err(failed)?;
        let library: &'static libloading::Library = Box::leak(Box::new(library));
        // SAFETY: each field has its function's type in `kllvm-c.h`
        unsafe {
            Ok(Box::leak(Box::new(Self {
                init: symbol(library, "kllvm_init").map_err(failed)?,
                parse: symbol(library, "kore_pattern_parse").map_err(failed)?,
                construct: symbol(library, "kore_pattern_construct").map_err(failed)?,
                take_steps: symbol(library, "take_steps").map_err(failed)?,
                free: symbol(library, "kore_pattern_free").map_err(failed)?,
                dump: symbol(library, "kore_block_dump").map_err(failed)?,
                munmap: symbol(library, "kllvm_reset_munmap_all_arenas").map_err(failed)?,
            })))
        }
    }

    /// Runs a configuration on this thread until no rule applies,
    /// then unmaps the thread's arenas, which would otherwise outlive it.
    ///
    /// Returns the final configuration, as KORE, if `stopped` holds of the run.
    ///
    /// # Safety
    /// This thread's `ENDPOINT` must be set: the configuration calls the host functions.
    unsafe fn run(&self, config: &CStr, stopped: impl FnOnce() -> bool) -> Option<String> {
        // SAFETY: the functions of `kllvm-c.h`, called in the order it documents
        unsafe {
            (self.init)();
            let pattern = (self.parse)(config.as_ptr());
            if pattern.is_null() {
                return Some("the initial configuration does not parse".into());
            }
            let block = (self.construct)(pattern);
            (self.free)(pattern);
            let block = (self.take_steps)(-1, block);
            // The dump is leaked: it is made once, for a configuration that is stuck
            let dump = stopped().then(|| {
                let text = (self.dump)(block);
                if text.is_null() {
                    String::new()
                } else {
                    CStr::from_ptr(text).to_string_lossy().into_owned()
                }
            });
            (self.munmap)();
            dump
        }
    }
}

/// A symbol of a library that stays loaded.
///
/// # Safety
/// `T` must be the symbol's type.
unsafe fn symbol<T: Copy>(
    library: &'static libloading::Library,
    name: &str,
) -> Result<T, libloading::Error> {
    // SAFETY: the caller gives the symbol's type
    unsafe { library.get::<T>(name) }.map(|symbol| *symbol)
}

// == K threads

/// A message of a K thread to the `KEngine` waiting for it.
pub(super) enum Message {
    /// A request of `host_eval`, followed by its response.
    Request(String),
    /// An answer of `host_serve`, followed by the next call.
    Answer(String),
    /// The configuration stopped without being told to exit; its final KORE.
    Stopped(String),
}

/// The engine's side of a K thread.
pub(super) struct Worker {
    pub(super) to_k: Sender<String>,
    pub(super) from_k: Receiver<Message>,
    pub(super) thread: JoinHandle<()>,
}

/// A K thread's side: its channels, the buffer K sends from, and the last message it received.
struct Endpoint {
    to_engine: Sender<Message>,
    from_engine: Receiver<String>,
    send: Vec<u8>,
    /// NUL-terminated.
    recv: Vec<u8>,
    /// Whether the last call was the one that ends the configuration.
    exiting: bool,
}

thread_local! {
    static ENDPOINT: RefCell<Option<Endpoint>> = const { RefCell::new(None) };
}

/// Starts a K thread that runs spec-meta-k's configuration, serving the calls it receives.
///
/// A configuration that stops without being told to exit is reported as `Message::Stopped`;
/// a thread that ends drops its sender, which the engine sees.
pub(super) fn spawn(library: &'static Library) -> std::io::Result<Worker> {
    let (to_engine, from_k) = mpsc::channel();
    let (to_k, from_engine) = mpsc::channel();
    let thread = thread::Builder::new()
        .name("spec-meta-k".into())
        .stack_size(STACK)
        .spawn(move || {
            let endpoint = Endpoint {
                to_engine,
                from_engine,
                send: Vec::new(),
                recv: Vec::new(),
                exiting: false,
            };
            ENDPOINT.with(|cell| *cell.borrow_mut() = Some(endpoint));
            let stopped =
                || ENDPOINT.with(|cell| cell.borrow().as_ref().is_some_and(|e| !e.exiting));
            // SAFETY: this thread's endpoint is set until the configuration ends
            let dump = unsafe { library.run(&configuration(), stopped) };
            if let Some(endpoint) = ENDPOINT.with(|cell| cell.borrow_mut().take())
                && let Some(dump) = dump
            {
                let _ = endpoint.to_engine.send(Message::Stopped(dump));
            }
        })?;
    Ok(Worker { to_k, from_k, thread })
}

/// The initial configuration: the configuration variables mapped to the
/// addresses of the host functions, as `krun` builds it.
fn configuration() -> CString {
    let bind = |map: String, name: &str, address: usize| {
        format!(
            "Lbl'Unds'Map'Unds'{{}}({map},Lbl'UndsPipe'-'-GT-Unds'{{}}(\
             inj{{SortKConfigVar{{}}, SortKItem{{}}}}(\\dv{{SortKConfigVar{{}}}}(\"${name}\")),\
             inj{{SortInt{{}}, SortKItem{{}}}}(\\dv{{SortInt{{}}}}(\"{address}\"))))"
        )
    };
    let map = bind("Lbl'Stop'Map{}()".into(), "HBUFFER", host_buffer as *const () as usize);
    let map = bind(map, "HEVAL", host_eval as *const () as usize);
    let map = bind(map, "HSERVE", host_serve as *const () as usize);
    let map = bind(map, "HSTRLEN", host_strlen as *const () as usize);
    CString::new(format!("LblinitGeneratedTopCell{{}}({map})")).expect("KORE has no NUL")
}

// == Host functions

/// Runs `body` with this thread's endpoint, or gives `fallback` without one or on a panic.
fn with_endpoint<T>(fallback: impl Fn() -> T, body: impl FnOnce(&mut Endpoint) -> T) -> T {
    let result = catch_unwind(AssertUnwindSafe(|| {
        ENDPOINT.with(|cell| cell.borrow_mut().as_mut().map(body))
    }));
    match result {
        Ok(Some(value)) => value,
        Ok(None) => fallback(),
        Err(_) => {
            eprintln!("spec-meta-k: a host function panicked");
            fallback()
        }
    }
}

/// Sends `message` to the engine and returns its reply, or `fallback` without one.
fn exchange(message: Message, fallback: &'static CStr) -> *const c_char {
    with_endpoint(
        || fallback.as_ptr(),
        |endpoint| {
            let reply = endpoint
                .to_engine
                .send(message)
                .ok()
                .and_then(|()| receive(&endpoint.from_engine).ok());
            let Some(reply) = reply else {
                endpoint.exiting = true;
                return fallback.as_ptr();
            };
            endpoint.exiting = reply.as_bytes() == CALL_EXIT.to_bytes();
            endpoint.recv = reply.into_bytes();
            endpoint.recv.push(0);
            endpoint.recv.as_ptr().cast()
        },
    )
}

/// The C string at `text`, or nothing for a null pointer.
///
/// # Safety
/// A non-null `text` points to a NUL-terminated string.
unsafe fn message(text: *const c_char) -> String {
    if text.is_null() {
        return String::new();
    }
    // SAFETY: the caller passes a NUL-terminated string
    unsafe { CStr::from_ptr(text) }
        .to_string_lossy()
        .into_owned()
}

/// Returns the send buffer, resized to `len` bytes, for K to write a message into.
#[unsafe(no_mangle)]
pub extern "C" fn host_buffer(len: i64) -> *mut c_char {
    with_endpoint(ptr::null_mut, |endpoint| {
        endpoint.send.clear();
        endpoint.send.resize(usize::try_from(len).unwrap_or(0), 0);
        endpoint.send.as_mut_ptr().cast()
    })
}

/// Answers a JSON request of spec-meta-k's `#ffiCall` with a JSON response.
///
/// # Safety
///
/// `request` points to a NUL-terminated string, as `host_buffer` holds it.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn host_eval(request: *const c_char) -> *const c_char {
    // SAFETY: the caller passes a NUL-terminated string
    let request = unsafe { message(request) };
    exchange(Message::Request(request), RESPONSE_GONE)
}

/// Passes an answer of spec-meta-k's `serve()` loop and returns the next call.
///
/// # Safety
///
/// `answer` points to a NUL-terminated string, as `host_buffer` holds it.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn host_serve(answer: *const c_char) -> *const c_char {
    // SAFETY: the caller passes a NUL-terminated string
    let answer = unsafe { message(answer) };
    exchange(Message::Answer(answer), CALL_EXIT)
}

/// Returns the length of a NUL-terminated response, as K reads it back.
///
/// # Safety
///
/// `text` points to a NUL-terminated string that `host_eval` or `host_serve` returned.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn host_strlen(text: *const c_char) -> i64 {
    // SAFETY: the caller passes a string host_eval returned, which ends in NUL
    let length = unsafe { CStr::from_ptr(text) }.to_bytes().len();
    i64::try_from(length).unwrap_or(i64::MAX)
}
