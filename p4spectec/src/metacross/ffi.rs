//! The `extern "C"` surface of `bin/libp4spectec.so`, called by K and Racket
//!
//! This is the only module permitted to use `unsafe`: it reads the callers'
//! C strings and hands back a pointer into a string Rust owns.
//!
//! ```c
//! int64_t     host_init(const char* spec);   // 1 ok, 0 failed (message on stderr)
//! const char* host_eval(const char* req);    // JSON, valid until the next host_* call
//! int64_t     host_strlen(const char* s);    // sizes K's read-back buffer
//! ```
//!
//! The reply lives in a thread-local replaced by the next call, so nothing is
//! freed by the caller. Panics are contained and become `{"error": ...}`
//! replies, since unwinding out of `extern "C"` aborts the process.
//! All calls must come from the one thread that called `host_init`: the host
//! is thread-local, and K's interpreter and Racket both call from their main
//! thread.

#![allow(unsafe_code)]

use std::{
    cell::RefCell,
    ffi::{CStr, CString, c_char},
    panic::{AssertUnwindSafe, catch_unwind},
};

use super::{error::render, host, json};

thread_local! {
    static REPLY: RefCell<Option<CString>> = const { RefCell::new(None) };
}

/// Stores the reply and returns a pointer valid until the next reply.
fn reply(text: String) -> *const c_char {
    let text = CString::new(text)
        .unwrap_or_else(|_| CString::from(c"{\"error\": \"reply contains a NUL byte\"}"));
    REPLY.with(|reply| reply.borrow_mut().insert(text).as_ptr())
}

/// Runs `body`, turning a panic into `fallback` with a note on stderr.
fn contained<T>(name: &str, fallback: impl FnOnce() -> T, body: impl FnOnce() -> T) -> T {
    catch_unwind(AssertUnwindSafe(body)).unwrap_or_else(|_| {
        eprintln!("ffi: {name} panicked");
        fallback()
    })
}

/// The caller's C string as UTF-8, or `None` for a null or invalid pointer.
///
/// # Safety
/// A non-null `ptr` must point to a NUL-terminated string that outlives the call.
unsafe fn argument<'a>(ptr: *const c_char) -> Option<&'a str> {
    if ptr.is_null() {
        return None;
    }
    unsafe { CStr::from_ptr(ptr) }.to_str().ok()
}

/// Builds the host runner for a specification path; 1 on success, 0 on failure.
///
/// # Safety
/// `spec` must be null or a NUL-terminated string valid for the call.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn host_init(spec: *const c_char) -> i64 {
    let spec = unsafe { argument(spec) };
    contained(
        "host_init",
        || 0,
        || match spec {
            Some(spec) => match host::init(spec) {
                Ok(()) => 1,
                Err(error) => {
                    render(&error.into_report());
                    0
                }
            },
            None => {
                eprintln!("ffi: host_init: null or non-UTF-8 specification path");
                0
            }
        },
    )
}

/// Answers a JSON request with a JSON reply owned by this library.
///
/// # Safety
/// `request` must be null or a NUL-terminated string valid for the call.
/// The returned pointer is valid until the next `host_*` call and must not be freed.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn host_eval(request: *const c_char) -> *const c_char {
    let request = unsafe { argument(request) };
    let text = contained(
        "host_eval",
        || json::error("host_eval panicked").to_string(),
        || match request {
            Some(request) => host::eval(request),
            None => host::error_reply("host_eval: null or non-UTF-8 request"),
        },
    );
    reply(text)
}

/// The length of a C string, so K can size its read-back buffer.
///
/// # Safety
/// `s` must be null or a NUL-terminated string valid for the call.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn host_strlen(s: *const c_char) -> i64 {
    if s.is_null() {
        return 0;
    }
    i64::try_from(unsafe { CStr::from_ptr(s) }.to_bytes().len()).unwrap_or(i64::MAX)
}
