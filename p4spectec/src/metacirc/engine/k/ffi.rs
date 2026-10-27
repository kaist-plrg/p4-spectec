//! The C ABI between this process and the K library, the only `unsafe` module
//!
//! `KRuntime` calls into the library through the functions declared here.
//! The library calls back with `#ffiCall`, finding its host functions
//! by name in this executable, which exports its symbols for it
//! (`-rdynamic` for the `metacirc` binary, in `build.rs`):
//!
//! ```c
//! const char* host_eval(const char* req);  // JSON request, JSON response
//! int64_t     host_strlen(const char* s);  // sizes K's read-back buffer
//! ```
//!
//! The request and response are those of spec-meta-k's JSON codec;
//! a response lives until the next call. `host_eval` runs on a K thread,
//! so it hands the request to the thread waiting in `KEngine` and blocks.

#![allow(unsafe_code)]

use std::ffi::{CStr, c_char};

/// The response of `host_eval` until the ST-in-K implementation answers requests.
const RESPONSE_UNIMPLEMENTED: &CStr = c"{\"error\": \"host_eval is not implemented yet\"}";

/// Answers a JSON request of spec-meta-k's `#ffiCall` with a JSON response.
#[unsafe(no_mangle)]
pub extern "C" fn host_eval(_request: *const c_char) -> *const c_char {
    RESPONSE_UNIMPLEMENTED.as_ptr()
}

/// Returns the length of a NUL-terminated response, as K reads it back.
///
/// # Safety
///
/// `text` points to a NUL-terminated string that `host_eval` returned.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn host_strlen(text: *const c_char) -> i64 {
    // SAFETY: the caller passes a string host_eval returned, which ends in NUL
    let length = unsafe { CStr::from_ptr(text) }.to_bytes().len();
    i64::try_from(length).unwrap_or(i64::MAX)
}
