//! The C ABI of the K library that `Offload` calls, an `unsafe` module
//!
//! The library is the one `kompile` made of the K definition being run;
//! its K hooks follow the K LLVM backend's runtime
//! (`runtime/strings/strings.cpp`, `runtime/strings/bytes.cpp`, ...).
//! Open it with `libloading`, look up each hook symbol `Offload::call` needs,
//! and keep every `unsafe` block of K-in-ST in this file.
//! Besides `engine/k/ffi.rs`, this is the only module where the crate allows `unsafe`.

#![allow(unsafe_code)]
