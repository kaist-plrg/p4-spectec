//! Core P4 externs shared by every architecture
//!
//! `func` implements `static_assert` and `verify`;
//! `object` implements `packet_in` and `packet_out`.

pub mod func;
pub mod object;
