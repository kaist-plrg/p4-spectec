//! Host-call arguments and results at one AL representation boundary
//!
//! Calls decode identifiers, types, and values through the shared AL codec.
//! Successful native calls become OK; exhausted alternatives become UNMATCH.
//! Fatal errors remain diagnostics, and every path retains the call's effects.
//!
//! The reverse direction enters the meta specification from the host:
//! `encode::rel_args` builds the name and inputs of `Call_rel`,
//! and `decode::rel_result` maps OK, UNMATCH, and ERROR back to native outcomes.

use super::super::error;
use crate::{
    interface::meta::shared::{
        self as meta, Cache,
        decode::Decode,
        encode::{Encode, list},
    },
    lang::{
        common::source::Span,
        data::{
            arena::Arena,
            value::flat::{Value, get, make},
        },
        il::ast::{Id, Typ},
    },
    runner::{Eval, ExternError, InterpreterError},
};

/// Constructors of the meta specification's call results.
mod m {
    pub(super) const OK: &str = "OK val";
    pub(super) const UNMATCH: &str = "UNMATCH";
    pub(super) const ERROR: &str = "ERROR";
}

/// Decoding of host-call arguments and meta call results.
pub(in crate::metacirc) mod decode {
    use super::*;

    /// Decodes one checked AL representation without flattening its diagnostic.
    fn decode<T: Decode>(
        arena: &mut Arena,
        cache: &mut Cache,
        value: Value,
    ) -> Result<T, ExternError> {
        T::decode_cached(arena, cache, value)
            .map_err(|error| ExternError::from(error.into_report()))
    }

    /// Decodes the three arguments shared by builtin and extern function calls.
    pub(in crate::metacirc) fn func_args(
        arena: &mut Arena,
        cache: &mut Cache,
        values: &[Value],
    ) -> Result<(Id, Vec<Typ>, Vec<Value>), ExternError> {
        let [value_id, value_targs, value_args] = values else {
            return Err(ExternError::diagnostic_message(
                "meta function call requires three arguments",
            ));
        };
        Ok((
            decode(arena, cache, *value_id)?,
            decode(arena, cache, *value_targs)?,
            decode(arena, cache, *value_args)?,
        ))
    }

    /// Decodes an extern relation name and its input values.
    pub(in crate::metacirc) fn rel_args(
        arena: &mut Arena,
        cache: &mut Cache,
        values: &[Value],
    ) -> Result<(Id, Vec<Value>), ExternError> {
        let [value_id, value_args] = values else {
            return Err(ExternError::diagnostic_message(
                "meta relation call requires two arguments",
            ));
        };
        Ok((decode(arena, cache, *value_id)?, decode(arena, cache, *value_args)?))
    }

    /// Selects the OK payload; UNMATCH allows another candidate, ERROR aborts.
    fn result(arena: &mut Arena, value: Value, name: &str) -> Result<Value, InterpreterError> {
        get::matches_string! { arena, &value,
            m::OK => |[value_payload]| Ok(*value_payload),
            m::UNMATCH => |[]| Err(InterpreterError::Mismatch(vec![*error::execution_unmatched(name)])),
            m::ERROR => |[]| Err(InterpreterError::Fatal(error::execution_failed(name))),
            _ => Err(InterpreterError::Fatal(
                meta::Error::unexpected(arena, value, "a meta call result").into_report(),
            )),
        }
    }

    /// Decodes a funcRes into the native function result.
    pub(in crate::metacirc) fn func_result(
        arena: &mut Arena,
        cache: &mut Cache,
        value: Value,
        name: &str,
    ) -> Result<Value, InterpreterError> {
        let value_payload = result(arena, value, name)?;
        Ok(decode(arena, cache, value_payload)?)
    }

    /// Decodes a relRes into the native relation outputs.
    pub(in crate::metacirc) fn rel_result(
        arena: &mut Arena,
        cache: &mut Cache,
        value: Value,
        name: &str,
    ) -> Result<Vec<Value>, InterpreterError> {
        let value_payload = result(arena, value, name)?;
        Ok(decode(arena, cache, value_payload)?)
    }
}

/// Encoding of call arguments and native outcomes.
pub(in crate::metacirc) mod encode {
    use super::*;

    /// Encodes a function name, type arguments, and values for `Call_func`.
    pub(in crate::metacirc) fn func_args(
        arena: &mut Arena,
        cache: &mut Cache,
        id: &Id,
        targs: &[Typ],
        values: &[Value],
    ) -> Result<[Value; 3], InterpreterError> {
        let encode = |arena: &mut Arena, cache: &mut Cache| -> Result<_, meta::Error> {
            Ok([
                id.encode_cached(arena, cache)?,
                list(arena, cache, "targ", targs)?,
                list(arena, cache, "val", values)?,
            ])
        };
        encode(arena, cache).map_err(|error| InterpreterError::Fatal(error.into_report()))
    }

    /// Encodes a relation name and its input values for `Call_rel`.
    pub(in crate::metacirc) fn rel_args(
        arena: &mut Arena,
        cache: &mut Cache,
        id: &Id,
        values: &[Value],
    ) -> Result<[Value; 2], InterpreterError> {
        let encode = |arena: &mut Arena, cache: &mut Cache| -> Result<_, meta::Error> {
            Ok([id.encode_cached(arena, cache)?, list(arena, cache, "val", values)?])
        };
        encode(arena, cache).map_err(|error| InterpreterError::Fatal(error.into_report()))
    }

    /// Wraps a native outcome while preserving its effect even if encoding fails.
    fn result<T>(
        arena: &mut Arena,
        cache: &mut Cache,
        eval: Eval<T>,
        text_typ: &str,
        encode_output: impl FnOnce(&mut Arena, &mut Cache, T) -> Result<Value, meta::Error>,
    ) -> Eval<Vec<Value>, ExternError> {
        let result = match eval.result {
            // A successful call is OK of its outputs, encoded in the arena of the arguments
            Ok(output) => encode_output(arena, cache, output)
                .map_err(|error| ExternError::from(error.into_report()))
                .and_then(|value_output| {
                    make::case_string! {
                        arena: arena,
                        shape: "OK val",
                        args: [value_output],
                        typ: text_typ,
                        span: Span::default(),
                    }
                    .map_err(|error| ExternError::from(meta::Error::from(error).into_report()))
                }),
            // An exhausted relation or function is a meta-level UNMATCH, not a host abort
            Err(InterpreterError::Mismatch(_)) => make::case_string! {
                arena: arena,
                shape: "UNMATCH",
                args: [],
                typ: text_typ,
                span: Span::default(),
            }
            .map_err(|error| ExternError::from(meta::Error::from(error).into_report())),
            // Keep fatal failures outside the specification's backtracking protocol
            Err(InterpreterError::Fatal(report)) => Err(ExternError::from(report)),
        };
        Eval::new(result.map(|value| vec![value]), eval.effect)
    }

    /// Encodes a native function outcome as funcRes.
    pub(in crate::metacirc) fn func_result(
        arena: &mut Arena,
        cache: &mut Cache,
        eval: Eval<Value>,
    ) -> Eval<Vec<Value>, ExternError> {
        result(arena, cache, eval, "funcRes", |arena, cache, value| {
            value.encode_cached(arena, cache)
        })
    }

    /// Encodes a native relation outcome as relRes.
    pub(in crate::metacirc) fn rel_result(
        arena: &mut Arena,
        cache: &mut Cache,
        eval: Eval<Vec<Value>>,
    ) -> Eval<Vec<Value>, ExternError> {
        result(arena, cache, eval, "relRes", |arena, cache, values| {
            list(arena, cache, "val", &values)
        })
    }
}
