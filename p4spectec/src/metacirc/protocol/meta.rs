//! spec-meta's protocol: the level above is AL, and calls go through Call_rel
//!
//! `load` encodes the level above as AL (IL through `Algo_IL` below first)
//! and makes C = `$load_ctx($empty_ctx, spec)` on the level below.
//! `lower` turns `name(values)` into `Call_rel(C, name, values)`
//! or `Call_func(C, name, targs, values)`; `raise` maps OK, UNMATCH, or ERROR
//! to outputs, a mismatch, or a fatal failure.
//! `host` decodes `Call_builtin_func`, `Call_extern_func`, and `Call_extern_rel`;
//! `reply` encodes a native outcome as OK or UNMATCH, keeping its effects.

use super::{Below, Call, HostCall, Output, Protocol, Values};
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
            value::flat::{self as value, Value, get, make},
        },
        il::ast::{Id, Typ},
    },
    metacirc::{error, tower::LevelSpec},
    phrase,
    runner::{Eval, ExternError, InterpreterError},
};
use smallvec::smallvec;
use std::borrow::Cow;

// == Protocol

/// The protocol of spec-meta toward the AL or IL specification above it.
pub(in crate::metacirc) struct MetaProtocol;

impl Protocol for MetaProtocol {
    fn load(&self, below: &mut dyn Below, above: &LevelSpec) -> Result<Value, InterpreterError> {
        // Encode the specification as AL, converting IL through spec-meta below
        let value_spec_al = match above {
            LevelSpec::AboveAl { spec_al, .. } => spec_al
                .encode(below.arena())
                .map_err(|error| InterpreterError::Fatal(error.into_report()))?,
            LevelSpec::AboveIl { spec_il, .. } => {
                let value_spec_il = spec_il
                    .encode(below.arena())
                    .map_err(|error| InterpreterError::Fatal(error.into_report()))?;
                let call =
                    Call::Rel { name: "Algo_IL", values: Values::Borrowed(&[value_spec_il]) };
                let values = below.call(call)?.into_rel();
                *value::get::one(&values).map_err(ExternError::from)?
            }
            LevelSpec::Base { .. } => unreachable!("the base is never loaded by a level below"),
        };
        // C = $load_ctx($empty_ctx, spec), on the level below
        let call = Call::Func {
            name: "empty_ctx",
            targs: Cow::Borrowed(&[]),
            values: Values::Borrowed(&[]),
        };
        let value_ctx_empty = below.call(call)?.into_func();
        let call = Call::Func {
            name: "load_ctx",
            targs: Cow::Borrowed(&[]),
            values: Values::Borrowed(&[value_ctx_empty, value_spec_al]),
        };
        Ok(below.call(call)?.into_func())
    }

    fn lower(
        &self,
        arena: &mut Arena,
        cache: &mut Cache,
        value_ctx: Value,
        call: &Call<'_>,
    ) -> Result<Call<'static>, InterpreterError> {
        let id = phrase!(node: call.name().into(), span: Span::default());
        let values = match call {
            // Call_rel(C, name, values) : relRes
            Call::Rel { values, .. } => {
                let [value_id, value_args] = encode::rel_args(arena, cache, &id, values)?;
                Call::Rel {
                    name: "Call_rel",
                    values: Values::Lowered(smallvec![value_ctx, value_id, value_args]),
                }
            }
            // Call_func(C, name, targs, values) : funcRes
            Call::Func { targs, values, .. } => {
                let [value_id, value_targs, value_args] =
                    encode::func_args(arena, cache, &id, targs, values)?;
                let values = smallvec![value_ctx, value_id, value_targs, value_args];
                Call::Rel { name: "Call_func", values: Values::Lowered(values) }
            }
        };
        Ok(values)
    }

    fn raise(
        &self,
        arena: &mut Arena,
        cache: &mut Cache,
        output: Output,
        call: &Call<'_>,
    ) -> Result<Output, InterpreterError> {
        let values_res = output.into_rel();
        let value_res = *value::get::one(&values_res).map_err(ExternError::from)?;
        match call {
            // OK outputs, UNMATCH a mismatch, ERROR a fatal failure
            Call::Rel { name, .. } => {
                decode::rel_result(arena, cache, value_res, name).map(Output::Rel)
            }
            // OK the result, UNMATCH a mismatch, ERROR a fatal failure
            Call::Func { name, .. } => {
                decode::func_result(arena, cache, value_res, name).map(Output::Func)
            }
        }
    }

    fn host(
        &self,
        arena: &mut Arena,
        cache: &mut Cache,
        name: &str,
        values: &[Value],
    ) -> Result<HostCall, ExternError> {
        match name {
            // Builtins are answered by the interface of the level above
            "Call_builtin_func" => {
                let (id, targs, values) = decode::func_args(arena, cache, values)?;
                Ok(HostCall::Builtin { id, targs, values })
            }
            // Host functions are answered by the extern of the level above
            "Call_extern_func" => {
                let (id, targs, values) = decode::func_args(arena, cache, values)?;
                Ok(HostCall::ExternFunc { id, targs, values })
            }
            // Host relations return an ordered list of outputs
            "Call_extern_rel" => {
                let (id, values) = decode::rel_args(arena, cache, values)?;
                Ok(HostCall::ExternRel { id, values })
            }
            // Host calls are reached through the three declared meta relations
            _ => Err(error::extern_unsupported(name).into()),
        }
    }

    fn reply(
        &self,
        arena: &mut Arena,
        cache: &mut Cache,
        call: &HostCall,
        eval: Eval<Output>,
    ) -> Eval<Vec<Value>, ExternError> {
        let Eval { result, effect } = eval;
        match call {
            // A function outcome is funcRes
            HostCall::Builtin { .. } | HostCall::ExternFunc { .. } => {
                let eval = Eval::new(result.map(Output::into_func), effect);
                encode::func_result(arena, cache, eval)
            }
            // A relation outcome is relRes
            HostCall::ExternRel { .. } => {
                let eval = Eval::new(result.map(Output::into_rel), effect);
                encode::rel_result(arena, cache, eval)
            }
        }
    }
}

// == Codec

/// Constructors of the meta specification's call results.
mod m {
    pub(super) const OK: &str = "OK val";
    pub(super) const UNMATCH: &str = "UNMATCH";
    pub(super) const ERROR: &str = "ERROR";
}

/// Decoding of host-call arguments and meta call results.
mod decode {
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
    pub(super) fn func_args(
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
    pub(super) fn rel_args(
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
    pub(super) fn func_result(
        arena: &mut Arena,
        cache: &mut Cache,
        value: Value,
        name: &str,
    ) -> Result<Value, InterpreterError> {
        let value_payload = result(arena, value, name)?;
        Ok(decode(arena, cache, value_payload)?)
    }

    /// Decodes a relRes into the native relation outputs.
    pub(super) fn rel_result(
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
mod encode {
    use super::*;

    /// Encodes a function name, type arguments, and values for `Call_func`.
    pub(super) fn func_args(
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
    pub(super) fn rel_args(
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
    pub(super) fn func_result(
        arena: &mut Arena,
        cache: &mut Cache,
        eval: Eval<Value>,
    ) -> Eval<Vec<Value>, ExternError> {
        result(arena, cache, eval, "funcRes", |arena, cache, value| {
            value.encode_cached(arena, cache)
        })
    }

    /// Encodes a native relation outcome as relRes.
    pub(super) fn rel_result(
        arena: &mut Arena,
        cache: &mut Cache,
        eval: Eval<Vec<Value>>,
    ) -> Eval<Vec<Value>, ExternError> {
        result(arena, cache, eval, "relRes", |arena, cache, values| {
            list(arena, cache, "val", &values)
        })
    }
}
