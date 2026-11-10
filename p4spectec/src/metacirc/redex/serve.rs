//! The calls and answers of spec-meta-redex's serve loop
//!
//! spec-meta-redex (`spec-meta-redex/3-execution/3.15-entry.rkt`) serves s-expression text,
//! held in `text` values (`make::text`, `get::text`):
//! calls and answers for the level above, and the requests and responses of their host calls.
//! Each is one line, which Racket reads with `read` and prints with `write`,
//! so values and types are the Redex terms of `sexp`,
//! and answers and responses are the Redex results themselves:
//!
//! ```text
//! call     ::= (load spec)                       $load_ctx($empty_ctx, spec)
//!            | (call-rel "id" (val ...))
//!            | (call-func "id" (typ ...) (val ...))
//!            | (exit)
//! answer   ::= READY                             before the first call
//!            | (OK ())                           load
//!            | (OK (val ...)) | UNMATCH | ERROR  call-rel, a valsres
//!            | val | UNMATCH | ERROR            call-func, a valres
//! request  ::= (builtin "id" (typ ...) (val ...))
//!            | (extern-func "id" (typ ...) (val ...))
//!            | (extern-rel "id" (val ...))
//! response ::= val | UNMATCH | ERROR            builtin and extern-func, a valres
//!            | (OK (val ...)) | UNMATCH | ERROR  extern-rel, a valsres
//! ```
//!
//! ERROR in a response says the host could not answer; the engine reports why.
//! The entries of spec-meta/4-entry.watsup are two calls in a row:
//! `Eval_AL_rel` is `load` then `call-rel`,
//! and `Eval_AL_func` is `load` then `call-func` with no type arguments.

use super::{
    cache::Cache,
    read::{Reader, read_id, read_typs, read_value, read_values},
    sexp,
};
use crate::metacirc::protocol::{Call, HostCall, Output, Values};
use crate::{
    lang::{
        common::source::Span,
        data::{
            arena::Arena,
            value::flat::{self as value, Value, get, make},
        },
        il::ast::Typ,
    },
    metacirc::{Result as MetacircResult, error},
    runner::{Eval, ExternError, InterpreterError},
};
use smallvec::smallvec;
use std::borrow::Cow;

/// The engine's call that loads a specification, `load(request)`, returning C.
pub(super) const LOAD: &str = "load";
/// The engine's call that serves a request under C, `serve(C, request)`.
pub(super) const SERVE: &str = "serve";
/// The host call of every request of the base.
pub(super) const HOST_EVAL: &str = "host_eval";
/// The entry calling a relation, `Eval_AL_rel(spec, id, args)`.
pub(super) const ENTRY_REL: &str = "Eval_AL_rel";
/// The entry calling a function, `Eval_AL_func(spec, id, args)`.
pub(super) const ENTRY_FUNC: &str = "Eval_AL_func";

/// The answer before the first call.
pub(super) const READY: &str = "READY";
/// The answer or response of a mismatch.
const UNMATCH: &str = "UNMATCH";
/// The answer of a call that evaluated to ERROR, or the response of a host call that failed.
pub(super) const ERROR: &str = "ERROR";
/// The call that ends the serve loop.
pub(super) const EXIT: &str = "(exit)";

// == The level above's side

/// `(load spec)`, the call loading the specification printed as `text_spec`.
pub(super) fn load_request(text_spec: &str) -> String {
    format!("({LOAD} {text_spec})")
}

/// Encodes the inputs of the base's entry `name` as s-expression text:
/// `text_spec`, the specification above as the base loads it,
/// `"name"`, and `(val ...)`, which the engine's `call_rel` passes on.
pub(super) fn entry_inputs(
    arena: &mut Arena,
    text_spec: String,
    name: &str,
    values: &[Value],
) -> MetacircResult<Vec<Value>> {
    let mut text_id = String::new();
    sexp::write_string(&mut text_id, name);
    let mut text_args = String::new();
    sexp::write_values(&mut text_args, arena, &mut Cache::new(false), values);
    [text_spec, text_id, text_args]
        .into_iter()
        .map(|text| {
            make::text(arena, text, Span::default())
                .map_err(|error| error::input_invalid(error.to_string()))
        })
        .collect()
}

/// Wraps `call` of the AL level above as `serve(C, request)` on the base.
pub(super) fn lower(
    arena: &mut Arena,
    cache: &mut Cache,
    value_ctx: Value,
    call: &Call<'_>,
) -> Result<Call<'static>, InterpreterError> {
    let request = match call {
        Call::Rel { name, values } => call_rel(arena, cache, name, values),
        Call::Func { name, targs, values } => call_func(arena, cache, name, targs, values),
    };
    let value_request = make::text(arena, request, Span::default()).map_err(ExternError::from)?;
    let values = Values::Lowered(smallvec![value_ctx, value_request]);
    Ok(match call {
        Call::Rel { .. } => Call::Rel { name: SERVE, values },
        Call::Func { .. } => Call::Func { name: SERVE, targs: Cow::Borrowed(&[]), values },
    })
}

/// `(call-rel "id" (val ...))`.
fn call_rel(arena: &Arena, cache: &mut Cache, name: &str, values: &[Value]) -> String {
    let mut out = String::from("(call-rel ");
    sexp::write_string(&mut out, name);
    out.push(' ');
    sexp::write_values(&mut out, arena, cache, values);
    out.push(')');
    out
}

/// `(call-func "id" (typ ...) (val ...))`.
fn call_func(
    arena: &Arena,
    cache: &mut Cache,
    name: &str,
    targs: &[Typ],
    values: &[Value],
) -> String {
    let mut out = String::from("(call-func ");
    sexp::write_string(&mut out, name);
    out.push(' ');
    sexp::write_typs(&mut out, targs);
    out.push(' ');
    sexp::write_values(&mut out, arena, cache, values);
    out.push(')');
    out
}

/// Unwraps the answer of the base into the output of `call`.
///
/// An OK answer gives the outputs; UNMATCH is `InterpreterError::Mismatch`,
/// and ERROR is `InterpreterError::Fatal`, as `ProtocolStSt::raise` does.
pub(super) fn raise(
    arena: &mut Arena,
    cache: &mut Cache,
    output: Output,
    call: &Call<'_>,
) -> Result<Output, InterpreterError> {
    let value_answer = match output {
        Output::Rel(values) => *value::get::one(&values).map_err(ExternError::from)?,
        Output::Func(value) => value,
    };
    let text = get::text(arena, &value_answer)
        .map_err(ExternError::from)?
        .to_owned();
    Ok(match call {
        Call::Rel { .. } => Output::Rel(answer_rel(arena, cache, &text, call.name())?),
        Call::Func { .. } => Output::Func(answer_func(arena, cache, &text, call.name())?),
    })
}

/// Decodes a request of the base for the level above.
///
/// The engine passes each request it receives as
/// `upper.host_call(arena, "host_eval", &[request])`,
/// so `name` is `host_eval` and `values` is the one request.
pub(super) fn host(
    arena: &mut Arena,
    cache: &mut Cache,
    name: &str,
    values: &[Value],
) -> Result<HostCall, ExternError> {
    if name != HOST_EVAL {
        return Err(error::extern_unsupported(name).into());
    }
    let text = get::text(arena, value::get::one(values)?)?.to_owned();
    let mut reader = Reader::new(&text);
    let call = match reader.head("a request")? {
        "builtin" => HostCall::Builtin {
            id: read_id(&mut reader)?,
            targs: read_typs(&mut reader)?,
            values: read_values(&mut reader, arena, cache)?,
        },
        "extern-func" => HostCall::ExternFunc {
            id: read_id(&mut reader)?,
            targs: read_typs(&mut reader)?,
            values: read_values(&mut reader, arena, cache)?,
        },
        "extern-rel" => HostCall::ExternRel {
            id: read_id(&mut reader)?,
            values: read_values(&mut reader, arena, cache)?,
        },
        _ => return Err(Reader::new(&text).unexpected("a request")),
    };
    reader.close()?;
    reader.end()?;
    Ok(call)
}

/// Encodes the outcome of a host call as the text answering the request,
/// the one value that the engine passes back to the base.
///
/// A mismatch is UNMATCH; a fatal failure stays outside the
/// specification's backtracking, for the engine to report.
pub(super) fn reply(
    arena: &mut Arena,
    cache: &mut Cache,
    eval: Eval<Output>,
) -> Eval<Vec<Value>, ExternError> {
    let Eval { result, effect } = eval;
    let response = match result {
        Ok(Output::Func(value)) => {
            let mut out = String::new();
            sexp::write_value(&mut out, arena, cache, value);
            Ok(out)
        }
        Ok(Output::Rel(values)) => {
            let mut out = String::from("(OK ");
            sexp::write_values(&mut out, arena, cache, &values);
            out.push(')');
            Ok(out)
        }
        Err(InterpreterError::Mismatch(_)) => Ok(UNMATCH.to_owned()),
        Err(InterpreterError::Fatal(report)) => Err(ExternError::from(report)),
    };
    let result = response.and_then(|response| {
        let value = make::text(arena, response, Span::default())?;
        Ok(vec![value])
    });
    Eval::new(result, effect)
}

/// Whether `response`, a reply to a host request, carries outputs.
pub(super) fn is_ok(response: &str) -> bool {
    response != UNMATCH && response != ERROR
}

// == The engine's side

/// The calls that run the base's entry `name` on `values`,
/// the texts `spec`, `"id"`, and `(val ...)` that `entry_inputs` made.
///
/// Both entries load first, `(load spec)`; `Eval_AL_rel` then calls
/// `(call-rel "id" (val ...))`, and `Eval_AL_func` `(call-func "id" () (val ...))`.
pub(super) fn entry_requests(
    arena: &Arena,
    name: &str,
    values: &[Value],
) -> Result<[String; 2], InterpreterError> {
    let [value_spec, value_id, value_args] = values else {
        let message = format!("{name} of the base takes a spec, an id, and inputs");
        return Err(ExternError::diagnostic_message(message).into());
    };
    let text_spec = get::text(arena, value_spec).map_err(ExternError::from)?;
    let text_id = get::text(arena, value_id).map_err(ExternError::from)?;
    let text_args = get::text(arena, value_args).map_err(ExternError::from)?;
    // Load the specification, as $load_ctx($empty_ctx, spec) does
    let request_load = load_request(text_spec);
    // Call the entry's relation or function on the loaded specification
    let request_call = if name == ENTRY_FUNC {
        format!("(call-func {text_id} () {text_args})")
    } else {
        format!("(call-rel {text_id} {text_args})")
    };
    Ok([request_load, request_call])
}

/// The outputs of the base's entry `name` from the answer to its call:
/// the relation's values for `Eval_AL_rel`, the function's value for `Eval_AL_func`.
pub(super) fn entry_outputs(
    arena: &mut Arena,
    name: &str,
    answer: &str,
) -> Result<Vec<Value>, InterpreterError> {
    let mut cache = Cache::new(false);
    if name == ENTRY_FUNC {
        Ok(vec![answer_func(arena, &mut cache, answer, name)?])
    } else {
        answer_rel(arena, &mut cache, answer, name)
    }
}

/// Checks the answer to a load, `(OK ())`.
pub(super) fn load_answer(arena: &mut Arena, answer: &str) -> Result<(), InterpreterError> {
    match &answer_rel(arena, &mut Cache::new(false), answer, LOAD)?[..] {
        [] => Ok(()),
        _ => Err(Reader::new(answer)
            .unexpected("the answer to a load, (OK ())")
            .into()),
    }
}

// == Answers

/// The failure that UNMATCH or ERROR answers to `name`, if `text` is one.
fn read_failure(text: &str, name: &str) -> Option<InterpreterError> {
    match text.trim() {
        UNMATCH => Some(InterpreterError::Mismatch(vec![*error::execution_unmatched(name)])),
        ERROR => Some(InterpreterError::Fatal(error::execution_failed(name))),
        _ => None,
    }
}

/// The values of `(OK (val ...))`, the answer of a relation.
fn answer_rel(
    arena: &mut Arena,
    cache: &mut Cache,
    text: &str,
    name: &str,
) -> Result<Vec<Value>, InterpreterError> {
    if let Some(failure) = read_failure(text, name) {
        return Err(failure);
    }
    let mut reader = Reader::new(text);
    if reader.head("an answer")? != "OK" {
        return Err(Reader::new(text).unexpected("an answer").into());
    }
    let values = read_values(&mut reader, arena, cache)?;
    reader.close()?;
    reader.end()?;
    Ok(values)
}

/// The value of `val`, the answer of a function.
fn answer_func(
    arena: &mut Arena,
    cache: &mut Cache,
    text: &str,
    name: &str,
) -> Result<Value, InterpreterError> {
    if let Some(failure) = read_failure(text, name) {
        return Err(failure);
    }
    let mut reader = Reader::new(text);
    let value = read_value(&mut reader, arena, cache)?;
    reader.end()?;
    Ok(value)
}
