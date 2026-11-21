//! Core extern functions `static_assert` and `verify`
//!
//! Both read their arguments from the call;
//! `static_assert` evaluates to a boolean at compile time,
//! `verify` returns or rejects the parser.
//! `call_compile_time` dispatches a compile-time call to `static_assert`
//! for every architecture.

use p4spectec::runner::ExternError;

use crate::error;

use crate::p4::{CallResult, CompileTimeEnv, ExternEnv, P4Value, P4Vars, Signature, SpecValue};

use crate::spec::host::SpecHost;

/// Evaluates `static_assert`, the only compile-time known extern function,
/// in the caller's context `value_ctx`.
pub fn call_compile_time(
    host: &mut dyn SpecHost,
    signature: &Signature,
    value_ctx: SpecValue,
) -> Result<P4Value, ExternError> {
    // Both overloads of `static_assert`; nothing else is known
    let (name, params) = signature.as_strs();
    let has_message = match (name, &params[..]) {
        ("static_assert", ["check", "message"]) => true,
        ("static_assert", ["check"]) => false,
        _ => {
            return Err(error::extern_function_unsupported(format!(
                "unsupported local compile-time known extern function call: {signature}"
            ))
            .into());
        }
    };
    let mut env = CompileTimeEnv::new(host, value_ctx);
    static_assert(&mut env, has_message)
}

/// Evaluates a boolean expression at compilation time
/// and stops compilation with the supplied message when it is false.
///
/// The boolean result can initialize a global constant, for example:
/// ```text
/// const bool _check = static_assert(
///     V1MODEL_VERSION > 20180000,
///     "Expected a v1 model version >= 20180000");
/// ```
///
/// The overload without a message uses the default failure message:
/// ```text
/// extern bool static_assert(bool check, string message);
/// extern bool static_assert(bool check);
/// ```
pub fn static_assert(env: &mut CompileTimeEnv, has_message: bool) -> Result<P4Value, ExternError> {
    // Arguments are bound as local variables of the call
    let check = env.get_local("check")?;
    let message = if has_message { Some(env.get_local("message")?) } else { None };
    // A passing assertion evaluates to its check
    if bool::try_from(check.clone())? {
        return Ok(check);
    }
    // The default message when the one-argument overload is used
    let message = match message {
        Some(message) => message.try_into()?,
        None => "static_assert failed".to_owned(),
    };
    Err(error::assertion_unmet(message).into())
}

/// Checks a predicate in the parser, leaving execution unchanged when true.
///
/// A false predicate sets the parser error to `toSignal` and transitions to
/// the `reject` state:
/// ```text
/// extern void verify(in bool check, in error toSignal);
/// ```
pub fn verify(env: &mut ExternEnv) -> Result<CallResult, ExternError> {
    // A true check returns nothing
    let check: bool = env.get_local("check")?.try_into()?;
    if check {
        return Ok(CallResult::Return(None));
    }
    // A false check rejects with the `toSignal` error
    match env.get_local("toSignal")? {
        P4Value::Error(id) => Ok(CallResult::Reject(id)),
        to_signal => Err(to_signal.mismatch("an error")),
    }
}
