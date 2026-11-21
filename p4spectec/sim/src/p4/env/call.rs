//! The extern call the specification hands over
//!
//! The specification makes an extern call through one of three relations;
//! `spec::entry` reads each into its Rust form:
//! `ExternFunctionCall_eval` and `ExternFunctionCall_eval_lctk` a `Signature`,
//! and `ExternMethodCall_eval` a `MethodCall`, a `Signature` on an extern object.
//! A dispatcher matches its `Signature`
//! and starts the environment the extern runs in:
//! `Sim::run_extern` for a function or method call,
//! and `CompileTimeEnv::new` for a compile-time call.

use std::fmt;

use crate::spec::SpecValue;

// == Extern calls

/// The extern call an `ExternEnv` runs, for failure messages.
#[derive(Clone, Copy)]
pub enum ExternCall<'a> {
    /// `ExternFunctionCall_eval`: an extern function called while a packet runs.
    Function(&'a Signature),
    /// `ExternMethodCall_eval`: a method of an extern object.
    Method(&'a MethodCall),
}

impl ExternCall<'_> {
    /// The callee and its parameters, for failure messages:
    /// `name(params)`, or `ids.name(params)` for a method.
    pub fn callee(&self) -> String {
        match self {
            // A function: `name(params)`
            Self::Function(signature) => signature.to_string(),
            // A method: `object.name(params)`
            Self::Method(MethodCall { ids, signature, .. }) => {
                format!("{}.{signature}", ids.join("."))
            }
        }
    }
}

impl<'a> From<&'a Signature> for ExternCall<'a> {
    fn from(signature: &'a Signature) -> Self {
        Self::Function(signature)
    }
}

impl<'a> From<&'a MethodCall> for ExternCall<'a> {
    fn from(call: &'a MethodCall) -> Self {
        Self::Method(call)
    }
}

// == Signatures

/// The callee's name and parameter names, such as `read(result, index)`.
///
/// P4 overloads an extern by its parameter names,
/// so a dispatcher matches both.
#[derive(Clone)]
pub struct Signature {
    /// The function or method name.
    pub name: String,
    /// The parameter names.
    pub params: Vec<String>,
}

impl Signature {
    /// The name and parameter names as string slices, for matching.
    pub fn as_strs(&self) -> (&str, Vec<&str>) {
        (&self.name, self.params.iter().map(String::as_str).collect())
    }
}

impl fmt::Display for Signature {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}({})", self.name, self.params.join(", "))
    }
}

// == Method calls

/// A call of a method of the extern object `ids`,
/// such as `ig.reg.read(result, index)`.
pub struct MethodCall {
    /// The object's specification id.
    pub(crate) value_id: SpecValue,
    /// The object's name path.
    pub(crate) ids: Vec<String>,
    /// The method name and parameter names.
    pub signature: Signature,
}
