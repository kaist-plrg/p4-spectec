//! The P4 operations on types, values, and variables
//!
//! `P4Ops` answers questions about P4 types and values,
//! such as `env.max_size_in_bits(&typ)` or `sim.cast(&typ, p4value)`.
//! `P4Vars` reads and writes P4 variables by name or dotted path,
//! such as `env.get_local("index")`
//! or `sim.set_global("standard_metadata.egress_spec", port)`.
//! `ExternEnv`, `InitEnv`, and `Sim` implement `P4Ops`,
//! and `ExternEnv` and `Sim` implement `P4Vars`;
//! the `impl` blocks at the bottom give each environment the reads only it makes.
//! Each method's documentation names the specification definition it calls.

use p4spectec::lang::{data::value::flat::Value, traits::print::Print};

use p4spectec::runner::ExternError;

use crate::error;

use crate::p4::{CompileTimeEnv, ExternEnv, InitEnv, P4Type, P4Value};

use super::{
    SpecValue,
    bridge::{FromSpec, ToSpec},
    call::{
        args, func,
        rel::{self, Cursor, StorageReference},
    },
    host::SpecHost,
};

// == Types and values

/// Operations on P4 types and values.
pub trait P4Ops {
    /// The runner the operations call the specification through.
    fn host(&mut self) -> &mut dyn SpecHost;

    /// The value a variable of type `typ` starts with; calls `$default`.
    fn default_of(&mut self, typ: &P4Type) -> Result<P4Value, ExternError> {
        let host = self.host();
        let value = func::default(host, typ.0.0)?;
        P4Value::from_spec(host.arena_mut(), &value)
    }

    /// `(typ) p4value`, the cast of `p4value` to `typ`; calls `$cast_op`.
    fn cast(&mut self, typ: &P4Type, p4value: P4Value) -> Result<P4Value, ExternError> {
        let host = self.host();
        let value = p4value.to_spec(host.arena_mut())?;
        let value = func::cast_op(host, typ.0.0, value)?;
        P4Value::from_spec(host.arena_mut(), &value)
    }

    /// `T.minSizeInBits()` of the type `typ`; calls `$sizeof_minSizeInBits'`.
    fn min_size_in_bits(&mut self, typ: &P4Type) -> Result<usize, ExternError> {
        let size = func::sizeof_min_size_in_bits(self.host(), typ.0.0)?;
        size_to_usize(&size)
    }

    /// `T.maxSizeInBits()` of the type `typ`; calls `$sizeof_maxSizeInBits'`.
    fn max_size_in_bits(&mut self, typ: &P4Type) -> Result<usize, ExternError> {
        let size = func::sizeof_max_size_in_bits(self.host(), typ.0.0)?;
        size_to_usize(&size)
    }

    /// The bits of `p4value`, as `emit` writes them; calls `$write_bits_from_value`.
    #[allow(clippy::wrong_self_convention, reason = "self is the environment, not the value")]
    fn to_bits(&mut self, p4value: &P4Value) -> Result<Vec<bool>, ExternError> {
        let host = self.host();
        let value = p4value.to_spec(host.arena_mut())?;
        let value_bits = func::write_bits_from_value(host, value)?;
        Vec::from_spec(host.arena_mut(), &value_bits)
    }

    /// `p4value` with its fields filled from `bits`, as `extract` reads them,
    /// a variable-size field taking `width_varsize` bits;
    /// calls `$write_value_from_bits`.
    #[allow(clippy::wrong_self_convention, reason = "self is the environment, not the value")]
    fn from_bits(
        &mut self,
        p4value: P4Value,
        width_varsize: usize,
        bits: &[bool],
    ) -> Result<P4Value, ExternError> {
        let host = self.host();
        let value = p4value.to_spec(host.arena_mut())?;
        let value = func::write_value_from_bits(host, value, width_varsize, bits)?;
        P4Value::from_spec(host.arena_mut(), &value)
    }

    /// `p4value` as the specification prints it, for log output; calls no definition.
    fn format(&mut self, p4value: &P4Value) -> Result<String, ExternError> {
        let host = self.host();
        let value = p4value.to_spec(host.arena_mut())?;
        Ok(value.view(host.arena()).to_string())
    }
}

/// A type size as a `usize`.
fn size_to_usize(size: &num_bigint::BigInt) -> Result<usize, ExternError> {
    usize::try_from(size).map_err(|_| {
        error::packet_size_invalid("type size does not fit in usize".to_owned()).into()
    })
}

// == Variables

/// Reads and writes of P4 variables:
/// the local ones of the extern call being run,
/// such as its parameters, and the global ones.
pub trait P4Vars {
    /// The runner and the evaluation context the variables live in.
    fn host_ctx(&mut self) -> (&mut dyn SpecHost, &mut SpecValue);

    /// The architecture state, which the specification reads and writes
    /// variables through.
    fn value_arch(&self) -> SpecValue;

    /// The local variable `name`, such as the parameter `index`;
    /// calls `$find_var_e` at `LOCAL`.
    fn get_local(&mut self, name: &str) -> Result<P4Value, ExternError> {
        let (host, value_ctx) = self.host_ctx();
        let value = func::find_var_e_local(host, value_ctx.0, name)?;
        P4Value::from_spec(host.arena_mut(), &value)
    }

    /// The global variable or field at the dotted `path`,
    /// such as `standard_metadata.egress_spec`;
    /// calls `Lvalue_read` at `GLOBAL`.
    fn get_global(&mut self, path: &str) -> Result<P4Value, ExternError> {
        let reference = StorageReference::from_path(path);
        let value_arch = self.value_arch();
        let (host, value_ctx) = self.host_ctx();
        let value = rel::lvalue_read(host, Cursor::Global, value_ctx.0, value_arch.0, &reference)?;
        P4Value::from_spec(host.arena_mut(), &value)
    }

    /// Assigns `p4value` to the local variable or field at the dotted `path`,
    /// such as the `out` parameter `result`
    /// or `standard_metadata.egress_spec` of an `inout` parameter;
    /// calls `Lvalue_write` at `LOCAL`.
    fn set_local(&mut self, path: &str, p4value: impl Into<P4Value>) -> Result<(), ExternError> {
        set(self, Cursor::Local, path, p4value.into())
    }

    /// Assigns `p4value` to the global variable or field at the dotted `path`,
    /// such as `standard_metadata.instance_type`;
    /// calls `Lvalue_write` at `GLOBAL`.
    fn set_global(&mut self, path: &str, p4value: impl Into<P4Value>) -> Result<(), ExternError> {
        set(self, Cursor::Global, path, p4value.into())
    }
}

/// Assigns `p4value` to the variable or field at the dotted `path` found from `cursor`.
fn set<Vars: P4Vars + ?Sized>(
    vars: &mut Vars,
    cursor: Cursor,
    path: &str,
    p4value: P4Value,
) -> Result<(), ExternError> {
    let reference = StorageReference::from_path(path);
    let value_arch = vars.value_arch();
    let (host, value_ctx) = vars.host_ctx();
    let value = p4value.to_spec(host.arena_mut())?;
    value_ctx.0 = rel::lvalue_write(host, cursor, value_ctx.0, value_arch.0, &reference, value)?;
    Ok(())
}

// == Environment reads

impl ExternEnv<'_> {
    /// The type argument `name`, such as `T` of `extract<T>`,
    /// with the caller's type variables resolved;
    /// calls `$find_type_e` and `$subst_type_e` at `LOCAL`.
    pub fn get_type(&mut self, name: &str) -> Result<P4Type, ExternError> {
        let value_typ = func::find_type_e_local(self.host, self.value_ctx.0, name)?;
        let value_typ = func::subst_type_e_local(self.host, self.value_ctx.0, value_typ)?;
        Ok(P4Type(SpecValue(value_typ)))
    }
}

impl CompileTimeEnv<'_> {
    /// The local variable `name`, such as the parameter `check` of `static_assert`;
    /// calls `$find_var_value_t` at `LOCAL`.
    pub fn get_local(&mut self, name: &str) -> Result<P4Value, ExternError> {
        let value = func::find_var_value_t_local(self.host, &self.value_ctx.0, name)?;
        P4Value::from_spec(self.host.arena_mut(), &value)
    }
}

impl<'a> InitEnv<'a> {
    /// Starts a constructor call from its type arguments,
    /// parameter names, and arguments; the name and argument counts must match.
    pub(crate) fn new(
        host: &'a mut dyn SpecHost,
        value_targs: SpecValue,
        value_ids: SpecValue,
        value_args: SpecValue,
    ) -> Result<Self, ExternError> {
        let values_typ = Vec::<Value>::from_spec(host.arena_mut(), &value_targs.0)?;
        let types = values_typ
            .into_iter()
            .map(|value| P4Type(SpecValue(value)))
            .collect();
        let args = args::assoc(host.arena_mut(), value_ids.0, value_args.0)?
            .into_iter()
            .map(|(name, value)| (name, SpecValue(value)))
            .collect();
        Ok(Self { host, types, args })
    }

    /// The value of the argument `name`, if the constructor overload has it.
    pub fn get_opt(&mut self, name: &str) -> Result<Option<P4Value>, ExternError> {
        let value = self
            .args
            .iter()
            .find(|(name_arg, _)| name_arg == name)
            .map(|(_, value)| value.0);
        value
            .map(|value| P4Value::from_spec(self.host.arena_mut(), &value))
            .transpose()
    }
}
