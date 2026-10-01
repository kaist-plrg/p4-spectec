//! The `register` extern, an array the program reads and writes
//!
//! Elements start at the element type's default value;
//! an out-of-range read yields that default,
//! an out-of-range write is ignored.

use serde_derive_state::{DeserializeState, SerializeState};

use crate::lang::{
    common::source::Span,
    data::{
        typ,
        value::{
            Value,
            external::{DecodeContext, EncodeContext},
            make,
        },
    },
};

use crate::runner::{Extern, ExternError, Interface, Interpreter, RunnerContext};

use crate::sim_plugin::{
    error,
    spec::{args, func, rel, unpack},
};

#[derive(Clone, Debug, PartialEq, Eq, SerializeState, DeserializeState)]
#[serde(deny_unknown_fields, serialize_state = "EncodeContext<'arena>", ser_parameters = "'arena")]
#[serde(deserialize_state = "DecodeContext<'de>")]
/// Register array with its element type.
pub struct Register {
    #[serde(state)]
    /// Element type `T`.
    pub value_typ: Value,
    #[serde(state)]
    /// The `size` elements.
    pub values: Vec<Value>,
}

impl Register {
    /// A register object is created by calling its constructor.  This
    /// creates an array of 'size' identical elements, each with type
    /// T.  The array indices are in the range [0, size-1].  For
    /// example, this constructor call:
    ///
    /// ```text
    ///     register<bit<32>>(512) my_reg;
    ///
    /// ```
    /// allocates storage for 512 values, each with type `bit<32>`.
    ///
    /// `register(bit<32> size);`
    pub fn init<Interp, Iface, Ext>(
        ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
        value_targs: Value,
        value_ids: Value,
        value_args: Value,
    ) -> Result<Self, ExternError>
    where
        Iface: Interface,
        Ext: Extern,
        Interp: Interpreter<Iface, Ext>,
    {
        let values_targ = crate::lang::data::value::get::list(ctx.arena(), &value_targs)?;
        // Exactly one type argument, the element type
        let [value_typ] = values_targ else {
            return Err(error::register_type_argument_arity_mismatch(format!(
                "register constructor expects 1 type argument, but {} were given",
                values_targ.len()
            ))
            .into());
        };
        let value_typ = *value_typ;
        let args = args::assoc(ctx.arena(), value_ids, value_args)?;
        let value_size = args::find(&args, "size")?;
        let value_initial = func::default(ctx, value_typ)?;
        let size = usize::try_from(&unpack::p4_fixed_bit(ctx.arena(), &value_size)?.1)?;
        Ok(Self { value_typ, values: vec![value_initial; size] })
    }

    /// `read()` reads the state of the register array stored at the
    /// specified index, and returns it as the value written to the
    /// result parameter.
    ///
    /// @param index The index of the register array element to be
    ///              read, normally a value in the range [0, size-1].
    /// @param result Only types T that are `bit<W>` are currently
    ///              supported.  When index is in range, the value of
    ///              result becomes the value read from the register
    ///              array element.  When index >= size, the final
    ///              value of result is not specified, and should be
    ///              ignored by the caller.
    ///
    /// `void read(out T result, in bit<32> index);`
    pub fn read<Interp, Iface, Ext>(
        self,
        ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
        value_ctx: Value,
        value_arch: Value,
    ) -> Result<(Self, Value, Value, Value), ExternError>
    where
        Iface: Interface,
        Ext: Extern,
        Interp: Interpreter<Iface, Ext>,
    {
        let value_idx = func::find_var_e_local(ctx, value_ctx, "index")?;
        let idx = usize::try_from(&unpack::p4_fixed_bit(ctx.arena(), &value_idx)?.1)?;
        // Out of range: the default, since the result is unspecified
        let value = match self.values.get(idx) {
            Some(value) => *value,
            None => func::default(ctx, self.value_typ)?,
        };
        let value_ctx = rel::lvalue_write_var_local(ctx, value_ctx, value_arch, "result", value)?;
        // Return without a value
        let typ = typ::make::opt(typ::make::var(
            crate::phrase!(node: "value".to_owned(), span: Span::default()),
            Vec::new(),
        ));
        let value_opt = make::opt(ctx.arena_mut(), typ.node.into(), None, Span::default())?;
        let value_call_result = make::case_shaped! {
            arena: ctx.arena_mut(),
            shape: "RETURN value?",
            args: vec![value_opt],
            typ: "returnResult",
            span: Span::default(),
        }?;
        Ok((self, value_ctx, value_arch, value_call_result))
    }

    /// `write()` writes the state of the register array at the specified
    /// index, with the value provided by the value parameter.
    ///
    /// If you wish to perform a `read()` followed later by a `write()` to
    /// the same register array element, and you wish the
    /// read-modify-write sequence to be atomic relative to other
    /// processed packets, then there may be parallel implementations
    /// of the v1model architecture for which you must execute them in
    /// a P4_16 block annotated with an `@atomic` annotation.  See the
    /// P4_16 language specification description of the `@atomic`
    /// annotation for more details.
    ///
    /// @param index The index of the register array element to be
    ///              written, normally a value in the range [0,
    ///              size-1].  If index >= size, no register state will
    ///              be updated.
    /// @param value Only types T that are `bit<W>` are currently
    ///              supported.  When index is in range, this
    ///              parameter's value is written into the register
    ///              array element specified by index.
    /// `void write(in bit<32> index, in T value);`
    pub fn write<Interp, Iface, Ext>(
        mut self,
        ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
        value_ctx: Value,
        value_arch: Value,
    ) -> Result<(Self, Value, Value, Value), ExternError>
    where
        Iface: Interface,
        Ext: Extern,
        Interp: Interpreter<Iface, Ext>,
    {
        let value_idx = func::find_var_e_local(ctx, value_ctx, "index")?;
        let idx = usize::try_from(&unpack::p4_fixed_bit(ctx.arena(), &value_idx)?.1)?;
        let value_target = func::find_var_e_local(ctx, value_ctx, "value")?;
        // Out of range: ignored
        if let Some(value) = self.values.get_mut(idx) {
            *value = value_target;
        }
        // Return without a value
        let typ = typ::make::opt(typ::make::var(
            crate::phrase!(node: "value".to_owned(), span: Span::default()),
            Vec::new(),
        ));
        let value_opt = make::opt(ctx.arena_mut(), typ.node.into(), None, Span::default())?;
        let value_call_result = make::case_shaped! {
            arena: ctx.arena_mut(),
            shape: "RETURN value?",
            args: vec![value_opt],
            typ: "returnResult",
            span: Span::default(),
        }?;
        Ok((self, value_ctx, value_arch, value_call_result))
    }
}
