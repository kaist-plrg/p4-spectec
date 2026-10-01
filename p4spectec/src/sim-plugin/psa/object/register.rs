//! The PSA `Register` extern, an array the program reads and writes
//!
//! Elements start at `initial_value` or the element type's default;
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
    spec::{args, func, unpack},
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
    /// Instantiate an array of `size` registers with undefined initial values,
    /// or initialize every register to the supplied `initial_value`
    ///
    /// ```text
    /// extern Register<T, S>
    /// Register(bit<32> size);
    /// Register(bit<32> size, T initial_value);
    /// ```
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
        // Exactly two type arguments: the element and index types
        let [value_typ, _] = values_targ else {
            return Err(error::register_type_argument_arity_mismatch(format!(
                "Register constructor expects 2 type arguments, but {} were given",
                values_targ.len()
            ))
            .into());
        };
        let value_typ = *value_typ;
        let args = args::assoc(ctx.arena(), value_ids, value_args)?;
        let value_size = args::find(&args, "size")?;
        // The second constructor supplies an initial value
        let value_initial = match args.iter().find(|(name, _)| name == "initial_value") {
            Some((_, value)) => *value,
            None => func::default(ctx, value_typ)?,
        };
        let size = usize::try_from(&unpack::p4_fixed_bit(ctx.arena(), &value_size)?.1)?;
        Ok(Self { value_typ, values: vec![value_initial; size] })
    }

    /// `T read(in S index);`
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
        // Out of range: the element type's default
        let value = match self.values.get(idx) {
            Some(value) => *value,
            None => func::default(ctx, self.value_typ)?,
        };
        // Return without a value
        let typ = typ::make::opt(typ::make::var(
            crate::phrase!(node: "value".to_owned(), span: Span::default()),
            Vec::new(),
        ));
        let value_opt = make::opt(ctx.arena_mut(), typ.node.into(), Some(value), Span::default())?;
        let value_call_result = make::case_shaped! {
            arena: ctx.arena_mut(),
            shape: "RETURN value?",
            args: vec![value_opt],
            typ: "returnResult",
            span: Span::default(),
        }?;
        Ok((self, value_ctx, value_arch, value_call_result))
    }

    /// `void write(in S index, in T value);`
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
