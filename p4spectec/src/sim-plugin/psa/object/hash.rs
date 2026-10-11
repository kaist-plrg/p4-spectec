//! The PSA `Hash` extern, a hash over a data tuple
//!
//! The constructor's algorithm enumerator maps to a `hash` algorithm name.

use num_bigint::BigInt;
use num_traits::Zero;
use serde::{Deserialize, Serialize};

use crate::lang::{
    common::source::Span,
    data::{
        arena::Arena,
        typ,
        value::flat::{self as value, Value},
    },
};

use crate::runner::{Extern, ExternError, Interface, Interpreter, RunnerContext};

use crate::sim_plugin::{
    error, hash,
    spec::{args, func, pack, unpack},
};

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
/// A hash configured with one algorithm.
pub struct HashExtern {
    /// The `hash` algorithm name.
    pub algo: String,
}

impl HashExtern {
    /// ```text
    /// extern Hash<O>
    /// Hash(PSA_HashAlgorithm_t algo);
    /// ```
    pub fn init(
        arena: &Arena,
        _value_targs: Value,
        value_ids: Value,
        value_args: Value,
    ) -> Result<Self, ExternError> {
        let args = args::assoc(arena, value_ids, value_args)?;
        let value_algo = args::find(&args, "algo")?;
        let (id_enum, id_type) = unpack::p4_enum(arena, &value_algo)?;
        // Only a `PSA_HashAlgorithm_t` enumerator selects the algorithm
        if id_enum != "PSA_HashAlgorithm_t" {
            return Err(error::hash_algorithm_invalid(
                "invalid PSA hash algorithm enum type".to_owned(),
            )
            .into());
        }
        // Map the enumerator to the internal algorithm name
        let algo = match id_type.as_str() {
            "IDENTITY" => "identity",
            "CRC32" => "crc32",
            "CRC16" => "crc16",
            "ONES_COMPLEMENT16" => "csum16",
            algo => algo,
        }
        .to_owned();
        Ok(Self { algo })
    }

    /// Compute and return the hash for `data`
    ///
    /// `O get_hash<D>(in D data);`
    pub fn get_hash<Interp, Iface, Ext>(
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
        let value_data = func::find_var_e_local(ctx, value_ctx, "data")?;
        let values = unpack::p4_tuple(ctx.arena(), &value_data)?;
        let int_hash = hash::compute_checksum(ctx.arena(), &self.algo, None, &values)?;
        self.return_hash(ctx, value_ctx, value_arch, int_hash)
    }

    /// Compute the hash for `data`, reduce it modulo `max`, then add `base`
    ///
    /// `base` specifies the minimum return value. `max` is the hash modulus;
    /// an implementation may limit its largest supported value, for example
    /// to 32 or 256, and may only support powers of two. P4 developers should
    /// limit their choice to such values to maximize portability
    ///
    /// Returns `base + (h % max)`, where `h` is the hash value
    ///
    /// `O get_hash<T, D>(in T base, in D data, in T max);`
    pub fn get_hash_adjust<Interp, Iface, Ext>(
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
        let value_base = func::find_var_e_local(ctx, value_ctx, "base")?;
        let base = unpack::p4_fixed_bit(ctx.arena(), &value_base)?.1;
        let value_max = func::find_var_e_local(ctx, value_ctx, "max")?;
        let max = unpack::p4_fixed_bit(ctx.arena(), &value_max)?.1;
        let value_data = func::find_var_e_local(ctx, value_ctx, "data")?;
        let values = unpack::p4_tuple(ctx.arena(), &value_data)?;
        let int_hash = hash::compute_checksum(ctx.arena(), &self.algo, None, &values)?;
        if max <= BigInt::zero() {
            return Err(
                error::hash_range_invalid("hash modulus must be positive".to_owned()).into()
            );
        }
        let int_hash = ((int_hash % &max) + &max) % &max + base;
        self.return_hash(ctx, value_ctx, value_arch, int_hash)
    }

    /// Casts the hash to the output type `O` and returns it.
    fn return_hash<Interp, Iface, Ext>(
        self,
        ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
        value_ctx: Value,
        value_arch: Value,
        int_hash: BigInt,
    ) -> Result<(Self, Value, Value, Value), ExternError>
    where
        Iface: Interface,
        Ext: Extern,
        Interp: Interpreter<Iface, Ext>,
    {
        let value_typ = func::find_type_e_local(ctx, value_ctx, "O")?;
        let value_result = pack::p4_arbitrary_int(ctx.arena_mut(), int_hash)?;
        let value_result = func::cast_op(ctx, value_typ, value_result)?;
        let typ = typ::make::opt(typ::make::var(
            crate::phrase!(node: "value".to_owned(), span: Span::default()),
            Vec::new(),
        ));
        let value_opt = value::make::opt(
            ctx.arena_mut(),
            typ.node.into(),
            Some(value_result),
            Span::default(),
        )?;
        let value_call_result = value::make::case_shaped! {
            arena: ctx.arena_mut(),
            shape: "RETURN value?",
            args: vec![value_opt],
            typ: "returnResult",
            span: Span::default(),
        }?;
        Ok((self, value_ctx, value_arch, value_call_result))
    }
}
