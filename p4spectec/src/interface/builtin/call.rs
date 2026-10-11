//! Dispatch of specification builtin calls to their Rust implementations
//!
//! Construction installs the standard entries in specification order
//! and then applies interface-specific overrides.
//! Invocation resolves one name and calls its implementation;
//! for example, `sum_nat` dispatches to `nats::sum_nat`,
//! while `fresh_typeId` advances state hidden in `fresh`
//! and is therefore marked impure, so its results are never cached.

use std::collections::HashMap;

use crate::lang::data::{arena::Arena, value::flat::Value};

use crate::lang::il::ast::{Id, Typ};

use super::{BuiltinError, fresh, ints, lists, maps, nats, numerics, sets, texts};

// == Extensibility point: extra or override builtins per interface

/// A builtin body: arena, type arguments, values in; a value out.
pub type BuiltinImpl = Box<dyn FnMut(&mut Arena, &[Typ], &[Value]) -> Result<Value, BuiltinError>>;

/// A registered builtin and whether calling it has effects.
enum BuiltinEntry {
    /// No effects; the interpreter may memoize the result.
    Pure(BuiltinImpl),
    /// Has effects; every call runs.
    Impure(BuiltinImpl),
}

// == Builtin registry

/// The builtin table of one interface.
pub struct Builtins {
    /// Entries by the specification's builtin name.
    funcs: HashMap<String, BuiltinEntry>,
}

impl Default for Builtins {
    fn default() -> Self {
        Self::new()
    }
}

impl Builtins {
    /// The standard builtins alone.
    pub fn new() -> Self {
        Self::with_extensions([])
    }

    /// The standard builtins plus `entries`,
    /// which override by name and count as pure.
    pub fn with_extensions<const N: usize>(entries: [(&str, BuiltinImpl); N]) -> Self {
        let mut funcs = HashMap::from([
            // Nats
            ("sum_nat".to_owned(), BuiltinEntry::Pure(Box::new(nats::sum_nat))),
            ("max_nat".to_owned(), BuiltinEntry::Pure(Box::new(nats::max_nat))),
            ("min_nat".to_owned(), BuiltinEntry::Pure(Box::new(nats::min_nat))),
            // Ints
            ("sum_int".to_owned(), BuiltinEntry::Pure(Box::new(ints::sum_int))),
            ("max_int".to_owned(), BuiltinEntry::Pure(Box::new(ints::max_int))),
            ("min_int".to_owned(), BuiltinEntry::Pure(Box::new(ints::min_int))),
            // Texts
            ("text_to_int".to_owned(), BuiltinEntry::Pure(Box::new(texts::text_to_int))),
            ("int_to_text".to_owned(), BuiltinEntry::Pure(Box::new(texts::int_to_text))),
            ("split_text".to_owned(), BuiltinEntry::Pure(Box::new(texts::split_text))),
            ("strip_prefix".to_owned(), BuiltinEntry::Pure(Box::new(texts::strip_prefix))),
            ("strip_suffix".to_owned(), BuiltinEntry::Pure(Box::new(texts::strip_suffix))),
            (
                "strip_all_whitespace".to_owned(),
                BuiltinEntry::Pure(Box::new(texts::strip_all_whitespace)),
            ),
            // Lists
            ("rev_".to_owned(), BuiltinEntry::Pure(Box::new(lists::rev_))),
            ("concat_".to_owned(), BuiltinEntry::Pure(Box::new(lists::concat_))),
            ("distinct_".to_owned(), BuiltinEntry::Pure(Box::new(lists::distinct_))),
            ("partition_".to_owned(), BuiltinEntry::Pure(Box::new(lists::partition_))),
            ("assoc_".to_owned(), BuiltinEntry::Pure(Box::new(lists::assoc_))),
            ("sort_".to_owned(), BuiltinEntry::Pure(Box::new(lists::sort_))),
            ("transpose_".to_owned(), BuiltinEntry::Pure(Box::new(lists::transpose_))),
            // Sets
            ("intersect_set".to_owned(), BuiltinEntry::Pure(Box::new(sets::intersect_set))),
            ("union_set".to_owned(), BuiltinEntry::Pure(Box::new(sets::union_set))),
            ("unions_set".to_owned(), BuiltinEntry::Pure(Box::new(sets::unions_set))),
            ("diff_set".to_owned(), BuiltinEntry::Pure(Box::new(sets::diff_set))),
            ("sub_set".to_owned(), BuiltinEntry::Pure(Box::new(sets::sub_set))),
            ("eq_set".to_owned(), BuiltinEntry::Pure(Box::new(sets::eq_set))),
            // Maps
            ("find_map".to_owned(), BuiltinEntry::Pure(Box::new(maps::find_map))),
            ("find_maps".to_owned(), BuiltinEntry::Pure(Box::new(maps::find_maps))),
            ("add_map".to_owned(), BuiltinEntry::Pure(Box::new(maps::add_map))),
            ("adds_map".to_owned(), BuiltinEntry::Pure(Box::new(maps::adds_map))),
            ("update_map".to_owned(), BuiltinEntry::Pure(Box::new(maps::update_map))),
            // Fresh type id
            ("fresh_typeId".to_owned(), BuiltinEntry::Impure(Box::new(fresh::fresh_type_id))),
            // Numerics
            ("shl".to_owned(), BuiltinEntry::Pure(Box::new(numerics::shl))),
            ("shr".to_owned(), BuiltinEntry::Pure(Box::new(numerics::shr))),
            ("shr_arith".to_owned(), BuiltinEntry::Pure(Box::new(numerics::shr_arith))),
            ("pow2".to_owned(), BuiltinEntry::Pure(Box::new(numerics::pow2))),
            ("bitstr_to_int".to_owned(), BuiltinEntry::Pure(Box::new(numerics::bitstr_to_int))),
            ("int_to_bitstr".to_owned(), BuiltinEntry::Pure(Box::new(numerics::int_to_bitstr))),
            (
                "bits_to_int_unsigned".to_owned(),
                BuiltinEntry::Pure(Box::new(numerics::bits_to_int_unsigned)),
            ),
            (
                "bits_to_int_signed".to_owned(),
                BuiltinEntry::Pure(Box::new(numerics::bits_to_int_signed)),
            ),
            (
                "int_to_bits_unsigned".to_owned(),
                BuiltinEntry::Pure(Box::new(numerics::int_to_bits_unsigned)),
            ),
            (
                "int_to_bits_signed".to_owned(),
                BuiltinEntry::Pure(Box::new(numerics::int_to_bits_signed)),
            ),
            ("bneg".to_owned(), BuiltinEntry::Pure(Box::new(numerics::bneg))),
            ("band".to_owned(), BuiltinEntry::Pure(Box::new(numerics::band))),
            ("bxor".to_owned(), BuiltinEntry::Pure(Box::new(numerics::bxor))),
            ("bor".to_owned(), BuiltinEntry::Pure(Box::new(numerics::bor))),
            ("bitacc".to_owned(), BuiltinEntry::Pure(Box::new(numerics::bitacc))),
            ("bitacc_replace".to_owned(), BuiltinEntry::Pure(Box::new(numerics::bitacc_replace))),
        ]);
        // Extensions merge last so an interface can override standard entries
        for (name, builtin_impl) in entries {
            funcs.insert(name.to_owned(), BuiltinEntry::Pure(builtin_impl));
        }
        Self { funcs }
    }

    // - Initialization

    /// Resets builtin state between programs.
    pub fn init(&mut self) {
        fresh::init();
    }

    // - Calls

    /// Calls a builtin by name; the flag reports whether it had effects.
    pub fn invoke(
        &mut self,
        arena: &mut Arena,
        id: &Id,
        targs: &[Typ],
        values: &[Value],
    ) -> Result<(Value, bool), BuiltinError> {
        // An undeclared builtin is a hard error, not a mismatch
        let entry = self
            .funcs
            .get_mut(&id.node)
            .ok_or_else(|| BuiltinError::implementation_missing(&id.node))?;
        let (value, side_effected) = match entry {
            // Pure results may be memoized by the interpreter
            BuiltinEntry::Pure(builtin_impl) => {
                let value = builtin_impl(arena, targs, values)?;
                (value, false)
            }
            // Impure results may not
            BuiltinEntry::Impure(builtin_impl) => {
                let value = builtin_impl(arena, targs, values)?;
                (value, true)
            }
        };
        Ok((value, side_effected))
    }
}
