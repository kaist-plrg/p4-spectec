//! Shared value types, arena storage, constructors, and projections
//!
//! A `Value` is a handle into a `ValueArena`;
//! `make` allocates values of each kind with their type,
//! `get` projects a kind back out or fails with `ValueError`.
//! Primitive types are allocated once per thread and shared.

mod arena;
pub mod external;
#[allow(clippy::module_inception, reason = "separate facade and implementation")]
mod value;

use std::rc::Rc;

use crate::util::json::json;

use crate::lang::{
    common::{
        Id, TId,
        notation::mixfix::Mixfix,
        prim::num::{self, Number},
        source::Span,
    },
    data::typ::{self, Typ, TypKind},
};

pub use arena::ValueArena;
pub use value::*;

// = Smart constructors

/// Constructors that allocate a value in an arena.
pub mod make {
    use super::*;

    // - General

    /// Allocates a value of the given kind, type, and span.
    pub fn new(
        arena: &mut ValueArena,
        kind: ValueKind,
        typ: Rc<TypKind>,
        span: Span,
    ) -> Result<Value, ValueError> {
        arena.alloc(kind, typ, span)
    }

    // - Primitives

    /// A boolean.
    pub fn bool(arena: &mut ValueArena, value: bool, span: Span) -> Result<Value, ValueError> {
        thread_local! {
            static TYP: Rc<TypKind> = Rc::new(TypKind::Bool);
        }
        TYP.with(|typ| new(arena, ValueKind::Bool(value), typ.clone(), span))
    }

    /// A number, typed by its kind.
    pub fn num(arena: &mut ValueArena, value: Number, span: Span) -> Result<Value, ValueError> {
        thread_local! {
            static TYP_NAT: Rc<TypKind> = Rc::new(TypKind::Num(num::Typ::Nat));
            static TYP_INT: Rc<TypKind> = Rc::new(TypKind::Num(num::Typ::Int));
        }
        let typ = match num::to_typ(&value) {
            num::Typ::Nat => TYP_NAT.with(Rc::clone),
            num::Typ::Int => TYP_INT.with(Rc::clone),
        };
        new(arena, ValueKind::Num(value), typ, span)
    }

    /// A natural number.
    pub fn nat(
        arena: &mut ValueArena,
        value: num::Natural,
        span: Span,
    ) -> Result<Value, ValueError> {
        num(arena, Number::Nat(value), span)
    }

    /// An integer.
    pub fn int(
        arena: &mut ValueArena,
        value: num_bigint::BigInt,
        span: Span,
    ) -> Result<Value, ValueError> {
        num(arena, Number::Int(value), span)
    }

    /// A text.
    pub fn text(arena: &mut ValueArena, value: String, span: Span) -> Result<Value, ValueError> {
        thread_local! {
            static TYP: Rc<TypKind> = Rc::new(TypKind::Text);
        }
        TYP.with(|typ| new(arena, ValueKind::Text(value), typ.clone(), span))
    }

    // - Structures

    /// A struct with the given fields.
    pub fn structure(
        arena: &mut ValueArena,
        typ: Rc<TypKind>,
        value_fields: Vec<ValueField>,
        span: Span,
    ) -> Result<Value, ValueError> {
        new(arena, ValueKind::Struct(value_fields), typ, span)
    }

    // - Cases

    /// A variant case from a filled notation.
    pub fn case(
        arena: &mut ValueArena,
        typ: Rc<TypKind>,
        value_case: Mixfix<Value>,
        span: Span,
    ) -> Result<Value, ValueError> {
        new(arena, ValueKind::Case(value_case), typ, span)
    }

    /// A variant case from a mixop text, its arguments, and its type name.
    macro_rules! case_shaped {
        (
            arena: $arena:expr,
            shape: $shape:expr,
            args: $args:expr,
            typ: $typ:expr,
            span: $span:expr $(,)?
        ) => {{
            let (shape_text, args, typ_name, span) = ($shape, $args, $typ, $span);
            let mixop = $crate::lang::common::notation::mixop::shape(shape_text);
            let value_case =
                $crate::lang::common::notation::mixop::Mixop::fill(mixop.as_ref(), args)
                    .expect("mixop arity matches its value constructor");
            let id = $crate::phrase! {
                node: typ_name.to_owned(),
                span: $crate::lang::common::source::Span::default(),
            };
            let typ = $crate::lang::data::typ::make::var(id, std::vec::Vec::new());
            $crate::lang::data::value::make::case(
                $arena,
                std::rc::Rc::new(typ.node),
                value_case,
                span,
            )
        }};
    }

    pub(crate) use case_shaped;

    // - Sequences

    /// A tuple.
    pub fn tuple(
        arena: &mut ValueArena,
        typ: Rc<TypKind>,
        values: Vec<Value>,
        span: Span,
    ) -> Result<Value, ValueError> {
        new(arena, ValueKind::Tuple(values), typ, span)
    }

    /// An option.
    pub fn opt(
        arena: &mut ValueArena,
        typ: Rc<TypKind>,
        value: Option<Value>,
        span: Span,
    ) -> Result<Value, ValueError> {
        new(arena, ValueKind::Opt(value), typ, span)
    }

    /// A list.
    pub fn list(
        arena: &mut ValueArena,
        typ: Rc<TypKind>,
        values: Vec<Value>,
        span: Span,
    ) -> Result<Value, ValueError> {
        new(arena, ValueKind::List(values), typ, span)
    }

    // - Functions

    /// A function value, typed by its signature.
    pub fn func(
        arena: &mut ValueArena,
        id: Id,
        tparams: Vec<TId>,
        typs_params: Vec<Typ>,
        typ_ret: Typ,
        span: Span,
    ) -> Result<Value, ValueError> {
        let typ = typ::make::func(tparams, typs_params, typ_ret).node;
        new(arena, ValueKind::Func(id), Rc::new(typ), span)
    }

    // - Externals

    /// A host-owned value carried as JSON.
    pub fn external(
        arena: &mut ValueArena,
        typ: Rc<TypKind>,
        json: Rc<json>,
        span: Span,
    ) -> Result<Value, ValueError> {
        new(arena, ValueKind::Extern(json), typ, span)
    }
}

// = Projections

/// Projections that read a kind out of a value or fail.
pub mod get {
    use super::*;

    // - Errors

    /// The error for a value of the wrong kind.
    fn unexpected(arena: &ValueArena, value: &Value, expected: ValueTag) -> ValueError {
        ValueError::KindMismatch { expected, actual: arena.kind(value).tag() }
    }

    // - Primitives

    /// The boolean in a value.
    pub fn bool(arena: &ValueArena, value: &Value) -> Result<bool, ValueError> {
        match arena.kind(value) {
            ValueKind::Bool(value) => Ok(*value),
            _ => Err(unexpected(arena, value, ValueTag::Bool)),
        }
    }

    /// The number in a value.
    pub fn num<'a>(arena: &'a ValueArena, value: &Value) -> Result<&'a Number, ValueError> {
        match arena.kind(value) {
            ValueKind::Num(value) => Ok(value),
            _ => Err(unexpected(arena, value, ValueTag::Num)),
        }
    }

    /// The text in a value.
    pub fn text<'a>(arena: &'a ValueArena, value: &Value) -> Result<&'a str, ValueError> {
        match arena.kind(value) {
            ValueKind::Text(value) => Ok(value),
            _ => Err(unexpected(arena, value, ValueTag::Text)),
        }
    }

    // - Structures

    /// The fields of a struct value.
    pub fn structure<'a>(
        arena: &'a ValueArena,
        value: &Value,
    ) -> Result<&'a [ValueField], ValueError> {
        match arena.kind(value) {
            ValueKind::Struct(value_fields) => Ok(value_fields),
            _ => Err(unexpected(arena, value, ValueTag::Struct)),
        }
    }

    // - Cases

    /// The filled notation of a case value.
    pub fn case<'a>(arena: &'a ValueArena, value: &Value) -> Result<&'a ValueCase, ValueError> {
        match arena.kind(value) {
            ValueKind::Case(value_case) => Ok(value_case),
            _ => Err(unexpected(arena, value, ValueTag::Case)),
        }
    }

    /// Matches a case value against mixop texts, binding its arguments per arm.
    macro_rules! matches {
        (
            @arms $value_case:ident;
            $shape:literal $(| $shape_alt:literal)* => |$values:ident| $body:expr,
            $($rest:tt)+
        ) => {{
            match $value_case {
                Some(value_case)
                    if [$shape, $($shape_alt),*].into_iter().any(|shape_text| {
                        let expected = $crate::lang::common::notation::mixop::shape(shape_text);
                        value_case.eq_shape(expected.as_ref())
                    }) =>
                {
                    let $values = value_case.args();
                    $body
                }
                _ => $crate::lang::data::value::get::matches! {
                    @arms $value_case;
                    $($rest)+
                },
            }
        }};
        (@arms $value_case:ident; _ => $fallback:expr $(,)?) => {
            $fallback
        };
        ($arena:expr, $value:expr, $($arms:tt)+) => {{
            let value = $value;
            let arena = $arena;
            let value_case = match arena.kind(value) {
                $crate::lang::data::value::ValueKind::Case(value_case) => Some(value_case),
                _ => None,
            };
            $crate::lang::data::value::get::matches! {
                @arms value_case;
                $($arms)+
            }
        }};
    }

    pub(crate) use matches;

    // - Sequences

    /// The components of a tuple value.
    pub fn tuple<'a>(arena: &'a ValueArena, value: &Value) -> Result<&'a [Value], ValueError> {
        match arena.kind(value) {
            ValueKind::Tuple(values) => Ok(values),
            _ => Err(unexpected(arena, value, ValueTag::Tuple)),
        }
    }

    /// The content of an option value.
    pub fn opt(arena: &ValueArena, value: &Value) -> Result<Option<Value>, ValueError> {
        match arena.kind(value) {
            ValueKind::Opt(value) => Ok(*value),
            _ => Err(unexpected(arena, value, ValueTag::Opt)),
        }
    }

    /// The elements of a list value.
    pub fn list<'a>(arena: &'a ValueArena, value: &Value) -> Result<&'a [Value], ValueError> {
        match arena.kind(value) {
            ValueKind::List(values) => Ok(values),
            _ => Err(unexpected(arena, value, ValueTag::List)),
        }
    }

    // - Externals

    /// The JSON of a host-owned value.
    pub fn external<'a>(arena: &'a ValueArena, value: &Value) -> Result<&'a Rc<json>, ValueError> {
        match arena.kind(value) {
            ValueKind::Extern(json) => Ok(json),
            _ => Err(unexpected(arena, value, ValueTag::Extern)),
        }
    }

    // - Indexing

    /// The element at `index`, or an out-of-bounds error.
    pub fn nth(values: &[Value], index: usize) -> Result<&Value, ValueError> {
        values
            .get(index)
            .ok_or(ValueError::IndexOutOfBounds { index, len: values.len() })
    }

    // - Arity

    /// Exactly one value.
    pub fn one(values: &[Value]) -> Result<&Value, ValueError> {
        match values {
            [value] => Ok(value),
            _ => Err(ValueError::CountMismatch { expected: 1, actual: values.len() }),
        }
    }

    /// Exactly two values.
    pub fn two(values: &[Value]) -> Result<(&Value, &Value), ValueError> {
        match values {
            [value_a, value_b] => Ok((value_a, value_b)),
            _ => Err(ValueError::CountMismatch { expected: 2, actual: values.len() }),
        }
    }

    /// Exactly three values.
    #[allow(clippy::type_complexity)]
    pub fn three(values: &[Value]) -> Result<(&Value, &Value, &Value), ValueError> {
        match values {
            [value_a, value_b, value_c] => Ok((value_a, value_b, value_c)),
            _ => Err(ValueError::CountMismatch { expected: 3, actual: values.len() }),
        }
    }

    /// Exactly four values.
    #[allow(clippy::type_complexity)]
    pub fn four(values: &[Value]) -> Result<(&Value, &Value, &Value, &Value), ValueError> {
        match values {
            [value_a, value_b, value_c, value_d] => Ok((value_a, value_b, value_c, value_d)),
            _ => Err(ValueError::CountMismatch { expected: 4, actual: values.len() }),
        }
    }
}
