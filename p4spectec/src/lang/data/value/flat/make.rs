//! Constructors that allocate a value in an arena
//!
//! Constructors pair each body with its type and span through `Value::new`.
//! Case construction interns the shared mixop before allocating the value.

use std::rc::Rc;

use crate::util::json::json;

use crate::lang::{
    common::{
        Id, TId,
        prim::num::{self, Number},
        source::Span,
    },
    data::{
        arena::Arena,
        notation,
        typ::{self, Typ, TypKind},
    },
};

use super::super::ValueError;
use super::{Value, ValueField, ValueKind};

// - General

impl Value {
    /// Allocates a value of the given kind, type, and span.
    pub fn new(
        arena: &mut Arena,
        kind: ValueKind,
        typ: Rc<TypKind>,
        span: Span,
    ) -> Result<Self, ValueError> {
        arena.value.alloc(&arena.mixop, kind, typ, span)
    }
}

// - Primitives

/// A boolean.
pub fn bool(arena: &mut Arena, value: bool, span: Span) -> Result<Value, ValueError> {
    thread_local! {
        static TYP: Rc<TypKind> = Rc::new(TypKind::Bool);
    }
    TYP.with(|typ| Value::new(arena, ValueKind::Bool(value), typ.clone(), span))
}

/// A number, typed by its kind.
pub fn num(arena: &mut Arena, value: Number, span: Span) -> Result<Value, ValueError> {
    thread_local! {
        static TYP_NAT: Rc<TypKind> = Rc::new(TypKind::Num(num::Typ::Nat));
        static TYP_INT: Rc<TypKind> = Rc::new(TypKind::Num(num::Typ::Int));
    }
    let typ = match num::to_typ(&value) {
        num::Typ::Nat => TYP_NAT.with(Rc::clone),
        num::Typ::Int => TYP_INT.with(Rc::clone),
    };
    Value::new(arena, ValueKind::Num(value), typ, span)
}

/// A natural number.
pub fn nat(arena: &mut Arena, value: num::Natural, span: Span) -> Result<Value, ValueError> {
    num(arena, Number::Nat(value), span)
}

/// An integer.
pub fn int(arena: &mut Arena, value: num_bigint::BigInt, span: Span) -> Result<Value, ValueError> {
    num(arena, Number::Int(value), span)
}

/// A text.
pub fn text(arena: &mut Arena, value: String, span: Span) -> Result<Value, ValueError> {
    thread_local! {
        static TYP: Rc<TypKind> = Rc::new(TypKind::Text);
    }
    TYP.with(|typ| Value::new(arena, ValueKind::Text(value), typ.clone(), span))
}

// - Structures

/// A struct with the given fields.
pub fn structure(
    arena: &mut Arena,
    typ: Rc<TypKind>,
    value_fields: Vec<ValueField>,
    span: Span,
) -> Result<Value, ValueError> {
    Value::new(arena, ValueKind::Struct(value_fields), typ, span)
}

// - Cases

/// A variant case whose notation already belongs to the value's arena.
pub fn case_flat(
    arena: &mut Arena,
    typ: Rc<TypKind>,
    mixfix: notation::flat::Mixfix<Value>,
    span: Span,
) -> Result<Value, ValueError> {
    Value::new(arena, ValueKind::Case(mixfix), typ, span)
}

/// A variant case from a mixop text, its arguments, and its type name.
macro_rules! case_string {
    (
        arena: $arena:expr,
        shape: $shape:expr,
        args: $args:expr,
        typ: $typ:expr,
        span: $span:expr $(,)?
    ) => {{
        let (mixop_text, args, typ_name, span) = ($shape, $args, $typ, $span);
        let arena = &mut *($arena);
        use $crate::lang::data::notation::flat::parse::ToMixop as _;
        mixop_text
            .to_mixop(&mut arena.mixop)
            .map_err(|error| match error {
                $crate::lang::data::notation::flat::parse::ParseMixopError::Syntax(error) => {
                    panic!("value constructor contains a valid SpecTec mixop: {error}")
                }
                $crate::lang::data::notation::flat::parse::ParseMixopError::Intern(error) => {
                    $crate::lang::data::value::ValueError::from(error)
                }
            })
            .and_then(|mixop| {
                let mixfix = $crate::lang::data::notation::flat::Mixfix::new(
                    &arena.mixop,
                    mixop,
                    std::iter::IntoIterator::into_iter(args).collect(),
                )
                .expect("mixop arity matches its value constructor");
                let id = $crate::phrase! {
                    node: typ_name.into(),
                    span: $crate::lang::common::source::Span::default(),
                };
                let typ = $crate::lang::data::typ::make::var(id, std::vec::Vec::new());
                $crate::lang::data::value::flat::make::case_flat(
                    arena,
                    std::rc::Rc::new(typ.node),
                    mixfix,
                    span,
                )
            })
    }};
}

pub(crate) use case_string;

// - Sequences

/// A tuple.
pub fn tuple(
    arena: &mut Arena,
    typ: Rc<TypKind>,
    values: Vec<Value>,
    span: Span,
) -> Result<Value, ValueError> {
    Value::new(arena, ValueKind::Tuple(values), typ, span)
}

/// An option.
pub fn opt(
    arena: &mut Arena,
    typ: Rc<TypKind>,
    value: Option<Value>,
    span: Span,
) -> Result<Value, ValueError> {
    Value::new(arena, ValueKind::Opt(value), typ, span)
}

/// A list.
pub fn list(
    arena: &mut Arena,
    typ: Rc<TypKind>,
    values: Vec<Value>,
    span: Span,
) -> Result<Value, ValueError> {
    Value::new(arena, ValueKind::List(values), typ, span)
}

// - Functions

/// A function value, typed by its signature.
pub fn func(
    arena: &mut Arena,
    id: Id,
    tparams: Vec<TId>,
    typs_params: Vec<Typ>,
    typ_ret: Typ,
    span: Span,
) -> Result<Value, ValueError> {
    let typ = typ::make::func(tparams, typs_params, typ_ret).node;
    Value::new(arena, ValueKind::Func(id), Rc::new(typ), span)
}

// - Externals

/// A host-owned value carried as JSON.
pub fn external(
    arena: &mut Arena,
    typ: Rc<TypKind>,
    json: Rc<json>,
    span: Span,
) -> Result<Value, ValueError> {
    Value::new(arena, ValueKind::Extern(json), typ, span)
}
