//! Extraction of name-resolution metadata from completed P4 parse-tree values
//!
//! Parser semantic actions use these projections to register declaration names,
//! referenced type identifiers, and the presence of type parameters.
//! Each matches the value's mixop text
//! against the grammar productions it may come from.

use crate::lang::data::{
    arena::Arena,
    value::flat::{self as value, Value},
};

use super::{context::TypeId, error::ExtractError};

// == Identifier extraction

/// The text of a `name` value, including the keywords usable as names.
pub(super) fn id_name(arena: &Arena, value: &Value) -> Result<String, ExtractError> {
    let unexpected = || ExtractError::ValueUnexpected("id_name");
    value::get::matches! { arena,
        value,
        // Identifiers carry their text; keyword names spell themselves
        "_ID text" => |values| {
            let text = value::get::text(arena, &values[0]).map_err(|_| unexpected())?;
            Ok(text.to_owned())
        },
        "APPLY" => |_values| Ok("apply".to_owned()),
        "KEY" => |_values| Ok("key".to_owned()),
        "ACTIONS" => |_values| Ok("actions".to_owned()),
        "STATE" => |_values| Ok("state".to_owned()),
        "ENTRIES" => |_values| Ok("entries".to_owned()),
        "TYPE" => |_values| Ok("type".to_owned()),
        "PRIORITY" => |_values| Ok("priority".to_owned()),
        "_TID text" => |values| {
            let text = value::get::text(arena, &values[0]).map_err(|_| unexpected())?;
            Ok(text.to_owned())
        },
        "LIST" => |_values| Ok("list".to_owned()),
        _ => Err(unexpected()),
    }
}

/// The function name of a `functionPrototype` value.
pub(super) fn id_function_prototype(arena: &Arena, value: &Value) -> Result<String, ExtractError> {
    value::get::matches! { arena,
        value,
        "typeOrVoid name typeParameterListOpt `( parameterList `)" => |values| {
            id_name(arena, &values[1])
        },
        _ => Err(ExtractError::ValueUnexpected("id_function_prototype")),
    }
}

/// The declared name of any declaration value.
pub(super) fn id_declaration(arena: &Arena, value: &Value) -> Result<String, ExtractError> {
    value::get::matches! { arena,
        value,
        // One arm per declaration production, picking out its `name`
        "annotationList CONST type name initializer ';'" => |values| id_name(arena, &values[2]),
        "annotationList type `( argumentList `) name ';'" => |values| id_name(arena, &values[3]),
        "annotationList type `( argumentList `) name objectInitializer ';'" => |values| {
            id_name(arena, &values[3])
        },
        "annotationList functionPrototype blockStatement" => |values| {
            id_function_prototype(arena, &values[1])
        },
        "annotationList ACTION name `( parameterList `) blockStatement" => |values| {
            id_name(arena, &values[1])
        },
        "annotationList EXTERN functionPrototype ';'" => |values| {
            id_function_prototype(arena, &values[1])
        },
        "annotationList EXTERN nonTypeName typeParameterListOpt `{ externConstructorOrMethodPrototypeList `}" => |values| {
            id_name(arena, &values[1])
        },
        "annotationList PARSER name typeParameterListOpt `( parameterList `) constructorParameterListOpt `{ parserLocalDeclarationList parserStateList `}" => |values| {
            id_name(arena, &values[1])
        },
        "annotationList CONTROL name typeParameterListOpt `( parameterList `) constructorParameterListOpt `{ controlLocalDeclarationList APPLY controlBody `}" => |values| {
            id_name(arena, &values[1])
        },
        "annotationList ENUM name `{ nameList trailingCommaOpt `}" => |values| {
            id_name(arena, &values[1])
        },
        "annotationList ENUM type name `{ namedExpressionList trailingCommaOpt `}" => |values| {
            id_name(arena, &values[2])
        },
        "annotationList STRUCT name typeParameterListOpt `{ typeFieldList `}" => |values| {
            id_name(arena, &values[1])
        },
        "annotationList HEADER name typeParameterListOpt `{ typeFieldList `}" => |values| {
            id_name(arena, &values[1])
        },
        "annotationList HEADER_UNION name typeParameterListOpt `{ typeFieldList `}" => |values| {
            id_name(arena, &values[1])
        },
        "annotationList TYPEDEF typedef name ';'" => |values| id_name(arena, &values[2]),
        "annotationList TYPE typeRef name ';'" => |values| id_name(arena, &values[2]),
        "annotationList PARSER name typeParameterListOpt `( parameterList `) ';'"
        | "annotationList CONTROL name typeParameterListOpt `( parameterList `) ';'"
        | "annotationList PACKAGE name typeParameterListOpt `( parameterList `) ';'" => |values| {
            id_name(arena, &values[1])
        },
        "annotationList TABLE name `{ tablePropertyList `}" => |values| id_name(arena, &values[1]),
        _ => Err(ExtractError::ValueUnexpected("id_declaration")),
    }
}

// == Type identifier extraction

/// The named type a `typeRef` refers to; built-in types have no members.
pub(super) fn type_id_type_ref(arena: &Arena, value: &Value) -> Result<TypeId, ExtractError> {
    let unexpected = || ExtractError::ValueUnexpected("type_id_type_ref");
    value::get::matches! { arena,
        value,
        // Built-in types have no members to resolve
        "BOOL"
        | "ERROR"
        | "MATCH_KIND"
        | "STRING"
        | "INT"
        | "INT `< int `>"
        | "INT `< `( expression `) `>"
        | "BIT"
        | "BIT `< int `>"
        | "BIT `< `( expression `) `>"
        | "VARBIT `< int `>"
        | "VARBIT `< `( expression `) `>" => |_values| Ok(TypeId::Empty),
        "_TID text" => |values| {
            let text = value::get::text(arena, &values[0]).map_err(|_| unexpected())?;
            Ok(TypeId::Local(text.to_owned()))
        },
        // `.T` is looked up globally
        "_TID '.' typeName" => |values| {
            match type_id_type_ref(arena, &values[0])? {
                TypeId::Local(id) => Ok(TypeId::Global(id)),
                _ => Err(unexpected()),
            }
        },
        // Type arguments do not change which type is named
        "prefixedTypeName `< typeArgumentList `>" => |values| {
            type_id_type_ref(arena, &values[0])
        },
        "namedType `[ expression `]"
        | "LIST `< typeArgument `>"
        | "TUPLE `< typeArgumentList `>" => |_values| Ok(TypeId::Empty),
        _ => Err(unexpected()),
    }
}

/// The declared type of a variable or instance declaration.
pub(super) fn type_id_declaration(arena: &Arena, value: &Value) -> Result<TypeId, ExtractError> {
    value::get::matches! { arena,
        value,
        "annotationList CONST type name initializer ';'"
        | "annotationList type `( argumentList `) name ';'"
        | "annotationList type `( argumentList `) name objectInitializer ';'" => |values| {
            type_id_type_ref(arena, &values[1])
        },
        _ => Err(ExtractError::ValueUnexpected("type_id_declaration")),
    }
}

// == Type parameter extraction

/// Whether a `typeParameterListOpt` is non-empty.
fn has_type_params(arena: &Arena, value: &Value) -> Result<bool, ExtractError> {
    value::get::matches! { arena,
        value,
        "_EMPTY" => |_values| Ok(false),
        "`< typeParameterList `>" => |_values| Ok(true),
        _ => Err(ExtractError::ValueUnexpected("has_type_params")),
    }
}

/// Whether a `functionPrototype` has type parameters.
pub(super) fn has_type_params_function_prototype(
    arena: &Arena,
    value: &Value,
) -> Result<bool, ExtractError> {
    value::get::matches! { arena,
        value,
        "typeOrVoid name typeParameterListOpt `( parameterList `)" => |values| {
            has_type_params(arena, &values[2])
        },
        _ => Err(ExtractError::ValueUnexpected(
            "has_type_params_function_prototype",
        )),
    }
}

/// Whether a declaration introduces type parameters.
pub(super) fn has_type_params_declaration(
    arena: &Arena,
    value: &Value,
) -> Result<bool, ExtractError> {
    value::get::matches! { arena,
        value,
        // One arm per declaration production; only some take type parameters
        "annotationList CONST type name initializer ';'"
        | "annotationList type `( argumentList `) name ';'"
        | "annotationList type `( argumentList `) name objectInitializer ';'" => |_values| {
            Ok(false)
        },
        "annotationList functionPrototype blockStatement" => |values| {
            has_type_params_function_prototype(arena, &values[1])
        },
        "annotationList ACTION name `( parameterList `) blockStatement" => |_values| Ok(false),
        "annotationList EXTERN functionPrototype ';'" => |values| {
            has_type_params_function_prototype(arena, &values[1])
        },
        "annotationList EXTERN nonTypeName typeParameterListOpt `{ externConstructorOrMethodPrototypeList `}"
        | "annotationList PARSER name typeParameterListOpt `( parameterList `) constructorParameterListOpt `{ parserLocalDeclarationList parserStateList `}"
        | "annotationList CONTROL name typeParameterListOpt `( parameterList `) constructorParameterListOpt `{ controlLocalDeclarationList APPLY controlBody `}" => |values| {
            has_type_params(arena, &values[2])
        },
        "annotationList ENUM name `{ nameList trailingCommaOpt `}"
        | "annotationList ENUM type name `{ namedExpressionList trailingCommaOpt `}" => |_values| {
            Ok(false)
        },
        "annotationList STRUCT name typeParameterListOpt `{ typeFieldList `}"
        | "annotationList HEADER name typeParameterListOpt `{ typeFieldList `}"
        | "annotationList HEADER_UNION name typeParameterListOpt `{ typeFieldList `}" => |values| {
            has_type_params(arena, &values[2])
        },
        "annotationList TYPEDEF typedef name ';'"
        | "annotationList TYPE typeRef name ';'" => |_values| Ok(false),
        "annotationList PARSER name typeParameterListOpt `( parameterList `) ';'"
        | "annotationList CONTROL name typeParameterListOpt `( parameterList `) ';'"
        | "annotationList PACKAGE name typeParameterListOpt `( parameterList `) ';'" => |values| {
            has_type_params(arena, &values[2])
        },
        "annotationList TABLE name `{ tablePropertyList `}" => |_values| Ok(false),
        _ => Err(ExtractError::ValueUnexpected(
            "has_type_params_declaration",
        )),
    }
}
