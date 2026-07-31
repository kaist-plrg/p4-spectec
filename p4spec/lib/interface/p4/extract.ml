(*
 * Helper functions for context management
 *
 * - id_of : extracts identifiers from CaseV values
 * - has_type_params : checks for type parameters in CaseV values
 *)

open Lang
open Il
module Value = Runtime.Value
open Util.Error
open Util.Source
module F = Format

let error = error_parse

(* Identifier extraction *)

let id_of_name (value : value) : string =
  Value.Get.mtch value
    [
      ("_ID text", fun values -> values |> Value.Get.nth 0 |> Value.Get.text);
      ("APPLY", fun _ -> "apply");
      ("KEY", fun _ -> "key");
      ("ACTIONS", fun _ -> "actions");
      ("STATE", fun _ -> "state");
      ("ENTRIES", fun _ -> "entries");
      ("TYPE", fun _ -> "type");
      ("PRIORITY", fun _ -> "priority");
      ("_TID text", fun values -> values |> Value.Get.nth 0 |> Value.Get.text);
      ("LIST", fun _ -> "list");
    ]
    (fun _ -> error no_region "@id_of_name: unexpected value")

let id_of_function_prototype (value : value) : string =
  Value.Get.mtch value
    [
      ( "typeOrVoid name typeParameterListOpt `( parameterList `)",
        fun values -> values |> Value.Get.nth 1 |> id_of_name );
    ]
    (fun _ -> error no_region "@id_of_function_prototype: unexpected value")

let id_of_declaration (value : value) : string =
  Value.Get.mtch value
    [
      ( "annotationList CONST type name initializer ';'",
        fun values -> values |> Value.Get.nth 2 |> id_of_name );
      ( "annotationList type `( argumentList `) name ';'",
        fun values -> values |> Value.Get.nth 3 |> id_of_name );
      ( "annotationList type `( argumentList `) name objectInitializer ';'",
        fun values -> values |> Value.Get.nth 3 |> id_of_name );
      ( "annotationList functionPrototype blockStatement",
        fun values -> values |> Value.Get.nth 1 |> id_of_function_prototype );
      ( "annotationList ACTION name `( parameterList `) blockStatement",
        fun values -> values |> Value.Get.nth 1 |> id_of_name );
      ( "annotationList EXTERN functionPrototype ';'",
        fun values -> values |> Value.Get.nth 1 |> id_of_function_prototype );
      ( "annotationList EXTERN nonTypeName typeParameterListOpt `{ \
         externConstructorOrMethodPrototypeList `}",
        fun values -> values |> Value.Get.nth 1 |> id_of_name );
      ( "annotationList PARSER name typeParameterListOpt `( parameterList `) \
         constructorParameterListOpt `{ parserLocalDeclarationList \
         parserStateList `}",
        fun values -> values |> Value.Get.nth 1 |> id_of_name );
      ( "annotationList CONTROL name typeParameterListOpt `( parameterList `) \
         constructorParameterListOpt `{ controlLocalDeclarationList APPLY \
         controlBody `}",
        fun values -> values |> Value.Get.nth 1 |> id_of_name );
      ( "annotationList ENUM name `{ nameList trailingCommaOpt `}",
        fun values -> values |> Value.Get.nth 1 |> id_of_name );
      ( "annotationList ENUM type name `{ namedExpressionList trailingCommaOpt \
         `}",
        fun values -> values |> Value.Get.nth 2 |> id_of_name );
      ( "annotationList STRUCT name typeParameterListOpt `{ typeFieldList `}",
        fun values -> values |> Value.Get.nth 1 |> id_of_name );
      ( "annotationList HEADER name typeParameterListOpt `{ typeFieldList `}",
        fun values -> values |> Value.Get.nth 1 |> id_of_name );
      ( "annotationList HEADER_UNION name typeParameterListOpt `{ \
         typeFieldList `}",
        fun values -> values |> Value.Get.nth 1 |> id_of_name );
      ( "annotationList TYPEDEF typedef name ';'",
        fun values -> values |> Value.Get.nth 2 |> id_of_name );
      ( "annotationList TYPE type name ';'",
        fun values -> values |> Value.Get.nth 2 |> id_of_name );
      ( "annotationList PARSER name typeParameterListOpt `( parameterList `) ';'",
        fun values -> values |> Value.Get.nth 1 |> id_of_name );
      ( "annotationList CONTROL name typeParameterListOpt `( parameterList `) \
         ';'",
        fun values -> values |> Value.Get.nth 1 |> id_of_name );
      ( "annotationList PACKAGE name typeParameterListOpt `( parameterList `) \
         ';'",
        fun values -> values |> Value.Get.nth 1 |> id_of_name );
      ( "annotationList TABLE name `{ tablePropertyList `}",
        fun values -> values |> Value.Get.nth 1 |> id_of_name );
    ]
    (fun _ -> error no_region "@id_of_declaration: unexpected value")

let id_of_parameter (value : value) : string =
  Value.Get.mtch value
    [
      ( "annotationList direction type name initializerOpt",
        fun values -> values |> Value.Get.nth 3 |> id_of_name );
    ]
    (fun _ -> error no_region "@id_of_parameter: unexpected value")

(* Type identifier extraction *)

let rec tid_of_typeRef (value : value) : Context.tid =
  Value.Get.mtch value
    [
      ("BOOL", fun _ -> Context.Empty);
      ("ERROR", fun _ -> Context.Empty);
      ("MATCH_KIND", fun _ -> Context.Empty);
      ("STRING", fun _ -> Context.Empty);
      ("INT", fun _ -> Context.Empty);
      ("INT `< int `>", fun _ -> Context.Empty);
      ("INT `< `( expression `) `>", fun _ -> Context.Empty);
      ("BIT", fun _ -> Context.Empty);
      ("BIT `< int `>", fun _ -> Context.Empty);
      ("BIT `< `( expression `) `>", fun _ -> Context.Empty);
      ("VARBIT `< int `>", fun _ -> Context.Empty);
      ("VARBIT `< `( expression `) `>", fun _ -> Context.Empty);
      ( "_TID text",
        fun values ->
          let s = values |> Value.Get.nth 0 |> Value.Get.text in
          Context.Local s );
      ( "_TID '.' typeName",
        fun values ->
          let value = values |> Value.Get.nth 0 in
          match tid_of_typeRef value with
          | Context.Local s -> Context.Global s
          | _ -> error no_region "@tid_of_typeRef: unreachable" );
      ( "prefixedTypeName `< typeArgumentList `>",
        fun values ->
          let prefixedTypeName = values |> Value.Get.nth 0 in
          tid_of_typeRef prefixedTypeName );
      ("namedType `[ expression `]", fun _ -> Context.Empty);
      ("LIST `< typeArgument `>", fun _ -> Context.Empty);
      ("TUPLE `< typeArgumentList `>", fun _ -> Context.Empty);
    ]
    (fun _ -> error no_region "@tid_of_typeRef: unexpected value")

let tid_of_declaration (value : value) : Context.tid =
  Value.Get.mtch value
    [
      ( "annotationList CONST type name initializer ';'",
        fun values -> values |> Value.Get.nth 1 |> tid_of_typeRef );
      ( "annotationList type `( argumentList `) name ';'",
        fun values -> values |> Value.Get.nth 1 |> tid_of_typeRef );
      ( "annotationList type `( argumentList `) name objectInitializer ';'",
        fun values -> values |> Value.Get.nth 1 |> tid_of_typeRef );
    ]
    (fun _ -> error no_region "@tid_of_declaration: unexpected value")

(* Type parameter extraction *)

let has_type_params (value : value) : bool =
  Value.Get.mtch value
    [ ("_EMPTY", fun _ -> false); ("`< typeParameterList `>", fun _ -> true) ]
    (fun _ -> error no_region "@has_type_params: unexpected value")

let has_type_params_function_prototype (value : value) : bool =
  Value.Get.mtch value
    [
      ( "typeOrVoid name typeParameterListOpt `( parameterList `)",
        fun values -> values |> Value.Get.nth 2 |> has_type_params );
    ]
    (fun _ ->
      error no_region "@has_type_params_function_prototype: unexpected value")

let has_type_params_declaration (value : value) : bool =
  Value.Get.mtch value
    [
      ("annotationList CONST type name initializer ';'", fun _ -> false);
      ("annotationList type `( argumentList `) name ';'", fun _ -> false);
      ( "annotationList type `( argumentList `) name objectInitializer ';'",
        fun _ -> false );
      ( "annotationList functionPrototype blockStatement",
        fun values ->
          values |> Value.Get.nth 1 |> has_type_params_function_prototype );
      ( "annotationList ACTION name `( parameterList `) blockStatement",
        fun _ -> false );
      ( "annotationList EXTERN functionPrototype ';'",
        fun values ->
          values |> Value.Get.nth 1 |> has_type_params_function_prototype );
      ( "annotationList EXTERN nonTypeName typeParameterListOpt `{ \
         externConstructorOrMethodPrototypeList `}",
        fun values -> values |> Value.Get.nth 2 |> has_type_params );
      ( "annotationList PARSER name typeParameterListOpt `( parameterList `) \
         constructorParameterListOpt `{ parserLocalDeclarationList \
         parserStateList `}",
        fun values -> values |> Value.Get.nth 2 |> has_type_params );
      ( "annotationList CONTROL name typeParameterListOpt `( parameterList `) \
         constructorParameterListOpt `{ controlLocalDeclarationList APPLY \
         controlBody `}",
        fun values -> values |> Value.Get.nth 2 |> has_type_params );
      ( "annotationList ENUM name `{ nameList trailingCommaOpt `}",
        fun _ -> false );
      ( "annotationList ENUM type name `{ namedExpressionList trailingCommaOpt \
         `}",
        fun _ -> false );
      ( "annotationList STRUCT name typeParameterListOpt `{ typeFieldList `}",
        fun values -> values |> Value.Get.nth 2 |> has_type_params );
      ( "annotationList HEADER name typeParameterListOpt `{ typeFieldList `}",
        fun values -> values |> Value.Get.nth 2 |> has_type_params );
      ( "annotationList HEADER_UNION name typeParameterListOpt `{ \
         typeFieldList `}",
        fun values -> values |> Value.Get.nth 2 |> has_type_params );
      ("annotationList TYPEDEF typedef name ';'", fun _ -> false);
      ("annotationList TYPE type name ';'", fun _ -> false);
      ( "annotationList PARSER name typeParameterListOpt `( parameterList `) ';'",
        fun values -> values |> Value.Get.nth 2 |> has_type_params );
      ( "annotationList CONTROL name typeParameterListOpt `( parameterList `) \
         ';'",
        fun values -> values |> Value.Get.nth 2 |> has_type_params );
      ( "annotationList PACKAGE name typeParameterListOpt `( parameterList `) \
         ';'",
        fun values -> values |> Value.Get.nth 2 |> has_type_params );
      ("annotationList TABLE name `{ tablePropertyList `}", fun _ -> false);
    ]
    (fun _ -> error no_region "@has_type_params_declaration: unexpected value")
