(*
 * Helper functions for context management
 *
 * - id_of : extracts identifiers from CaseV values
 * - has_type_params : checks for type parameters in CaseV values
 *)

open Lang
open Il
open Flatten
open Util.Error
open Util.Source
module F = Format

let error = error_parse

(* Identifier extraction *)

let id_of_name (value : value) : string =
  match flatten_case_v_opt value with
  | Some ("identifier", [ [ "`ID" ]; [] ], [ { it = TextV s; _ } ]) -> s
  | Some ("nonTypeName", [ [ "APPLY" ] ], []) -> "apply"
  | Some ("nonTypeName", [ [ "KEY" ] ], []) -> "key"
  | Some ("nonTypeName", [ [ "ACTIONS" ] ], []) -> "actions"
  | Some ("nonTypeName", [ [ "STATE" ] ], []) -> "state"
  | Some ("nonTypeName", [ [ "ENTRIES" ] ], []) -> "entries"
  | Some ("nonTypeName", [ [ "TYPE" ] ], []) -> "type"
  | Some ("nonTypeName", [ [ "PRIORITY" ] ], []) -> "priority"
  | Some ("name", [ [ "LIST" ] ], []) -> "list"
  | Some ("typeIdentifier", [ [ "`TID" ]; [] ], [ { it = TextV s; _ } ]) -> s
  | _ ->
      error no_region
        (F.asprintf "@id_of_name: expected TextV, got %s"
           (Il.Print.string_of_value value))

let id_of_function_prototype (value : value) : string =
  match flatten_case_v_opt value with
  | Some
      ("functionPrototype", [ []; []; []; [ "(" ]; [ ")" ] ], [ _; name; _; _ ])
    ->
      id_of_name name
  | _ ->
      error no_region
        (F.asprintf
           "@id_of_function_prototype: expected functionPrototype, got %s"
           (Il.Print.string_of_value value))

let id_of_declaration (value : value) : string =
  match flatten_case_v_opt value with
  | Some
      ( "constantDeclaration",
        [ []; [ "CONST" ]; []; []; [ ";" ] ],
        [ _; _; name; _ ] ) ->
      id_of_name name
  | Some
      ("instantiation", [ []; []; [ "(" ]; [ ")" ]; [ ";" ] ], [ _; _; _; name ])
    ->
      id_of_name name
  | Some
      ( "instantiation",
        [ []; []; [ "(" ]; [ ")" ]; []; [ ";" ] ],
        [ _; _; _; name; _ ] ) ->
      id_of_name name
  | Some ("functionDeclaration", [ []; []; []; [] ], [ _; functionPrototype; _ ])
    ->
      id_of_function_prototype functionPrototype
  | Some
      ( "actionDeclaration",
        [ []; [ "ACTION" ]; [ "(" ]; [ ")" ]; [] ],
        [ _; name; _; _ ] ) ->
      id_of_name name
  | Some ("errorDeclaration", _, _) ->
      error no_region "errorDeclaration: no name"
  | Some ("matchKindDeclaration", _, _) ->
      error no_region "matchKindDeclaration: no name"
  | Some
      ( "externFunctionDeclaration",
        [ []; [ "EXTERN" ]; [ ";" ] ],
        [ _; functionPrototype ] ) ->
      id_of_function_prototype functionPrototype
  | Some
      ( "externObjectDeclaration",
        [ []; [ "EXTERN" ]; []; [ "{" ]; [ "}" ] ],
        [ _; nonTypeName; _; _ ] ) ->
      id_of_name nonTypeName
  | Some
      ( "parserDeclaration",
        [ []; [ "PARSER" ]; []; [ "(" ]; [ ")" ]; [ "{" ]; []; [ "}" ] ],
        [ _; name; _; _; _; _; _ ] )
  | Some
      ( "controlDeclaration",
        [
          []; [ "CONTROL" ]; []; [ "(" ]; [ ")" ]; [ "{" ]; [ "APPLY" ]; [ "}" ];
        ],
        [ _; name; _; _; _; _; _ ] )
  | Some
      ( "enumTypeDeclaration",
        [ []; [ "ENUM" ]; [ "{" ]; []; [ "}" ] ],
        [ _; name; _; _ ] )
  | Some
      ( "enumTypeDeclaration",
        [ []; [ "ENUM" ]; []; [ "{" ]; []; [ "}" ] ],
        [ _; _; name; _; _ ] )
  | Some
      ( "structTypeDeclaration",
        [ []; [ "STRUCT" ]; []; [ "{" ]; [ "}" ] ],
        [ _; name; _; _ ] )
  | Some
      ( "headerTypeDeclaration",
        [ []; [ "HEADER" ]; []; [ "{" ]; [ "}" ] ],
        [ _; name; _; _ ] )
  | Some
      ( "headerUnionTypeDeclaration",
        [ []; [ "HEADER_UNION" ]; []; [ "{" ]; [ "}" ] ],
        [ _; name; _; _ ] )
  | Some
      ("typedefDeclaration", [ []; [ "TYPEDEF" ]; []; [ ";" ] ], [ _; _; name ])
  | Some ("typedefDeclaration", [ []; [ "TYPE" ]; []; [ ";" ] ], [ _; _; name ])
  | Some
      ( "parserTypeDeclaration",
        [ []; [ "PARSER" ]; []; [ "(" ]; [ ")"; ";" ] ],
        [ _; name; _; _ ] )
  | Some
      ( "controlTypeDeclaration",
        [ []; [ "CONTROL" ]; []; [ "(" ]; [ ")"; ";" ] ],
        [ _; name; _; _ ] )
  | Some
      ( "packageTypeDeclaration",
        [ []; [ "PACKAGE" ]; []; [ "(" ]; [ ")"; ";" ] ],
        [ _; name; _; _ ] ) ->
      id_of_name name
  (* not a variant of declaration *)
  | Some
      ("tableDeclaration", [ []; [ "TABLE" ]; [ "{" ]; [ "}" ] ], [ _; name; _ ])
    ->
      id_of_name name
  | _ ->
      error no_region
        (F.asprintf "@id_of_declaration: expected declaration, got %s"
           (Il.Print.string_of_value value))

let id_of_parameter (value : value) : string =
  match flatten_case_v_opt value with
  | Some ("parameter", [ []; []; []; []; [] ], [ _; _; _; name ]) ->
      id_of_name name
  | Some ("parameter", [ []; []; []; []; []; [] ], [ _; _; _; name; _ ]) ->
      id_of_name name
  | _ ->
      error no_region
        (F.asprintf "@id_of_parameter: expected parameter, got %s"
           (Il.Print.string_of_value value))

(* Type parameter extraction *)

let has_type_params (value : value) : bool =
  match flatten_case_v_opt value with
  | Some ("typeParameterListOpt", [ [ "`EMPTY" ] ], []) -> false
  | Some ("typeParameterListOpt", [ [ "<" ]; [ ">" ] ], [ _ ]) -> true
  | _ ->
      error no_region
        (Printf.sprintf
           "@has_type_params: expected typeParameterListOpt, got %s"
           (Il.Print.string_of_value value))

let has_type_params_function_prototype (value : value) : bool =
  match flatten_case_v_opt value with
  | Some
      ( "functionPrototype",
        [ []; []; []; [ "(" ]; [ ")" ] ],
        [ _; _; typeParameterListOpt; _ ] ) ->
      has_type_params typeParameterListOpt
  | _ ->
      error no_region
        (Printf.sprintf
           "@has_type_params_function_prototype: expected functionPrototype, \
            got %s"
           (Il.Print.string_of_value value))

let has_type_params_declaration (value : value) : bool =
  match flatten_case_v_opt value with
  | Some ("constantDeclaration", _, _) | Some ("instantiation", _, _) -> false
  | Some ("functionDeclaration", [ []; []; []; [] ], [ _; functionPrototype; _ ])
    ->
      has_type_params_function_prototype functionPrototype
  | Some ("actionDeclaration", _, _)
  | Some ("errorDeclaration", _, _)
  | Some ("matchKindDeclaration", _, _) ->
      false
  | Some
      ( "externFunctionDeclaration",
        [ []; [ "EXTERN" ]; [ ";" ] ],
        [ _; functionPrototype ] ) ->
      has_type_params_function_prototype functionPrototype
  | Some
      ( "externObjectDeclaration",
        [ []; [ "EXTERN" ]; []; [ "{" ]; [ "}" ] ],
        [ _; _; typeParameterListOpt; _ ] ) ->
      has_type_params typeParameterListOpt
  | Some ("parserDeclaration", _, _)
  | Some ("controlDeclaration", _, _)
  | Some ("enumTypeDeclaration", _, _) ->
      false
  | Some
      ( "structTypeDeclaration",
        [ []; [ "STRUCT" ]; []; [ "{" ]; [ "}" ] ],
        [ _; _; typeParameterListOpt; _ ] )
  | Some
      ( "headerTypeDeclaration",
        [ []; [ "HEADER" ]; []; [ "{" ]; [ "}" ] ],
        [ _; _; typeParameterListOpt; _ ] )
  | Some
      ( "headerUnionTypeDeclaration",
        [ []; [ "HEADER_UNION" ]; []; [ "{" ]; [ "}" ] ],
        [ _; _; typeParameterListOpt; _ ] ) ->
      has_type_params typeParameterListOpt
  | Some ("typedefDeclaration", _, _) -> false
  | Some
      ( "parserTypeDeclaration",
        [ []; [ "PARSER" ]; []; [ "(" ]; [ ")"; ";" ] ],
        [ _; _; typeParameterListOpt; _ ] )
  | Some
      ( "controlTypeDeclaration",
        [ []; [ "CONTROL" ]; []; [ "(" ]; [ ")"; ";" ] ],
        [ _; _; typeParameterListOpt; _ ] )
  | Some
      ( "packageTypeDeclaration",
        [ []; [ "PACKAGE" ]; []; [ "(" ]; [ ")"; ";" ] ],
        [ _; _; typeParameterListOpt; _ ] ) ->
      has_type_params typeParameterListOpt
  | _ ->
      error no_region
        (Printf.sprintf
           "@has_type_params_declaration: expected declaration, got %s"
           (Il.Print.string_of_value value))
