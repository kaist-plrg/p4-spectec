(*
 * Helper functions for context management
 *
 * - id_of : extracts identifiers from CaseV values
 * - has_type_params : checks for type parameters in CaseV values
 *)

open Il.Ast
open Flatten
open Util.Error
open Util.Source
module F = Format

let error = error_parse

(* Identifier extraction *)

let id_of_name (v : value) : string =
  match v.it with
  | TextV s -> s
  | _ ->
      error no_region
        (F.asprintf "@id_of_name: expected TextV, got %s"
           (Il.Print.string_of_value v))

let id_of_declaration (decl : value) : string =
  match flatten_case_v_opt decl with
  | Some ([ [ "ConstD" ]; []; []; [] ], [ _; name; _ ])
  | Some ([ [ "InstD" ]; []; []; []; [] ], [ _; _; name; _ ])
  | Some ([ [ "FuncD" ]; []; []; []; []; [] ], [ _; name; _; _; _ ])
  | Some ([ [ "ActionD" ]; []; []; [] ], [ name; _; _ ])
  | Some ([ [ "ExternFuncD" ]; []; []; []; [] ], [ _; name; _; _ ])
  | Some ([ [ "ExternObjectD" ]; []; []; [] ], [ name; _; _ ])
  | Some ([ [ "ParserD" ]; []; []; []; []; []; [] ], [ name; _; _; _; _; _ ])
  | Some ([ [ "ControlD" ]; []; []; []; []; []; [] ], [ name; _; _; _; _; _ ])
  | Some ([ [ "EnumD" ]; []; [] ], [ name; _ ])
  | Some ([ [ "SEnumD" ]; []; []; [] ], [ _; name; _ ])
  | Some ([ [ "StructD" ]; []; []; [] ], [ name; _; _ ])
  | Some ([ [ "HeaderD" ]; []; []; [] ], [ name; _; _ ])
  | Some ([ [ "HeaderUnionD" ]; []; []; [] ], [ name; _; _ ])
  | Some ([ [ "TypeDefD" ]; []; [] ], [ _; name ])
  | Some ([ [ "NewTypeD" ]; []; [] ], [ _; name ])
  | Some ([ [ "ParserTypeD" ]; []; []; [] ], [ name; _; _ ])
  | Some ([ [ "ControlTypeD" ]; []; []; [] ], [ name; _; _ ])
  | Some ([ [ "PackageTypeD" ]; []; []; [] ], [ name; _; _ ]) ->
      id_of_name name
  (* not a variant of declaration *)
  | Some ([ [ "TableD" ]; []; [] ], [ name; _ ]) -> id_of_name name
  | _ ->
      error no_region
        (F.asprintf "@id_of_declaration: %s" (Il.Print.string_of_value decl))

let id_of_parameter (v : value) : string =
  match flatten_case_v_opt v with
  | Some ([ []; []; []; []; [] ], [ _; _; name; _ ]) -> id_of_name name
  | _ ->
      error no_region
        (F.asprintf "@id_of_parameter: %s" (Il.Print.string_of_value v))

(* Type parameter extraction *)

let has_type_params (v : value) : bool =
  match v.it with
  | ListV [] -> false
  | ListV (_ :: _) -> true
  | _ ->
      error no_region
        (F.asprintf "@has_type_params: expected ListV, got %s"
           (Il.Print.string_of_value v))

let has_type_params_declaration (decl : value) : bool =
  match flatten_case_v_opt decl with
  | Some ([ [ "FuncD" ]; []; []; []; []; [] ], [ _; _; tpl; _; _ ])
  | Some ([ [ "ExternFuncD" ]; []; []; []; [] ], [ _; _; tpl; _ ])
  | Some ([ [ "ExternObjectD" ]; []; []; [] ], [ _; tpl; _ ])
  | Some ([ [ "StructD" ]; []; []; [] ], [ _; tpl; _ ])
  | Some ([ [ "HeaderD" ]; []; []; [] ], [ _; tpl; _ ])
  | Some ([ [ "HeaderUnionD" ]; []; []; [] ], [ _; tpl; _ ])
  | Some ([ [ "ParserTypeD" ]; []; []; [] ], [ _; tpl; _ ])
  | Some ([ [ "ControlTypeD" ]; []; []; [] ], [ _; tpl; _ ])
  | Some ([ [ "PackageTypeD" ]; []; []; [] ], [ _; tpl; _ ]) ->
      has_type_params tpl
  | _ -> false
