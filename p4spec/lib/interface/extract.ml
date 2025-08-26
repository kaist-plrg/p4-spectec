(*
 * Helper functions for context management
 *
 * - id_of : extracts identifiers from CaseV values
 * - has_type_params : checks for type parameters in CaseV values
 *)

open Il.Ast
open Flatten
module F = Format

(* Identifier extraction *)

let id_of_name (v : value) : string =
  match v.it with
  | TextV s -> s
  | _ ->
      failwith
        (Printf.sprintf "@id_of_name: expected TextV, got %s"
           (Il.Print.string_of_value v))

let id_of_declaration (decl : value) : string =
  match flatten_case_v decl with
  | [ [ "ConstD" ]; []; []; [] ], [ _; name; _ ]
  | [ [ "InstD" ]; []; []; []; [] ], [ _; _; name; _ ]
  | [ [ "FuncD" ]; []; []; []; []; [] ], [ _; name; _; _; _ ]
  | [ [ "ActionD" ]; []; []; [] ], [ name; _; _ ]
  | [ [ "ExternFuncD" ]; []; []; []; [] ], [ _; name; _; _ ]
  | [ [ "ExternObjectD" ]; []; []; [] ], [ name; _; _ ]
  | [ [ "ParserD" ]; []; []; []; []; []; [] ], [ name; _; _; _; _; _ ]
  | [ [ "ControlD" ]; []; []; []; []; []; [] ], [ name; _; _; _; _; _ ]
  | [ [ "EnumD" ]; []; [] ], [ name; _ ]
  | [ [ "SEnumD" ]; []; []; [] ], [ _; name; _ ]
  | [ [ "StructD" ]; []; []; [] ], [ name; _; _ ]
  | [ [ "HeaderD" ]; []; []; [] ], [ name; _; _ ]
  | [ [ "HeaderUnionD" ]; []; []; [] ], [ name; _; _ ]
  | [ [ "TypeDefD" ]; []; [] ], [ _; name ]
  | [ [ "NewTypeD" ]; []; [] ], [ _; name ]
  | [ [ "ParserTypeD" ]; []; []; [] ], [ name; _; _ ]
  | [ [ "ControlTypeD" ]; []; []; [] ], [ name; _; _ ]
  | [ [ "PackageTypeD" ]; []; []; [] ], [ name; _; _ ] ->
      id_of_name name
  (* not a variant of declaration *)
  | [ [ "TableD" ]; []; [] ], [ name; _ ] -> id_of_name name
  | _ ->
      failwith
        (Printf.sprintf "@id_of_declaration: %s"
           (Il.Print.string_of_value decl))

let id_of_parameter (v : value) : string =
  match flatten_case_v v with
  | [ []; []; []; []; [] ], [ _; _; name; _ ] -> id_of_name name
  | _ ->
      failwith
        (Printf.sprintf "@id_of_parameter: %s" (Il.Print.string_of_value v))

(* Type parameter extraction *)

let has_type_params (v : value) : bool =
  match v.it with
  | ListV [] -> false
  | ListV (_ :: _) -> true
  | _ ->
      failwith
        (Printf.sprintf "@has_type_params: expected ListV, got %s"
           (Il.Print.string_of_value v))

let has_type_params_declaration (decl : value) : bool =
  match flatten_case_v decl with
  | [ [ "FuncD" ]; []; []; []; []; [] ], [ _; _; tpl; _; _ ]
  | [ [ "ExternFuncD" ]; []; []; []; [] ], [ _; _; tpl; _ ]
  | [ [ "ExternObjectD" ]; []; []; [] ], [ _; tpl; _ ]
  | [ [ "StructD" ]; []; []; [] ], [ _; tpl; _ ]
  | [ [ "HeaderD" ]; []; []; [] ], [ _; tpl; _ ]
  | [ [ "HeaderUnionD" ]; []; []; [] ], [ _; tpl; _ ]
  | [ [ "ParserTypeD" ]; []; []; [] ], [ _; tpl; _ ]
  | [ [ "ControlTypeD" ]; []; []; [] ], [ _; tpl; _ ]
  | [ [ "PackageTypeD" ]; []; []; [] ], [ _; tpl; _ ] ->
      has_type_params tpl
  | _ -> false
