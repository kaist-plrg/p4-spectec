(*
 * Helper functions for context management
 *
 * - id_of : extracts identifiers from CaseV values
 * - has_type_params : checks for type parameters in CaseV values
 *)

open Il.Ast
open Wrap
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
  | "constantDeclaration", [ [ "ConstD" ]; []; []; [] ], [ _; name; _ ] ->
      id_of_name name
  | "instantiation", [ [ "InstD" ]; []; []; []; [] ], [ _; _; name; _ ] ->
      id_of_name name
  | ( "functionDeclaration",
      [ [ "FuncD" ]; []; []; []; []; [] ],
      [ _; name; _; _; _ ] ) ->
      id_of_name name
  | "actionDeclaration", [ [ "ActionD" ]; []; []; [] ], [ name; _; _ ] ->
      id_of_name name
  | "errorDeclaration", _, _ -> failwith "errorDeclaration: no name"
  | "matchKindDeclaration", _, _ -> failwith "matchKindDeclaration: no name"
  | ( "externFunctionDeclaration",
      [ [ "ExternFuncD" ]; []; []; []; [] ],
      [ _; name; _; _ ] ) ->
      id_of_name name
  | ( "externObjectDeclaration",
      [ [ "ExternObjectD" ]; []; []; [] ],
      [ name; _; _ ] ) ->
      id_of_name name
  | ( "parserDeclaration",
      [ [ "ParserD" ]; []; []; []; []; []; [] ],
      [ name; _; _; _; _; _ ] )
  | ( "controlDeclaration",
      [ [ "ControlD" ]; []; []; []; []; []; [] ],
      [ name; _; _; _; _; _ ] )
  | "enumTypeDeclaration", [ [ "EnumD" ]; []; [] ], [ name; _ ]
  | "enumTypeDeclaration", [ [ "SEnumD" ]; []; []; [] ], [ _; name; _ ]
  | "structTypeDeclaration", [ [ "StructD" ]; []; []; [] ], [ name; _; _ ]
  | "headerTypeDeclaration", [ [ "HeaderD" ]; []; []; [] ], [ name; _; _ ]
  | ( "headerUnionTypeDeclaration",
      [ [ "HeaderUnionD" ]; []; []; [] ],
      [ name; _; _ ] )
  | "typedefDeclaration", [ [ "TypeDefD" ]; []; [] ], [ _; name ]
  | "typedefDeclaration", [ [ "NewTypeD" ]; []; [] ], [ _; name ]
  | "parserTypeDeclaration", [ [ "ParserTypeD" ]; []; []; [] ], [ name; _; _ ]
  | "controlTypeDeclaration", [ [ "ControlTypeD" ]; []; []; [] ], [ name; _; _ ]
  | "packageTypeDeclaration", [ [ "PackageTypeD" ]; []; []; [] ], [ name; _; _ ]
    ->
      id_of_name name
  (* not a variant of declaration *)
  | "tableDeclaration", [ [ "TableD" ]; []; [] ], [ name; _ ] -> id_of_name name
  | _ ->
      failwith
        (Printf.sprintf "@id_of_declaration: %s"
           (Il.Print.string_of_value decl))

let id_of_parameter (v : value) : string =
  match flatten_case_v v with
  | "parameter", [ []; []; []; []; [] ], [ _; _; name; _ ] -> id_of_name name
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
  | "constantDeclaration", _, _ | "instantiation", _, _ -> false
  | ( "functionDeclaration",
      [ [ "FuncD" ]; []; []; []; []; [] ],
      [ _; _; tpl; _; _ ] ) ->
      has_type_params tpl
  | "actionDeclaration", _, _
  | "errorDeclaration", _, _
  | "matchKindDeclaration", _, _ ->
      false
  | ( "externFunctionDeclaration",
      [ [ "ExternFuncD" ]; []; []; []; [] ],
      [ _; _; tpl; _ ] ) ->
      has_type_params tpl
  | ( "externObjectDeclaration",
      [ [ "ExternObjectD" ]; []; []; [] ],
      [ _; tpl; _ ] ) ->
      has_type_params tpl
  | "parserDeclaration", _, _
  | "controlDeclaration", _, _
  | "enumTypeDeclaration", _, _ ->
      false
  | "structTypeDeclaration", [ [ "StructD" ]; []; []; [] ], [ _; tpl; _ ]
  | "headerTypeDeclaration", [ [ "HeaderD" ]; []; []; [] ], [ _; tpl; _ ]
  | ( "headerUnionTypeDeclaration",
      [ [ "HeaderUnionD" ]; []; []; [] ],
      [ _; tpl; _ ] ) ->
      has_type_params tpl
  | "typedefDeclaration", _, _ -> false
  | "parserTypeDeclaration", [ [ "ParserTypeD" ]; []; []; [] ], [ _; tpl; _ ]
  | "controlTypeDeclaration", [ [ "ControlTypeD" ]; []; []; [] ], [ _; tpl; _ ]
  | "packageTypeDeclaration", [ [ "PackageTypeD" ]; []; []; [] ], [ _; tpl; _ ]
    ->
      has_type_params tpl
  | _ ->
      failwith
        (Printf.sprintf "@has_typ_params: Unknown declaration %s"
           (type_of_case_v decl))
