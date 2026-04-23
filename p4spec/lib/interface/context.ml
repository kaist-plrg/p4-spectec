(* Copyright 2018-present Cornell University
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy
 * of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations
 * under the License.
 *)

module SMap = Map.Make (String)

type has_params = bool
type tid = Empty | Local of string | Global of string

type ident_kind =
  | TypeName of has_params * namespace
  | Ident of has_params * tid

and namespace = ident_kind SMap.t

type t = namespace list

(* Current context, stored as a mutable global variable *)
let context : t ref = ref [ SMap.empty ]
let backup : t ref = ref []

(* Previously looked-up identifier *)
let previous_id : string option ref = ref None

(* Namespace of a member's parent *)
let parent_namespace : namespace option ref = ref None

(* Resets context *)
let reset () =
  context := [ SMap.empty ];
  backup := [];
  previous_id := None;
  parent_namespace := None

(* Associates [id] with [k] in map for current scope *)
let declare (id : string) (k : ident_kind) : unit =
  match !context with
  | [] -> failwith "ill-formed context"
  | m :: l ->
      Debug_config.context_debug_print ">>> Declaring %s as %s\n" id
        (match k with TypeName _ -> "TypeName" | Ident _ -> "Ident");
      context := SMap.add id k m :: l

let declare_type id has_params = declare id (TypeName (has_params, SMap.empty))
let declare_types types = List.iter (fun s -> declare_type s false) types

let declare_var ?(tid = Empty) id has_params =
  declare id (Ident (has_params, tid))

let declare_vars vars = List.iter (fun s -> declare_var s false) vars

let find_opt (id : string) (ctx : t) : ident_kind option =
  let rec loop = function
    | [] -> None
    | m :: rest -> (
        match SMap.find_opt id m with None -> loop rest | Some k -> Some k)
  in
  loop ctx

let find_type_opt (id : string) (ctx : t) : (has_params * namespace) option =
  let rec loop = function
    | [] -> None
    | m :: rest -> (
        match SMap.find_opt id m with
        | Some (TypeName (has_params, namespace)) -> Some (has_params, namespace)
        | _ -> loop rest)
  in
  loop ctx

let find_var_opt (id : string) (ctx : t) : (has_params * tid) option =
  let rec loop = function
    | [] -> None
    | m :: rest -> (
        match SMap.find_opt id m with
        | Some (Ident (has_params, tid)) -> Some (has_params, tid)
        | _ -> loop rest)
  in
  loop ctx

(* Tests whether [id] is known as a type name. *)
let get_kind (id : string) : ident_kind =
  let ctx =
    match !parent_namespace with None -> !context | Some ns -> [ ns ]
  in
  let kind =
    match find_opt id ctx with None -> Ident (false, Empty) | Some k -> k
  in
  previous_id := Some id;
  kind

let is_typename (id : string) : bool =
  match get_kind id with TypeName _ -> true | _ -> false

(* Takes a snapshot of the current context. *)
let push_scope () =
  Debug_config.context_debug_print "[[ Pushing scope\n";
  context := SMap.empty :: !context

(* Remove scope *)
let pop_scope () =
  Debug_config.context_debug_print "]] Popping scope\n";
  match !context with
  | [] -> failwith "ill-formed context"
  | [ _ ] -> failwith "pop would produce ill-formed context"
  | s :: l ->
      context := l;
      s

let go_toplevel () =
  let rec loop c =
    match c with
    | [] -> failwith "ill-formed context"
    | [ _ ] -> context := c
    | _ :: l -> loop l
  in
  backup := !context;
  loop !context

let go_local () = context := !backup

let get_global_context () =
  let rec loop c =
    match c with
    | [] -> failwith "ill-formed context"
    | [ _ ] -> c
    | _ :: l -> loop l
  in
  loop !context

let set_type_namespace (tid : string) (ns : namespace) =
  let rec loop = function
    | [] -> []
    | m :: rest -> (
        match SMap.find_opt tid m with
        | Some (TypeName (has_params, _)) ->
            SMap.add tid (TypeName (has_params, ns)) m :: rest
        | _ -> m :: loop rest)
  in
  context := loop !context

let set_parent_namespace () =
  let ( let* ) = Option.bind in
  let namespace =
    let* parent_id = !previous_id in
    let* _, tid = find_var_opt parent_id !context in
    let* tid, ctx =
      match tid with
      | Empty -> None
      | Local tid -> Some (tid, !context)
      | Global tid -> Some (tid, get_global_context ())
    in
    Option.map snd (find_type_opt tid ctx)
  in
  let namespace = Option.value namespace ~default:SMap.empty in
  parent_namespace := Some namespace

let clear_parent_namespace () = parent_namespace := None

(* Printing functions for debugging *)
let print_entry x k =
  match k with
  | TypeName (true, _) -> Printf.printf "%s : type<...>" x
  | TypeName (false, _) -> Printf.printf "%s : type" x
  | Ident (true, _) -> Printf.printf "%s : ident<...>" x
  | Ident (false, _) -> Printf.printf "%s : ident" x

let print_map m =
  SMap.iter
    (fun x k ->
      print_entry x k;
      print_endline "")
    m

let print_context () =
  List.iter
    (fun m ->
      print_map m;
      print_endline "----")
    !context
