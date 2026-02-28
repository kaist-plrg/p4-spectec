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
type ident_kind = TypeName of has_params | Ident of has_params

type t = {
  context : ident_kind SMap.t list;
  backup : ident_kind SMap.t list;
}

let empty = {
  context = [ SMap.empty ];
  backup = [];
}

(* Associates [id] with [k] in map for current scope *)
let declare (ctx : t) (id : string) (k : ident_kind) : t =
  let context = match ctx.context with
  | [] -> failwith "ill-formed context"
  | m :: l ->
      Debug_config.context_debug_print ">>> Declaring %s as %s\n" id
        (match k with TypeName _ -> "TypeName" | Ident _ -> "Ident");
      SMap.add id k m :: l
  in
  { ctx with context }

let declare_type ctx id has_params = declare ctx id (TypeName has_params)
let declare_types ctx types = List.fold_left (fun ctx' s -> declare_type ctx' s false) ctx types
let declare_var ctx id has_params = declare ctx id (Ident has_params)
let declare_vars ctx vars = List.fold_left (fun ctx' s -> declare_var ctx' s false) ctx vars

(* Tests whether [id] is known as a type name. *)
let get_kind (ctx : t) (id : string) : ident_kind =
  let rec loop = function
    | [] -> Ident false
    | m :: rest -> (
        match SMap.find_opt id m with None -> loop rest | Some k -> k)
  in
  loop ctx.context

let is_typename (ctx : t) (id : string) : bool =
  match get_kind ctx id with TypeName _ -> true | _ -> false

let mark_template (ctx : t) (id : string) : t =
  let rec loop = function
    | [] -> []
    | m :: rest -> (
        match SMap.find_opt id m with
        | None -> m :: loop rest
        | Some (TypeName _) -> SMap.add id (TypeName true) m :: rest
        | Some (Ident _) -> SMap.add id (Ident true) m :: rest)
  in
  { ctx with context = loop ctx.context }

(* Takes a snapshot of the current context. *)
let push_scope (ctx : t) : t =
  Debug_config.context_debug_print "[[ Pushing scope\n";
  { ctx with context = SMap.empty :: ctx.context }

(* Remove scope *)
let pop_scope (ctx : t) : t =
  Debug_config.context_debug_print "]] Popping scope\n";
  match ctx.context with
  | [] -> failwith "ill-formed context"
  | [ _ ] -> failwith "pop would produce ill-formed context"
  | _ :: l -> { ctx with context = l }

let go_toplevel (ctx : t) : t =
  let rec loop c =
    match c with
    | [] -> failwith "ill-formed context"
    | [ _ ] -> c
    | _ :: l -> loop l
  in
  { context = loop ctx.context; backup = ctx.context; }

let go_local (ctx : t) : t =
  { ctx with context = ctx.backup }

(* Printing functions for debugging *)
let print_entry x k =
  match k with
  | TypeName true -> Printf.printf "%s : type<...>" x
  | TypeName false -> Printf.printf "%s : type" x
  | Ident true -> Printf.printf "%s : ident<...>" x
  | Ident false -> Printf.printf "%s : ident" x

let print_map m =
  SMap.iter
    (fun x k ->
      print_entry x k;
      print_endline "")
    m

let print_context ctx =
  List.iter
    (fun m ->
      print_map m;
      print_endline "----")
    ctx.context
