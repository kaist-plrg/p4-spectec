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

open Ast
module F = Format

let convert_dollar_to_brackets (s : string) : string =
  Str.global_replace (Str.regexp "\\$\\([0-9]+\\)") "[\\1]" s

let pp_print_option pp fmt = function
  | None -> ()
  | Some x -> F.fprintf fmt " %a" pp x

let print_int fmt i = F.fprintf fmt "%d" i
let print_string fmt s = F.fprintf fmt "%s" s
let print_quoted_string fmt s = F.fprintf fmt "\"%s\"" s
let print_name fmt name = print_quoted_string fmt name
let print_id fmt id = print_quoted_string fmt id
let print_number fmt number = print_string fmt number
let print_session fmt session = print_string fmt session
let print_port fmt port = print_string fmt port
let print_exact fmt exact = if exact then F.fprintf fmt "$" else ()
let print_packet fmt packet = print_string fmt packet
let print_expect fmt expect = print_string fmt expect

let print_arg fmt arg =
  let id, number = arg in
  F.fprintf fmt "%a:%a" print_id id print_number number

let print_action fmt action =
  let name, args = action in
  F.fprintf fmt "%a(%a)" print_name name
    (F.pp_print_list ~pp_sep:(fun fmt () -> F.fprintf fmt ",") print_arg)
    args

let print_mtchkind fmt = function
  | Num number -> print_number fmt number
  | Slash (number_l, number_r) ->
      F.fprintf fmt "%a/%a" print_number number_l print_number number_r

let print_mtch fmt mtch =
  let name, mtchkind = mtch in
  F.fprintf fmt "%a:%a" print_name name print_mtchkind mtchkind

let print_id_or_index fmt = function
  | Id id -> print_string fmt id
  | Index number -> print_number fmt number

let print_cond fmt = function
  | Eq -> print_string fmt "=="
  | Ne -> print_string fmt "!="
  | Le -> print_string fmt "<="
  | Lt -> print_string fmt "<"
  | Ge -> print_string fmt ">="
  | Gt -> print_string fmt ">"

let print_ctr fmt = function
  | Bytes -> print_string fmt "bytes"
  | Packets -> print_string fmt "packets"

let print_stmt fmt = function
  | Wait -> print_string fmt "wait"
  | RemoveAll -> print_string fmt "remove_all"
  | Expect (port, Some expect, exact) ->
      F.fprintf fmt "expect %a %a%a" print_port port print_expect expect
        print_exact exact
  | Expect (port, None, exact) ->
      F.fprintf fmt "expect %a %a" print_port port print_exact exact
  | Packet (port, packet) ->
      F.fprintf fmt "packet %a %a" print_port port print_packet packet
  | NoPacket -> print_string fmt "no_packet"
  | Add (name, int_opt, mtchs, action, id_opt) ->
      F.fprintf fmt "add %a%a %a %a%a" print_name name
        (pp_print_option print_int)
        int_opt
        (F.pp_print_list ~pp_sep:(fun fmt () -> F.fprintf fmt " ") print_mtch)
        mtchs print_action action (pp_print_option print_id) id_opt
  | SetDefault (name, action) ->
      F.fprintf fmt "setdefault %a %a" print_name name print_action action
  | CheckCounter (id, id_or_index, (ctr, cond, number)) ->
      F.fprintf fmt "check_counter %a(%a)%a %a %a" print_id id print_id_or_index
        id_or_index
        (pp_print_option print_ctr)
        ctr print_cond cond print_number number
  | MirroringAdd (session, port) ->
      F.fprintf fmt "mirroring_add %a %a" print_session session print_port port
  | MirroringAddMc (session, id) ->
      F.fprintf fmt "mirroring_add_mc %a %a" print_session session print_id id
  | MirroringGet session ->
      F.fprintf fmt "mirroring_get %a" print_session session
  | McGroupCreate id -> F.fprintf fmt "mc_mgrp_create %a" print_number id
  | McNodeCreate (id, port) ->
      F.fprintf fmt "mc_node_create %a %a" print_number id
        (F.pp_print_list ~pp_sep:(fun fmt () -> F.fprintf fmt " ") print_number)
        port
  | McNodeAssociate (id, handle) ->
      F.fprintf fmt "mc_mgrp_associate %a %a" print_number id print_number
        handle
  | RegisterRead (name, index) ->
      F.fprintf fmt "register_read %a %a" print_name name print_number index
  | RegisterWrite (name, index, number) ->
      F.fprintf fmt "register_write %a %a %a" print_name name print_number index
        print_number number
  | RegisterReset name -> F.fprintf fmt "register_reset %a" print_name name

let print_stmts fmt stmts =
  F.pp_print_list
    ~pp_sep:(fun fmt () -> F.fprintf fmt "@.")
    print_stmt fmt stmts
