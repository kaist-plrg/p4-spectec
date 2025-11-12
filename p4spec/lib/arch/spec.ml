open Interface.Wrap
open Interface.Unwrap
module IO = Runtime_simulator.Io
module Value = Runtime_dynamic.Value

(* Helpers for invoking relations in the spec *)

(* Lvalue_write *)

let lvalue_write_var call_rel (value_cursor : Value.t) (value_ctx : Value.t)
    (value_sto : Value.t) (name : string) (value_val : Value.t) : Value.t =
  let value_prefixedNameIR =
    let value_nameIR = wrap_text_v name in
    [ Term "`"; NT value_nameIR ] #@ "prefixedNameIR"
  in
  match
    call_rel "Lvalue_write"
      [ value_cursor; value_ctx; value_sto; value_prefixedNameIR; value_val ]
  with
  | [ value_ctx ] -> value_ctx
  | _ -> assert false

let lvalue_write_dot call_rel (value_cursor : Value.t) (value_ctx : Value.t)
    (value_sto : Value.t) (name : string) (member : string)
    (value_val : Value.t) : Value.t =
  let value_prefixedNameIR =
    let value_nameIR = wrap_text_v name in
    [ Term "`"; NT value_nameIR ] #@ "prefixedNameIR"
  in
  let value_storageReference =
    let value_memberIR = wrap_text_v member in
    [ NT value_prefixedNameIR; Term "."; NT value_memberIR ]
    #@ "storageReference"
  in
  match
    call_rel "Lvalue_write"
      [ value_cursor; value_ctx; value_sto; value_storageReference; value_val ]
  with
  | [ value_ctx ] -> value_ctx
  | _ -> assert false

let lvalue_write_var_local call_rel (value_ctx : Value.t) (value_sto : Value.t)
    (name : string) (value_val : Value.t) : Value.t =
  let value_cursor = [ Term "LOCAL" ] #@ "cursor" in
  lvalue_write_var call_rel value_cursor value_ctx value_sto name value_val

let lvalue_write_dot_global call_rel (value_ctx : Value.t) (value_sto : Value.t)
    (name : string) (member : string) (value_val : Value.t) : Value.t =
  let value_cursor = [ Term "GLOBAL" ] #@ "cursor" in
  lvalue_write_dot call_rel value_cursor value_ctx value_sto name member
    value_val

let lvalue_write_dot_local call_rel (value_ctx : Value.t) (value_sto : Value.t)
    (name : string) (member : string) (value_val : Value.t) : Value.t =
  let value_cursor = [ Term "LOCAL" ] #@ "cursor" in
  lvalue_write_dot call_rel value_cursor value_ctx value_sto name member
    value_val

(* V1Model_init_packet_in/out *)

let v1model_init_packet_in call_rel (value_ctx : Value.t) (value_sto : Value.t)
    (value_packet_in_state : Value.t) : Value.t * Value.t =
  match
    call_rel "V1Model_init_packet_in"
      [ value_ctx; value_sto; value_packet_in_state ]
  with
  | [ value_ctx; value_sto ] -> (value_ctx, value_sto)
  | _ -> assert false

let v1model_init_packet_out call_rel (value_ctx : Value.t) (value_sto : Value.t)
    (value_packet_out_state : Value.t) : Value.t * Value.t =
  match
    call_rel "V1Model_init_packet_out"
      [ value_ctx; value_sto; value_packet_out_state ]
  with
  | [ value_ctx; value_sto ] -> (value_ctx, value_sto)
  | _ -> assert false

(* V1Model_init_globals *)

let v1model_init_globals call_rel (value_ctx : Value.t) (value_sto : Value.t)
    (port : IO.port) : Value.t =
  let value_port = port |> wrap_num_v_int in
  match
    call_rel "V1Model_init_globals" [ value_ctx; value_sto; value_port ]
  with
  | [ value_ctx ] -> value_ctx
  | _ -> assert false

(* V1Model_parser/verify/ig/eg/ck/dep *)

let v1model_parser call_rel (value_ctx : Value.t) (value_sto : Value.t) :
    Value.t * Value.t * Value.t =
  match call_rel "V1Model_parser" [ value_ctx; value_sto ] with
  | [ value_ctx; value_sto; value_callResult ] ->
      (value_ctx, value_sto, value_callResult)
  | _ -> assert false

let v1model_verify call_rel (value_ctx : Value.t) (value_sto : Value.t) :
    Value.t * Value.t * Value.t =
  match call_rel "V1Model_verify" [ value_ctx; value_sto ] with
  | [ value_ctx; value_sto; value_callResult ] ->
      (value_ctx, value_sto, value_callResult)
  | _ -> assert false

let v1model_ingress call_rel (value_ctx : Value.t) (value_sto : Value.t) :
    Value.t * Value.t * Value.t =
  match call_rel "V1Model_ingress" [ value_ctx; value_sto ] with
  | [ value_ctx; value_sto; value_callResult ] ->
      (value_ctx, value_sto, value_callResult)
  | _ -> assert false

let v1model_egress call_rel (value_ctx : Value.t) (value_sto : Value.t) :
    Value.t * Value.t * Value.t =
  match call_rel "V1Model_egress" [ value_ctx; value_sto ] with
  | [ value_ctx; value_sto; value_callResult ] ->
      (value_ctx, value_sto, value_callResult)
  | _ -> assert false

let v1model_check call_rel (value_ctx : Value.t) (value_sto : Value.t) :
    Value.t * Value.t * Value.t =
  match call_rel "V1Model_check" [ value_ctx; value_sto ] with
  | [ value_ctx; value_sto; value_callResult ] ->
      (value_ctx, value_sto, value_callResult)
  | _ -> assert false

let v1model_deparse call_rel (value_ctx : Value.t) (value_sto : Value.t) :
    Value.t * Value.t * Value.t =
  match call_rel "V1Model_deparse" [ value_ctx; value_sto ] with
  | [ value_ctx; value_sto; value_callResult ] ->
      (value_ctx, value_sto, value_callResult)
  | _ -> assert false

(* Helpers for invoking functions in the spec *)

(* write_value_from_bits *)

let write_value_from_bits call_func (value_target : Value.t) (varsize : int)
    (bits : bool Array.t) : Value.t =
  let value_varsize = varsize |> wrap_num_v_nat in
  let value_bits =
    bits |> Array.to_list |> List.map wrap_bool_v
    |> wrap_list_v_typed Il.Ast.BoolT
  in
  call_func "write_value_from_bits" []
    [ value_target; value_varsize; value_bits ]

(* default *)

let default call_func (value_typ : Value.t) : Value.t =
  call_func "default" [] [ value_typ ]

(* sizeof_min/maxSizeInBits *)

let sizeof_minSizeInBits' call_func (value_typ : Value.t) : Bigint.t =
  call_func "sizeof_minSizeInBits'" [] [ value_typ ] |> unwrap_num_v

let sizeof_maxSizeInBits' call_func (value_typ : Value.t) : Bigint.t =
  call_func "sizeof_maxSizeInBits'" [] [ value_typ ] |> unwrap_num_v

(* update_store_externState *)

let update_store_externState call_func (value_sto : Value.t)
    (value_oid : Value.t) (value_externState : Value.t) : Value.t =
  call_func "update_store_externState" []
    [ value_sto; value_oid; value_externState ]
  |> unwrap_opt_v |> Option.get

(* find_type_e *)

let find_type_e call_func (value_cursor : Value.t) (value_ctx : Value.t)
    (name : string) : Value.t =
  let value_nameIR = wrap_text_v name in
  call_func "find_type_e" [] [ value_cursor; value_ctx; value_nameIR ]

let find_type_e_local call_func (value_ctx : Value.t) (name : string) : Value.t
    =
  let value_cursor = [ Term "LOCAL" ] #@ "cursor" in
  find_type_e call_func value_cursor value_ctx name
  |> unwrap_opt_v |> Option.get

(* find_var_e *)

let find_var_e call_func (value_cursor : Value.t) (value_ctx : Value.t)
    (name : string) : Value.t =
  let value_prefixedNameIR =
    let value_nameIR = wrap_text_v name in
    [ Term "`"; NT value_nameIR ] #@ "prefixedNameIR"
  in
  call_func "find_var_e" [] [ value_cursor; value_ctx; value_prefixedNameIR ]

let find_var_e_global call_func (value_ctx : Value.t) (name : string) : Value.t
    =
  let value_cursor = [ Term "LOCAL" ] #@ "cursor" in
  find_var_e call_func value_cursor value_ctx name

let find_var_e_local call_func (value_ctx : Value.t) (name : string) : Value.t =
  let value_cursor = [ Term "LOCAL" ] #@ "cursor" in
  find_var_e call_func value_cursor value_ctx name

(* subst_type_e *)

let subst_type_e call_func (value_cursor : Value.t) (value_ctx : Value.t)
    (value_typ : Value.t) : Value.t =
  call_func "subst_type_e" [] [ value_cursor; value_ctx; value_typ ]

let subst_type_e_local call_func (value_ctx : Value.t) (value_typ : Value.t) :
    Value.t =
  let value_cursor = [ Term "LOCAL" ] #@ "cursor" in
  subst_type_e call_func value_cursor value_ctx value_typ
