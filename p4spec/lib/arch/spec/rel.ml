open Interface.Wrap
module IO = Runtime_simulator.Io
module Value = Runtime_dynamic.Value

(* Helpers for invoking relations in the spec *)

type call_rel = string -> Value.t list -> Value.t list

let call : call_rel ref = ref (fun _ _ -> assert false)
let register f = call := f

(* Lvalue_read *)

let lvalue_read_dot (value_cursor : Value.t) (value_ctx : Value.t)
    (value_sto : Value.t) (name : string) (member : string) : Value.t =
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
    !call "Lvalue_read"
      [ value_cursor; value_ctx; value_sto; value_storageReference ]
  with
  | [ value_value ] -> value_value
  | _ -> assert false

let lvalue_read_dot_global (value_ctx : Value.t) (value_sto : Value.t)
    (name : string) (member : string) : Value.t =
  let value_cursor = [ Term "GLOBAL" ] #@ "cursor" in
  lvalue_read_dot value_cursor value_ctx value_sto name member

(* Lvalue_write *)

let lvalue_write_var (value_cursor : Value.t) (value_ctx : Value.t)
    (value_sto : Value.t) (name : string) (value_val : Value.t) : Value.t =
  let value_prefixedNameIR =
    let value_nameIR = wrap_text_v name in
    [ Term "`"; NT value_nameIR ] #@ "prefixedNameIR"
  in
  match
    !call "Lvalue_write"
      [ value_cursor; value_ctx; value_sto; value_prefixedNameIR; value_val ]
  with
  | [ value_ctx ] -> value_ctx
  | _ -> assert false

let lvalue_write_dot (value_cursor : Value.t) (value_ctx : Value.t)
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
    !call "Lvalue_write"
      [ value_cursor; value_ctx; value_sto; value_storageReference; value_val ]
  with
  | [ value_ctx ] -> value_ctx
  | _ -> assert false

let lvalue_write_var_local (value_ctx : Value.t) (value_sto : Value.t)
    (name : string) (value_val : Value.t) : Value.t =
  let value_cursor = [ Term "LOCAL" ] #@ "cursor" in
  lvalue_write_var value_cursor value_ctx value_sto name value_val

let lvalue_write_dot_global (value_ctx : Value.t) (value_sto : Value.t)
    (name : string) (member : string) (value_val : Value.t) : Value.t =
  let value_cursor = [ Term "GLOBAL" ] #@ "cursor" in
  lvalue_write_dot value_cursor value_ctx value_sto name member value_val

let lvalue_write_dot_local (value_ctx : Value.t) (value_sto : Value.t)
    (name : string) (member : string) (value_val : Value.t) : Value.t =
  let value_cursor = [ Term "LOCAL" ] #@ "cursor" in
  lvalue_write_dot value_cursor value_ctx value_sto name member value_val

(* V1Model_init_packet_in/out *)

let v1model_init_packet_in (value_ctx : Value.t) (value_sto : Value.t)
    (value_packet_in_state : Value.t) : Value.t * Value.t =
  match
    !call "V1Model_init_packet_in"
      [ value_ctx; value_sto; value_packet_in_state ]
  with
  | [ value_ctx; value_sto ] -> (value_ctx, value_sto)
  | _ -> assert false

let v1model_init_packet_out (value_ctx : Value.t) (value_sto : Value.t)
    (value_packet_out_state : Value.t) : Value.t * Value.t =
  match
    !call "V1Model_init_packet_out"
      [ value_ctx; value_sto; value_packet_out_state ]
  with
  | [ value_ctx; value_sto ] -> (value_ctx, value_sto)
  | _ -> assert false

(* V1Model_init_globals *)

let v1model_init_globals (value_ctx : Value.t) (value_sto : Value.t)
    (port : IO.port) : Value.t =
  let value_port = port |> Bigint.of_int |> wrap_num_v_int in
  match !call "V1Model_init_globals" [ value_ctx; value_sto; value_port ] with
  | [ value_ctx ] -> value_ctx
  | _ -> assert false

(* V1Model_parser/verify/ig/eg/ck/dep *)

let v1model_parser (value_ctx : Value.t) (value_sto : Value.t) :
    Value.t * Value.t * Value.t =
  match !call "V1Model_parser" [ value_ctx; value_sto ] with
  | [ value_ctx; value_sto; value_callResult ] ->
      (value_ctx, value_sto, value_callResult)
  | _ -> assert false

let v1model_verify (value_ctx : Value.t) (value_sto : Value.t) :
    Value.t * Value.t * Value.t =
  match !call "V1Model_verify" [ value_ctx; value_sto ] with
  | [ value_ctx; value_sto; value_callResult ] ->
      (value_ctx, value_sto, value_callResult)
  | _ -> assert false

let v1model_ingress (value_ctx : Value.t) (value_sto : Value.t) :
    Value.t * Value.t * Value.t =
  match !call "V1Model_ingress" [ value_ctx; value_sto ] with
  | [ value_ctx; value_sto; value_callResult ] ->
      (value_ctx, value_sto, value_callResult)
  | _ -> assert false

let v1model_egress (value_ctx : Value.t) (value_sto : Value.t) :
    Value.t * Value.t * Value.t =
  match !call "V1Model_egress" [ value_ctx; value_sto ] with
  | [ value_ctx; value_sto; value_callResult ] ->
      (value_ctx, value_sto, value_callResult)
  | _ -> assert false

let v1model_check (value_ctx : Value.t) (value_sto : Value.t) :
    Value.t * Value.t * Value.t =
  match !call "V1Model_check" [ value_ctx; value_sto ] with
  | [ value_ctx; value_sto; value_callResult ] ->
      (value_ctx, value_sto, value_callResult)
  | _ -> assert false

let v1model_deparse (value_ctx : Value.t) (value_sto : Value.t) :
    Value.t * Value.t * Value.t =
  match !call "V1Model_deparse" [ value_ctx; value_sto ] with
  | [ value_ctx; value_sto; value_callResult ] ->
      (value_ctx, value_sto, value_callResult)
  | _ -> assert false
