open Interface.Wrap
module Value = Runtime.Sim.Value
module IO = Runtime.Sim.Io

(* Helpers for invoking relations in the spec *)

type call_rel = string -> Value.t list -> Value.t list

let call : call_rel ref = ref (fun _ _ -> assert false)
let register f = call := f

(* Lvalue_read *)

let lvalue_read_var (value_cursor : Value.t) (value_ctx : Value.t)
    (value_arch : Value.t) (name : string) : Value.t =
  let value_storageReference =
    let value_nameIR = wrap_text_v name in
    [ Term "`"; NT value_nameIR ] #@ "prefixedNameIR"
  in
  match
    !call "Lvalue_read"
      [ value_cursor; value_ctx; value_arch; value_storageReference ]
  with
  | [ value_value ] -> value_value
  | _ -> assert false

let lvalue_read_var_global (value_ctx : Value.t) (value_arch : Value.t)
    (name : string) : Value.t =
  let value_cursor = [ Term "GLOBAL" ] #@ "cursor" in
  lvalue_read_var value_cursor value_ctx value_arch name

let lvalue_read_dot (value_cursor : Value.t) (value_ctx : Value.t)
    (value_arch : Value.t) (name : string) (member : string) : Value.t =
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
      [ value_cursor; value_ctx; value_arch; value_storageReference ]
  with
  | [ value_value ] -> value_value
  | _ -> assert false

let lvalue_read_dot_global (value_ctx : Value.t) (value_arch : Value.t)
    (name : string) (member : string) : Value.t =
  let value_cursor = [ Term "GLOBAL" ] #@ "cursor" in
  lvalue_read_dot value_cursor value_ctx value_arch name member

(* Lvalue_write *)

let lvalue_write_var (value_cursor : Value.t) (value_ctx : Value.t)
    (value_arch : Value.t) (name : string) (value_val : Value.t) : Value.t =
  let value_prefixedNameIR =
    let value_nameIR = wrap_text_v name in
    [ Term "`"; NT value_nameIR ] #@ "prefixedNameIR"
  in
  match
    !call "Lvalue_write"
      [ value_cursor; value_ctx; value_arch; value_prefixedNameIR; value_val ]
  with
  | [ value_ctx ] -> value_ctx
  | _ -> assert false

let lvalue_write_dot (value_cursor : Value.t) (value_ctx : Value.t)
    (value_arch : Value.t) (name : string) (member : string)
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
      [ value_cursor; value_ctx; value_arch; value_storageReference; value_val ]
  with
  | [ value_ctx ] -> value_ctx
  | _ -> assert false

let lvalue_write_var_local (value_ctx : Value.t) (value_arch : Value.t)
    (name : string) (value_val : Value.t) : Value.t =
  let value_cursor = [ Term "LOCAL" ] #@ "cursor" in
  lvalue_write_var value_cursor value_ctx value_arch name value_val

let lvalue_write_dot_global (value_ctx : Value.t) (value_arch : Value.t)
    (name : string) (member : string) (value_val : Value.t) : Value.t =
  let value_cursor = [ Term "GLOBAL" ] #@ "cursor" in
  lvalue_write_dot value_cursor value_ctx value_arch name member value_val

let lvalue_write_dot_local (value_ctx : Value.t) (value_arch : Value.t)
    (name : string) (member : string) (value_val : Value.t) : Value.t =
  let value_cursor = [ Term "LOCAL" ] #@ "cursor" in
  lvalue_write_dot value_cursor value_ctx value_arch name member value_val

(* V1Model_init_packet_in/out *)

let v1model_init_packet_in (value_ctx : Value.t) (value_arch : Value.t)
    (value_packet_in_state : Value.t) : Value.t * Value.t =
  match
    !call "V1Model_init_packet_in"
      [ value_ctx; value_arch; value_packet_in_state ]
  with
  | [ value_ctx; value_arch ] -> (value_ctx, value_arch)
  | _ -> assert false

let v1model_init_packet_out (value_ctx : Value.t) (value_arch : Value.t)
    (value_packet_out_state : Value.t) : Value.t * Value.t =
  match
    !call "V1Model_init_packet_out"
      [ value_ctx; value_arch; value_packet_out_state ]
  with
  | [ value_ctx; value_arch ] -> (value_ctx, value_arch)
  | _ -> assert false

(* V1Model_init_globals *)

let v1model_init_globals (value_ctx : Value.t) (value_arch : Value.t)
    (port : IO.port) : Value.t =
  let value_port = port |> Bigint.of_int |> wrap_num_v_int in
  match !call "V1Model_init_globals" [ value_ctx; value_arch; value_port ] with
  | [ value_ctx ] -> value_ctx
  | _ -> assert false

(* V1Model_parser/verify/ig/eg/ck/dep *)

let v1model_parser (value_ctx : Value.t) (value_arch : Value.t) :
    Value.t * Value.t * Value.t =
  match !call "V1Model_parser" [ value_ctx; value_arch ] with
  | [ value_ctx; value_arch; value_callResult ] ->
      (value_ctx, value_arch, value_callResult)
  | _ -> assert false

let v1model_verify (value_ctx : Value.t) (value_arch : Value.t) :
    Value.t * Value.t * Value.t =
  match !call "V1Model_verify" [ value_ctx; value_arch ] with
  | [ value_ctx; value_arch; value_callResult ] ->
      (value_ctx, value_arch, value_callResult)
  | _ -> assert false

let v1model_ingress (value_ctx : Value.t) (value_arch : Value.t) :
    Value.t * Value.t * Value.t =
  match !call "V1Model_ingress" [ value_ctx; value_arch ] with
  | [ value_ctx; value_arch; value_callResult ] ->
      (value_ctx, value_arch, value_callResult)
  | _ -> assert false

let v1model_egress (value_ctx : Value.t) (value_arch : Value.t) :
    Value.t * Value.t * Value.t =
  match !call "V1Model_egress" [ value_ctx; value_arch ] with
  | [ value_ctx; value_arch; value_callResult ] ->
      (value_ctx, value_arch, value_callResult)
  | _ -> assert false

let v1model_check (value_ctx : Value.t) (value_arch : Value.t) :
    Value.t * Value.t * Value.t =
  match !call "V1Model_check" [ value_ctx; value_arch ] with
  | [ value_ctx; value_arch; value_callResult ] ->
      (value_ctx, value_arch, value_callResult)
  | _ -> assert false

let v1model_deparse (value_ctx : Value.t) (value_arch : Value.t) :
    Value.t * Value.t * Value.t =
  match !call "V1Model_deparse" [ value_ctx; value_arch ] with
  | [ value_ctx; value_arch; value_callResult ] ->
      (value_ctx, value_arch, value_callResult)
  | _ -> assert false

let v1model_setup_preserved_meta_fields (value_ctx : Value.t)
    (value_arch : Value.t) (value_index : Value.t) : Value.t =
  match
    !call "V1Model_setup_preserved_meta_fields"
      [ value_ctx; value_arch; value_index ]
  with
  | [ value_ctx ] -> value_ctx
  | _ -> assert false

(* EBPF_init_packet_in *)

let ebpf_init_packet_in (value_ctx : Value.t) (value_arch : Value.t)
    (value_packet_in_state : Value.t) : Value.t * Value.t =
  match
    !call "EBPF_init_packet_in" [ value_ctx; value_arch; value_packet_in_state ]
  with
  | [ value_ctx; value_arch ] -> (value_ctx, value_arch)
  | _ -> assert false

(* EBPF_init_globals *)

let ebpf_init_globals (value_ctx : Value.t) (value_arch : Value.t) : Value.t =
  match !call "EBPF_init_globals" [ value_ctx; value_arch ] with
  | [ value_ctx ] -> value_ctx
  | _ -> assert false

(* EBPF_parse/filter *)

let ebpf_parse (value_ctx : Value.t) (value_arch : Value.t) :
    Value.t * Value.t * Value.t =
  match !call "EBPF_parse" [ value_ctx; value_arch ] with
  | [ value_ctx; value_arch; value_callResult ] ->
      (value_ctx, value_arch, value_callResult)
  | _ -> assert false

let ebpf_filter (value_ctx : Value.t) (value_arch : Value.t) :
    Value.t * Value.t * Value.t =
  match !call "EBPF_filter" [ value_ctx; value_arch ] with
  | [ value_ctx; value_arch; value_callResult ] ->
      (value_ctx, value_arch, value_callResult)
  | _ -> assert false

(* PSA_ingress_init_packet_in/out *)

let psa_ingress_init_packet_in (value_ctx : Value.t) (value_arch : Value.t)
    (value_packet_in_state : Value.t) : Value.t * Value.t =
  match
    !call "PSA_ingress_init_packet_in"
      [ value_ctx; value_arch; value_packet_in_state ]
  with
  | [ value_ctx; value_arch ] -> (value_ctx, value_arch)
  | _ -> assert false

let psa_ingress_init_packet_out (value_ctx : Value.t) (value_arch : Value.t)
    (value_packet_out_state : Value.t) : Value.t * Value.t =
  match
    !call "PSA_ingress_init_packet_out"
      [ value_ctx; value_arch; value_packet_out_state ]
  with
  | [ value_ctx; value_arch ] -> (value_ctx, value_arch)
  | _ -> assert false

(* PSA_ingress_init_metadata *)

let psa_ingress_init_metadata (value_ctx : Value.t) (value_arch : Value.t)
    (port : IO.port) (path : string) : Value.t =
  let value_port = port |> Bigint.of_int |> wrap_num_v_int in
  let value_path = wrap_text_v path in
  match
    !call "PSA_ingress_init_metadata"
      [ value_ctx; value_arch; value_port; value_path ]
  with
  | [ value_ctx ] -> value_ctx
  | _ -> assert false

(* PSA_ingress_init_globals *)

let psa_ingress_init_globals (value_ctx : Value.t) (value_arch : Value.t)
    (port : IO.port) : Value.t =
  let value_port = port |> Bigint.of_int |> wrap_num_v_int in
  match
    !call "PSA_ingress_init_globals" [ value_ctx; value_arch; value_port ]
  with
  | [ value_ctx ] -> value_ctx
  | _ -> assert false

(* PSA_ip/ig/id *)

let psa_ingress_parser (value_ctx : Value.t) (value_arch : Value.t) :
    Value.t * Value.t * Value.t =
  match !call "PSA_ingress_parser" [ value_ctx; value_arch ] with
  | [ value_ctx; value_arch; value_callResult ] ->
      (value_ctx, value_arch, value_callResult)
  | _ -> assert false

let psa_ingress (value_ctx : Value.t) (value_arch : Value.t) :
    Value.t * Value.t * Value.t =
  match !call "PSA_ingress" [ value_ctx; value_arch ] with
  | [ value_ctx; value_arch; value_callResult ] ->
      (value_ctx, value_arch, value_callResult)
  | _ -> assert false

let psa_ingress_deparser (value_ctx : Value.t) (value_arch : Value.t) :
    Value.t * Value.t * Value.t =
  match !call "PSA_ingress_deparser" [ value_ctx; value_arch ] with
  | [ value_ctx; value_arch; value_callResult ] ->
      (value_ctx, value_arch, value_callResult)
  | _ -> assert false

(* PSA_egress_init_packet_in/out *)

let psa_egress_init_packet_in (value_ctx : Value.t) (value_arch : Value.t)
    (value_packet_in_state : Value.t) : Value.t * Value.t =
  match
    !call "PSA_egress_init_packet_in"
      [ value_ctx; value_arch; value_packet_in_state ]
  with
  | [ value_ctx; value_arch ] -> (value_ctx, value_arch)
  | _ -> assert false

let psa_egress_init_packet_out (value_ctx : Value.t) (value_arch : Value.t)
    (value_packet_out_state : Value.t) : Value.t * Value.t =
  match
    !call "PSA_egress_init_packet_out"
      [ value_ctx; value_arch; value_packet_out_state ]
  with
  | [ value_ctx; value_arch ] -> (value_ctx, value_arch)
  | _ -> assert false

(* PSA_egress_init_metadata *)

let psa_egress_init_metadata (value_ctx : Value.t) (value_arch : Value.t)
    (port : IO.port) (path : string) (cos : int) (inst : int) : Value.t =
  let value_port = port |> Bigint.of_int |> wrap_num_v_int in
  let value_path = wrap_text_v path in
  let value_cos = cos |> Bigint.of_int |> wrap_num_v_int in
  let value_inst = inst |> Bigint.of_int |> wrap_num_v_int in
  match
    !call "PSA_egress_init_metadata"
      [ value_ctx; value_arch; value_port; value_path; value_cos; value_inst ]
  with
  | [ value_ctx ] -> value_ctx
  | _ -> assert false

(* PSA_egress_init_globals *)

let psa_egress_init_globals (value_ctx : Value.t) (value_arch : Value.t)
    (port : IO.port) : Value.t =
  let value_port = port |> Bigint.of_int |> wrap_num_v_int in
  match
    !call "PSA_egress_init_globals" [ value_ctx; value_arch; value_port ]
  with
  | [ value_ctx ] -> value_ctx
  | _ -> assert false

(* PSA_ep/eg/ed *)

let psa_egress_parser (value_ctx : Value.t) (value_arch : Value.t) :
    Value.t * Value.t * Value.t =
  match !call "PSA_egress_parser" [ value_ctx; value_arch ] with
  | [ value_ctx; value_arch; value_callResult ] ->
      (value_ctx, value_arch, value_callResult)
  | _ -> assert false

let psa_egress (value_ctx : Value.t) (value_arch : Value.t) :
    Value.t * Value.t * Value.t =
  match !call "PSA_egress" [ value_ctx; value_arch ] with
  | [ value_ctx; value_arch; value_callResult ] ->
      (value_ctx, value_arch, value_callResult)
  | _ -> assert false

let psa_egress_deparser (value_ctx : Value.t) (value_arch : Value.t) :
    Value.t * Value.t * Value.t =
  match !call "PSA_egress_deparser" [ value_ctx; value_arch ] with
  | [ value_ctx; value_arch; value_callResult ] ->
      (value_ctx, value_arch, value_callResult)
  | _ -> assert false
