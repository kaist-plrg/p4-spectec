module Value = Runtime.Value
module IO = Runtime.Sim.Io

(* Helpers for invoking relations taking a program in the spec *)

module Make () = struct
  (* A trampoline for calling relations on programs in the spec,
     which will be registered at initialization time. *)

  type call_pgm = string -> string list -> string -> Value.t * Value.t

  let call : call_pgm ref = ref (fun _ _ _ -> assert false)
  let register f = call := f

  (* V1Model_init *)

  let v1model_init (includes_p4 : string list) (filename_p4 : string) :
      Value.t * Value.t =
    !call "V1Model_init" includes_p4 filename_p4

  (* EBPF_init *)

  let ebpf_init (includes_p4 : string list) (filename_p4 : string) :
      Value.t * Value.t =
    !call "EBPF_init" includes_p4 filename_p4

  (* PSA_init *)

  let psa_init (includes_p4 : string list) (filename_p4 : string) :
      Value.t * Value.t =
    !call "PSA_init" includes_p4 filename_p4

  (* NanoSwitch_init *)

  let nanoswitch_init (includes_p4 : string list) (filename_p4 : string) :
      Value.t * Value.t =
    !call "NanoSwitch_init" includes_p4 filename_p4
end

module type S = module type of Make ()
