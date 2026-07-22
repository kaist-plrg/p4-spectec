(* Stub Dispatch for the [spec_parts_il] library — copied into [compiled/] by
   `make restore-stub`. Every entry point fails, telling the user to generate
   the real interpreter. Overwritten by `make gen-ocaml-il`. *)

[@@@warning "-8-11-26-27-30-32-33-39"]

open Util.Source
open Trampoline

let eval_func (_name : string) (_typs : Typ.t list) (_args : Value.t list) :
    Run.func_result =
  Fail (no_region, "ML interpreter: run `make gen-ocaml-il` to generate it")

let eval_rel (_name : string) (_args : Value.t list) : Run.rel_result =
  Fail (no_region, "ML interpreter: run `make gen-ocaml-il` to generate it")

let eval_func_native (_name : string) (_typs : Typ.t list)
    (_args : Value.t list) : Run.func_result =
  Fail (no_region, "ML interpreter: run `make gen-ocaml-il` to generate it")

let eval_rel_native (_name : string) (_args : Value.t list) : Run.rel_result =
  Fail (no_region, "ML interpreter: run `make gen-ocaml-il` to generate it")

let eval_program (_relname : string) (_includes : string list) (_path : string)
    : Run.program_result =
  Fail
    (`Runtime
       (no_region, "ML interpreter: run `make gen-ocaml-il` to generate it"))

let unmarshal_program (_value : Value.t) : Value.t =
  failwith "ML interpreter: run `make gen-ocaml-il` to generate it"
