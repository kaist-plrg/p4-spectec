(* Stub Dispatch for the [spec_parts_p4] library — copied into [compiled/] by
   `make restore-stub`. Every entry point fails, telling the user to generate
   the real interpreter. Overwritten by `make gen-ocaml`. *)

[@@@warning "-8-11-26-27-30-32-33-39"]

open Util.Source
open Trampoline

let eval_func (_name : string) (_typs : Typ.t list) (_args : Value.t list) :
    Run.func_result =
  Fail (no_region, "ML interpreter: run `make gen-ocaml` to generate it")

let eval_rel (_name : string) (_args : Value.t list) : Run.rel_result =
  Fail (no_region, "ML interpreter: run `make gen-ocaml` to generate it")

let eval_program (_relname : string) (_includes : string list) (_path : string)
    : Run.program_result =
  Fail
    (`Runtime
       (no_region, "ML interpreter: run `make gen-ocaml` to generate it"))

(* Typed bridges re-exported by the real Dispatch from the generated parts.
   Stubbed here so the [spec_parts_p4] surface stays stable when the stub is in
   place (`make build`), letting [Val_native] bind them. *)

let marshal_typed (_typ : Typ.t) (_x : Obj.t) : Value.t =
  failwith "ML interpreter: run `make gen-ocaml` to generate it"

let unmarshal_typed (_typ : Typ.t) (_v : Value.t) : Obj.t =
  failwith "ML interpreter: run `make gen-ocaml` to generate it"

let case_of_typed (_x : Obj.t) (_typ : Lang.Il.typ) : Obj.t Domain.Mixfix.t =
  failwith "ML interpreter: run `make gen-ocaml` to generate it"

let make_case_typed (_mixop : Lang.Il.mixop) (_args : Obj.t list)
    (_typ : Lang.Il.typ) : Obj.t =
  failwith "ML interpreter: run `make gen-ocaml` to generate it"
