open Util.Attempt
open Util.Source

type t = region * string

exception Error of t list

(* As failtraces *)

let rec failtraces (traces : t list) : unit failtrace list =
  match traces with
  | [] -> []
  | (at, msg) :: traces_t ->
      let failtraces = failtraces traces_t in
      [ Failtrace (at, msg, (), failtraces) ]

(* Backtracing *)

let error (at : region) (msg : string) = raise (Error [ (at, msg) ])

let error_nest (at : region) (msg : string) (traces : t list) =
  raise (Error ((at, msg) :: traces))

(* Check *)

let check (b : bool) (at : region) (msg : string) : unit =
  if not b then error at msg
