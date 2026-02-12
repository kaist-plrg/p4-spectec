open Attempt
open Source

type t = region * string

exception Backtrace of t list

(* As failtraces *)

let rec back_failtraces (traces : t list) : failtrace list =
  match traces with
  | [] -> []
  | (at, msg) :: traces_t ->
      let failtraces = back_failtraces traces_t in
      [ Failtrace (at, msg, failtraces) ]

(* Backtracing *)

let back (at : region) (msg : string) = raise (Backtrace [ (at, msg) ])

let back_nest (at : region) (msg : string) (traces : t list) =
  raise (Backtrace ((at, msg) :: traces))

(* Check *)

let check_back (b : bool) (at : region) (msg : string) : unit =
  if not b then back at msg
