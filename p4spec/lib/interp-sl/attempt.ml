include Util.Attempt
open Error
open Util.Source

(* Types *)

type failtrace_unit = unit failtrace
type 'a attempt_unit = ('a, unit) attempt

(* Fail *)

let fail_unit (at : region) (msg : string) : 'a attempt_unit = fail at () msg

let nest_unit (at : region) (msg : string) (attempt : 'a attempt_unit) :
    'a attempt_unit =
  nest at () msg attempt

(* Check *)

let check_fail (b : bool) (at : region) (msg : string) =
  if b then Ok () else fail at () msg

(* Monadic interface *)

let ( let* ) (attempt : 'a attempt_unit) (f : 'a -> 'b) : 'b =
  match attempt with Ok a -> f a | Fail _ as fail -> fail

let ( let+ ) (attempt : 'a attempt_unit) (f : 'a -> 'b) : 'b =
  match attempt with
  | Ok a -> f a
  | Fail failtraces ->
      error no_region
        ("tracing backtrack logs:\n" ^ string_of_failtraces_short failtraces)
