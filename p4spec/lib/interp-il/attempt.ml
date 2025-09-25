include Util.Attempt
open Error
open Util.Source

(* Check *)

let check_fail (b : bool) (at : region) (msg : string) =
  if b then Ok () else fail at msg

(* Monadic interface *)

let ( let* ) (attempt : 'a attempt) (f : 'a -> 'b) : 'b =
  match attempt with Ok a -> f a | Fail _ as fail -> fail

let rec prune_failtraces (failtraces : failtrace list) =
  let reason = merge_failtrace_reason failtraces in
  match reason with
  | RootClause i ->
      let (Failtrace (region, msg, reason, fts)) =
        List.nth failtraces (i - 1)
      in
      Format.asprintf "- because %s (%s) (%s)\n" msg (string_of_region region)
        (string_of_reason reason)
      ^ prune_failtraces fts
  | Root ->
      error no_region
        "Invalid state. List of failtraces cannot have Root fail cause"
  | MismatchClause _ | Mismatch _ ->
      string_of_failtraces ~depth:0 (deepest_failtraces failtraces)
  | Unknown -> string_of_failtraces ~depth:0 failtraces

let error_with_failtraces (failtraces : failtrace list) =
  let sfailtrace = prune_failtraces failtraces in
  error no_region ("tracing backtrack logs:\n" ^ sfailtrace)

let ( let+ ) (attempt : 'a attempt) (f : 'a -> 'b) : 'b =
  match attempt with Ok a -> f a | Fail traces -> error_with_failtraces traces
