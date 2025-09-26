include Util.Attempt
open Error
open Util.Source

(* Check *)

let check_fail (b : bool) (at : region) (msg : string) =
  if b then Ok () else fail at msg

(* Monadic interface *)

let ( let* ) (attempt : 'a attempt) (f : 'a -> 'b) : 'b =
  match attempt with Ok a -> f a | Fail _ as fail -> fail

let rec prune_failtraces (failtraces : failtrace list) : failtrace list =
  let reason = merge_failtrace_reason failtraces in
  match reason with
  | RootClause i ->
      let (Failtrace (region, msg, reason, fts)) =
        List.nth failtraces (i - 1)
      in
      [ Failtrace (region, msg, reason, prune_failtraces fts) ]
  | Root ->
      error no_region
        "Invalid state. List of failtraces cannot have Root fail cause"
  | MismatchClause _ | Mismatch _ -> deepest_failtraces failtraces
  | Unknown -> failtraces

let error_with_failtraces (failtraces : failtrace list) =
  let failtraces' =
    if Trace_mode.get () = `Full then failtraces
    else prune_failtraces failtraces
  in
  let sfailtrace =
    match failtraces' with
    | [] -> ""
    | [ failtrace ] -> string_of_failtrace ~depth:0 ~bullet:"-" failtrace
    | failtraces ->
        List.mapi
          (fun idx failtrace ->
            string_of_failtrace ~depth:0
              ~bullet:(string_of_int (idx + 1) ^ ".")
              failtrace)
          failtraces
        |> String.concat ""
  in
  error no_region ("tracing backtrack logs:\n" ^ sfailtrace)

let ( let+ ) (attempt : 'a attempt) (f : 'a -> 'b) : 'b =
  match attempt with Ok a -> f a | Fail traces -> error_with_failtraces traces
