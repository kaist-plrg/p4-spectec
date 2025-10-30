include Util.Attempt
open Error
open Util.Source

(* Check *)

let check_fail (b : bool) (at : region) (msg : string) =
  if b then Ok () else fail at msg

(* Monadic interface *)

let ( let* ) (attempt : 'a attempt) (f : 'a -> 'b) : 'b =
  match attempt with Ok a -> f a | Fail _ as fail -> fail

let error_with_failtraces (failtraces : failtrace list) =
  let msg =
    match !Trace.mode with
    | Trace.Full -> prettify_failtraces ~limit:false failtraces
    | Trace.Concise -> prune_failtraces failtraces |> prettify_failtraces
  in
  error no_region ("tracing backtrack logs:\n" ^ msg)

let ( let+ ) (attempt : 'a attempt) (f : 'a -> 'b) : 'b =
  match attempt with Ok a -> f a | Fail traces -> error_with_failtraces traces
