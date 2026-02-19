open Util.Attempt
open Util.Source

(* Backtraces *)

type trace = region * string
type backtrace = Err of trace list | Unmatch of trace list

exception Backtrace of backtrace

(* As failtraces *)

let back_failtraces (backtrace : backtrace) : failtrace list =
  let rec back_failtraces (traces : trace list) : failtrace list =
    match traces with
    | [] -> []
    | (at, msg) :: traces_t ->
        let failtraces = back_failtraces traces_t in
        [ Failtrace (at, msg, failtraces) ]
  in
  match backtrace with Err traces | Unmatch traces -> back_failtraces traces

(* Backtracing *)

let back (backtrace : backtrace) = raise (Backtrace backtrace)

let back_err (at : region) (msg : string) =
  let traces = [ (at, msg) ] in
  raise (Backtrace (Err traces))

let back_unmatch (at : region) (msg : string) =
  let traces = [ (at, msg) ] in
  raise (Backtrace (Unmatch traces))

let back_nest (at : region) (msg : string) (backtrace : backtrace) =
  let trace = (at, msg) in
  match backtrace with
  | Err traces -> raise (Backtrace (Err (trace :: traces)))
  | Unmatch traces -> raise (Backtrace (Unmatch (trace :: traces)))

(* Check *)

let check_back_err (b : bool) (at : region) (msg : string) : unit =
  if not b then back_err at msg
