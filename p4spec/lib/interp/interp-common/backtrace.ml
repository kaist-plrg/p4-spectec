module Run = Runtime.Dynamic_Runner.Signature
open Util.Attempt
open Util.Source

(* Backtraces *)

type trace = Frame of region * (unit -> string) | Nested of failtrace list
type backtrace = Err of trace list | Unmatch of trace list

exception Backtrace of backtrace

(* As failtraces *)

let rec append_failtraces failtraces failtraces_suffix =
  match failtraces with
  | [] -> failtraces_suffix
  | failtraces ->
      List.map
        (fun (Failtrace (at, msg, failtraces_children)) ->
          let failtraces_children =
            if failtraces_children = [] then failtraces_suffix
            else append_failtraces failtraces_children failtraces_suffix
          in
          Failtrace (at, msg, failtraces_children))
        failtraces

let back_failtraces (backtrace : backtrace) : failtrace list =
  let rec of_traces (traces : trace list) : failtrace list =
    match traces with
    | [] -> []
    | Frame (at, msg) :: traces_t ->
        let failtraces = of_traces traces_t in
        [ Failtrace (at, msg, failtraces) ]
    | Nested failtraces :: traces_t ->
        append_failtraces failtraces (of_traces traces_t)
  in
  match backtrace with Err traces | Unmatch traces -> of_traces traces

(* Depth *)

let depth_failtraces failtraces =
  List.fold_left
    (fun depth failtrace -> max depth (depth_of failtrace))
    0 failtraces

let depth_traces traces =
  List.fold_left
    (fun depth -> function
      | Frame _ -> depth + 1
      | Nested failtraces -> depth + depth_failtraces failtraces)
    0 traces

(* Backtracing *)

let back (backtrace : backtrace) = raise (Backtrace backtrace)

let back_err (at : region) (msg : string) =
  let traces = [ Frame (at, fun () -> msg) ] in
  raise (Backtrace (Err traces))

let back_unmatch (at : region) (msg : string) =
  let traces = [ Frame (at, fun () -> msg) ] in
  raise (Backtrace (Unmatch traces))

let back_unmatch_of_failure (failure : Run.failure) : 'a =
  match failure with
  | Run.Diagnostic _ -> raise (Run.ExternError failure)
  | Run.Failtraces failtraces ->
      raise (Backtrace (Unmatch [ Nested failtraces ]))

let back_nest (at : region) (msg : unit -> string) (backtrace : backtrace) =
  let trace = Frame (at, msg) in
  match backtrace with
  | Err traces -> raise (Backtrace (Err (trace :: traces)))
  | Unmatch traces -> raise (Backtrace (Unmatch (trace :: traces)))

(* Check *)

let check_back_err (b : bool) (at : region) (msg : string) : unit =
  if not b then back_err at msg
