include Util.Attempt
open Error
open Util.Source

(* Monadic interface *)

let ( let* ) (attempt : 'a attempt) (f : 'a -> 'b) : 'b =
  match attempt with Ok a -> f a | Fail _ as fail -> fail

let error_with_failtraces (failtraces : failtrace list) =
  let sfailtrace =
    match failtraces with
    | [] -> ""
    | [ failtrace ] ->
        let depth = depth failtrace in
        let depth_limit = max 0 (depth - 10) in
        string_of_failtrace ~depth_limit ~bullet:"-" failtrace
    | failtraces ->
        List.mapi
          (fun idx failtrace ->
            let depth = depth failtrace in
            let depth_limit = max 0 (depth - 10) in
            string_of_failtrace ~depth_limit
              ~bullet:(string_of_int (idx + 1) ^ ".")
              failtrace)
          failtraces
        |> String.concat ""
  in
  error no_region ("tracing backtrack logs:\n" ^ sfailtrace)

let ( let+ ) (attempt : 'a attempt) (f : 'a -> 'b) : 'b =
  match attempt with Ok a -> f a | Fail traces -> error_with_failtraces traces
