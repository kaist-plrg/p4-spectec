include Util.Attempt
open Error
open Util.Source

(* Check *)

let check_fail (b : bool) (at : region) (msg : string) =
  if b then Ok () else fail at msg

(* Monadic interface *)

let ( let* ) (attempt : 'a attempt) (f : 'a -> 'b) : 'b =
  match attempt with Ok a -> f a | Fail _ as fail -> fail

let rec _sfailtraces (failtraces : failtrace list) =
  let reason = merge_failtrace_reason failtraces in
  match reason with
  | Clause i ->
      let (Failtrace (region, msg, reason, fts)) =
        List.nth failtraces (i - 1)
      in

      Format.asprintf "- because %s (%s) (%s)\n" msg (string_of_region region)
        (string_of_reason reason)
      ^ _sfailtraces fts
  | Root ->
      error no_region
        "Invalid state. List of failtraces cannot have Root fail cause"
  | Mismatch | Unknown -> string_of_failtraces ~depth:0 failtraces

let error_with_failtraces (failtraces : failtrace list) =
  (* let error_list = deepest_failtrace failtraces in *)
  let sfailtrace =
    _sfailtraces failtraces
    (* match failtraces with *)
    (* | [] -> "" *)
    (* | [ failtrace ] -> *)
    (*     (* let depth = depth failtrace in *) *)
    (*     (* let depth = max 0 (depth - 3) in *) *)
    (*     let depth = 0 in *)
    (*     string_of_failtrace ~depth ~bullet:"-" failtrace *)
    (* | failtraces -> *)
    (*     List.mapi *)
    (*       (fun idx failtrace -> *)
    (*         (* let depth = depth failtrace in *) *)
    (*         (* let depth = max 0 (depth - 3) in *) *)
    (*         let depth = 0 in *)
    (*         string_of_failtrace ~depth *)
    (*           ~bullet:(string_of_int (idx + 1) ^ ".") *)
    (*           failtrace) *)
    (*       failtraces *)
    (*     |> String.concat "" *)
  in
  error no_region ("tracing backtrack logs:\n" ^ sfailtrace)

let ( let+ ) (attempt : 'a attempt) (f : 'a -> 'b) : 'b =
  match attempt with Ok a -> f a | Fail traces -> error_with_failtraces traces
