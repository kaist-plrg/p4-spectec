include Util.Attempt
open Reason
open Error
open Util.Source

(* Types *)

type failtrace_reason = t failtrace
type 'a attempt_reason = ('a, t) attempt

(* Check *)

let check_fail (b : bool) (at : region) (msg : string) =
  if b then Ok () else fail at Unknown msg

(* Merging reasons *)

let merge_reason (failtraces : failtrace_reason list) : Reason.t =
  failtraces
  |> List.mapi (fun idx failtrace -> (idx + 1, failtrace))
  |> List.fold_left
       (fun reason_acc (idx, Failtrace (_, _, reason, _)) ->
         match (reason_acc, reason) with
         | Root idx_root, _ -> RootParent (idx, idx_root)
         | RootParent (_, depth_child_a), Root idx_root_b ->
             if depth_child_a < idx_root_b then RootParent (idx, idx_root_b)
             else reason_acc
         | RootParent (_, depth_child_a), RootParent (_, depth_child_b) ->
             if depth_child_a < depth_child_b then
               RootParent (idx, depth_child_b)
             else reason_acc
         | RootParent _, _ -> reason_acc
         | _, Root idx_root -> RootParent (idx, idx_root)
         | _, RootParent (_, depth_child) -> RootParent (idx, depth_child)
         | MismatchParent (_, depth_child_a), MismatchParent (_, depth_child_b)
           ->
             if depth_child_a < depth_child_b then
               MismatchParent (idx, depth_child_b)
             else reason_acc
         | _, MismatchParent (_, depth_child_b) ->
             MismatchParent (idx, depth_child_b)
         | _, Mismatch idx_mismatch -> MismatchParent (idx, idx_mismatch)
         | _, Unknown -> reason_acc)
       Unknown

(* Pruning failtraces: on mismatch, choose the deepest *)

let rec prune_failtraces (failtraces : failtrace_reason list) :
    failtrace_reason list =
  let reason = merge_reason failtraces in
  match reason with
  | Root _ -> failwith "list of failtraces cannot have a root fail cause"
  | Unknown -> failtraces
  | Mismatch _ -> deepest_failtraces failtraces
  | RootParent (idx_root, _) ->
      let (Failtrace (at, msg, reason, failtraces)) =
        List.nth failtraces (idx_root - 1)
      in
      [ Failtrace (at, msg, reason, prune_failtraces failtraces) ]
  | MismatchParent _ -> deepest_failtraces failtraces

and deepest_failtraces (failtraces : failtrace_reason list) :
    failtrace_reason list =
  deepest_failtraces' failtraces |> snd

and deepest_failtraces' (failtraces : failtrace_reason list) :
    int * failtrace_reason list =
  match failtraces with
  | [] -> failwith "attempt to compute deepest failtrace on empty failtrace"
  | [ (Failtrace (_, _, _, []) as failtrace) ] -> (1, [ failtrace ])
  | _ ->
      let length, subfailtraces_deep =
        failtraces
        |> List.map (fun (Failtrace (at, message, reason, subfailtraces)) ->
               let length, subfailtraces_deep =
                 deepest_failtraces' subfailtraces
               in
               (length + 1, Failtrace (at, message, reason, subfailtraces_deep)))
        |> List.fold_left
             (fun (length_acc, failtraces_acc) (length_cur, failtrace_cur) ->
               if length_acc > length_cur then (length_acc, failtraces_acc)
               else if length_acc < length_cur then
                 (length_cur, [ failtrace_cur ])
               else
                 let reason_acc =
                   match failtraces with
                   | Failtrace (_, _, reason_acc, _) :: _ -> reason_acc
                   | [] -> assert false
                 in
                 let (Failtrace (_, _, reason_cur, _)) = failtrace_cur in
                 match (reason_acc, reason_cur) with
                 | ( MismatchParent (_, depth_child_acc),
                     MismatchParent (_, depth_child_cur) ) ->
                     if depth_child_acc > depth_child_cur then
                       (length_acc, failtraces_acc)
                     else if depth_child_acc < depth_child_cur then
                       (length_cur, [ failtrace_cur ])
                     else (length_cur, failtrace_cur :: failtraces_acc)
                 | _, _ -> (length_cur, failtrace_cur :: failtraces_acc))
             (0, [])
      in
      (length, List.rev subfailtraces_deep)

(* Failures *)

let fail_without_reason (at : region) (msg : string) : 'a attempt_reason =
  Fail [ Failtrace (at, msg, Unknown, []) ]

let fail_with_reason (at : region) (msg : string) (reason : Reason.t) :
    'a attempt_reason =
  Fail [ Failtrace (at, msg, reason, []) ]

let nest (at : region) (msg : string) (attempt : 'a attempt_reason) :
    'a attempt_reason =
  match attempt with
  | Ok a -> Ok a
  | Fail failtraces ->
      let reason = merge_reason failtraces in
      Fail [ Failtrace (at, msg, reason, failtraces) ]

(* Monadic interface *)

let ( let* ) (attempt : 'a attempt_reason) (f : 'a -> 'b) : 'b =
  match attempt with Ok a -> f a | Fail _ as fail -> fail

let ( let+ ) (attempt : 'a attempt_reason) (f : 'a -> 'b) : 'b =
  match attempt with
  | Ok a -> f a
  | Fail failtraces ->
      error no_region
        ("tracing backtrack logs:\n"
        ^ (failtraces |> prune_failtraces |> string_of_failtraces_short))
