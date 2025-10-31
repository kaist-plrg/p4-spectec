open Source
open Print

(* Backtracking *)

type reason =
  (*
     Cannot determine the root cause of failure.
  *)
  | Unknown
  (*
    Any leaf node that isn't a mismatch. A stronger candidate to be the real cause for failure
    Argument: index # of the clause/rule that caused the failure
  *)
  | Root of int
  (*
    This leaf node is a possible "mismatch", i.e. the failure is caused by one of any guard patterns
    in the beginning of a clause/rule.
    Argument: index # of the clause/rule that caused the failure
  *)
  | Mismatch of int
  (*
    One of its children is a root cause node.
    Argument 1: index # of the clause/rule that caused the failure
    Argument 2: execution depth (index # of clause/rule) of the child node referenced by Arg 1
  *)
  | RootParent of int * int
  (*
    All of its children failed because of a mismatch.
    Argument 1: index # of the clause/rule that likely caused the failure
    Argument 2: execution depth (index # of clause/rule) of the child node referenced by Arg 1
  *)
  | MismatchParent of int * int

type failtrace = Failtrace of region * string * reason * failtrace list
type 'a attempt = Ok of 'a | Fail of failtrace list

(* Depth *)

let rec depth (failtrace : failtrace) : int =
  let (Failtrace (_, _, _, subfailtraces)) = failtrace in
  let subdepth = List.map depth subfailtraces |> List.fold_left max 0 in
  subdepth + 1

(* Printing *)

let string_of_reason = function
  | Unknown -> "Unknown"
  | Root idx -> "Root cause at premise " ^ string_of_int idx
  | Mismatch idx -> "Mismatch at premise " ^ string_of_int idx
  | RootParent (idx_root, _) -> "Contains root at " ^ string_of_int idx_root
  | MismatchParent (idx_root, _) ->
      "Contains mismatch at " ^ string_of_int idx_root

let rec string_of_failtrace ?(level = 0) ~(depth_limit : int) ~(bullet : string)
    (failtrace : failtrace) : string =
  let (Failtrace (region, msg, reason, subfailtraces)) = failtrace in
  let msg =
    if level < depth_limit then ""
    else
      Format.asprintf "%s%s because %s (%s) (%s)\n"
        (indent (level - depth_limit))
        bullet msg (string_of_region region) (string_of_reason reason)
  in
  Format.asprintf "%s%s" msg
    (string_of_failtraces ~level:(level + 1) ~depth_limit subfailtraces)

and string_of_failtraces ?(level = 0) ~(depth_limit : int)
    (failtraces : failtrace list) : string =
  match failtraces with
  | [] -> ""
  | [ failtrace ] ->
      string_of_failtrace ~level ~depth_limit ~bullet:"-" failtrace
  | failtraces ->
      List.mapi
        (fun idx failtrace ->
          string_of_failtrace ~level ~depth_limit
            ~bullet:(string_of_int (idx + 1) ^ ".")
            failtrace)
        failtraces
      |> String.concat ""

let prettify_failtraces ?(limit = false) (failtraces : failtrace list) : string
    =
  match failtraces with
  | [] -> ""
  | [ failtrace ] ->
      let depth_limit =
        if limit then
          let depth = depth failtrace in
          max 0 (depth - 3)
        else 0
      in
      string_of_failtrace ~depth_limit ~bullet:"-" failtrace
  | failtraces ->
      List.mapi
        (fun idx failtrace ->
          let depth_limit =
            if limit then
              let depth = depth failtrace in
              max 0 (depth - 3)
            else 0
          in
          string_of_failtrace ~depth_limit
            ~bullet:(string_of_int (idx + 1) ^ ".")
            failtrace)
        failtraces
      |> String.concat ""

(* Constructors and combinators *)

let fail (at : region) (msg : string) : 'a attempt =
  Fail [ Failtrace (at, msg, Unknown, []) ]

let fail_with_reason (at : region) (msg : string) (reason : reason) : 'a attempt
    =
  Fail [ Failtrace (at, msg, reason, []) ]

let fail_silent : 'a attempt = Fail []

let rec choice = function
  | [] -> fail_silent
  | f :: fs -> (
      match f () with
      | Ok a -> Ok a
      | Fail failtraces_h -> (
          match choice fs with
          | Ok a -> Ok a
          | Fail failtraces_t -> Fail (failtraces_h @ failtraces_t)))

let merge_failtrace_reason (failtraces : failtrace list) : reason =
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

let nest at msg attempt =
  match attempt with
  | Ok a -> Ok a
  | Fail failtraces ->
      let reason = merge_failtrace_reason failtraces in
      Fail [ Failtrace (at, msg, reason, failtraces) ]

(* Pruning and pretty-printing of failtraces *)

let rec deepest_failtraces (failtraces : failtrace list) : failtrace list =
  deepest_failtraces' failtraces |> snd

and deepest_failtraces' (failtraces : failtrace list) : int * failtrace list =
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

let rec prune_failtraces (failtraces : failtrace list) : failtrace list =
  let reason = merge_failtrace_reason failtraces in
  match reason with
  | Unknown -> failtraces
  | Root _ -> failwith "list of failtraces cannot have a root fail cause"
  | Mismatch _ -> deepest_failtraces failtraces
  | RootParent (idx_root, _) ->
      let (Failtrace (at, msg, reason, failtraces)) =
        List.nth failtraces (idx_root - 1)
      in
      [ Failtrace (at, msg, reason, prune_failtraces failtraces) ]
  | MismatchParent _ -> deepest_failtraces failtraces
