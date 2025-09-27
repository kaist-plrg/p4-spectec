open Source
open Print

(* Backtracking *)

type reason =
  | RootClause of int * int
  | MismatchClause of int * int (* clause idx, max (premise_idx) *)
  | Root of int
  | Mismatch of int (* premise idx *)
  | Unknown

type failtrace = Failtrace of region * string * reason * failtrace list
type 'a attempt = Ok of 'a | Fail of failtrace list

let string_of_reason = function
  | RootClause (clause_idx, _) -> "Clause " ^ string_of_int clause_idx
  | MismatchClause (clause_idx, _) ->
      "MismatchClause at clause " ^ string_of_int clause_idx
  | Mismatch i -> "Mismatch at premise " ^ string_of_int i
  | Root i -> "Root cause at premise " ^ string_of_int i
  | Unknown -> "Unknown"

let rec depth (failtrace : failtrace) : int =
  let (Failtrace (_, _, _, subfailtraces)) = failtrace in
  let subdepth = List.map depth subfailtraces |> List.fold_left max 0 in
  subdepth + 1

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
  |> List.mapi (fun i x -> (i + 1, x))
  |> List.fold_left
       (fun acc (i, Failtrace (_, _, reason, _)) ->
         match (acc, reason) with
         | Root premise, _ -> RootClause (i, premise)
         | RootClause (_, premise_1), Root premise_2
         | RootClause (_, premise_1), RootClause (_, premise_2) ->
             if premise_1 < premise_2 then RootClause (i, premise_2) else acc
         | RootClause _, _ -> acc
         | _, Root premise | _, RootClause (_, premise) ->
             RootClause (i, premise)
         | MismatchClause (_, premise_1), MismatchClause (_, premise_2) ->
             if premise_1 < premise_2 then MismatchClause (i, premise_2)
             else acc
         | _, MismatchClause (_, max_premise) | _, Mismatch max_premise ->
             MismatchClause (i, max_premise)
         | _, Unknown -> acc)
       Unknown

let nest at msg attempt =
  match attempt with
  | Ok a -> Ok a
  | Fail failtraces ->
      let reason = merge_failtrace_reason failtraces in
      Fail [ Failtrace (at, msg, reason, failtraces) ]

(* Error with backfailtraces *)

let rec string_of_failtrace ?(level = 0) ~(depth : int) ~(bullet : string)
    (failtrace : failtrace) : string =
  let (Failtrace (region, msg, reason, subfailtraces)) = failtrace in
  let smsg =
    if level < depth then ""
    else
      Format.asprintf "%s%s because %s (%s) (%s)\n"
        (indent (level - depth))
        bullet msg (string_of_region region) (string_of_reason reason)
  in
  Format.asprintf "%s%s" smsg
    (string_of_failtraces ~level:(level + 1) ~depth subfailtraces)

and string_of_failtraces ?(level = 0) ~(depth : int)
    (failtraces : failtrace list) : string =
  match failtraces with
  | [] -> ""
  | [ failtrace ] -> string_of_failtrace ~level ~depth ~bullet:"-" failtrace
  | failtraces ->
      List.mapi
        (fun idx failtrace ->
          string_of_failtrace ~level ~depth
            ~bullet:(string_of_int (idx + 1) ^ ".")
            failtrace)
        failtraces
      |> String.concat ""

let rec deepest_failtraces_aux (failtraces : failtrace list) :
    int * failtrace list =
  match failtraces with
  | [] -> failwith "Attempt to compute deepest failtrace on empty failtrace"
  | [ (Failtrace (_, _, _, []) as ft) ] -> (1, [ ft ])
  | _ ->
      let length, dfts =
        failtraces
        |> List.map (fun (Failtrace (region, message, reason, sub_fts)) ->
               let length, dfts = deepest_failtraces_aux sub_fts in
               (length + 1, Failtrace (region, message, reason, dfts)))
        |> List.fold_left
             (fun (acc_length, acc_failtraces) (cur_length, cur_failtrace) ->
               if acc_length > cur_length then (acc_length, acc_failtraces)
               else if acc_length < cur_length then
                 (cur_length, [ cur_failtrace ])
               else
                 let acc_reason =
                   match acc_failtraces with
                   | Failtrace (_, _, acc_reason, _) :: _ -> acc_reason
                   | [] ->
                       failwith "acc_failtraces is guaranteed to be non-empty!"
                 in
                 let (Failtrace (_, _, cur_reason, _)) = cur_failtrace in
                 match (acc_reason, cur_reason) with
                 | ( MismatchClause (_, acc_premise_idx),
                     MismatchClause (_, cur_premise_idx) ) ->
                     if acc_premise_idx > cur_premise_idx then
                       (acc_length, acc_failtraces)
                     else if acc_premise_idx < cur_premise_idx then
                       (cur_length, [ cur_failtrace ])
                     else (cur_length, cur_failtrace :: acc_failtraces)
                 | _, _ -> (cur_length, cur_failtrace :: acc_failtraces))
             (0, [])
      in
      (length, List.rev dfts)

let deepest_failtraces (failtraces : failtrace list) : failtrace list =
  deepest_failtraces_aux failtraces |> snd

let rec prune_failtraces (failtraces : failtrace list) : failtrace list =
  let reason = merge_failtrace_reason failtraces in
  match reason with
  | RootClause (i, _) ->
      let (Failtrace (region, msg, reason, fts)) =
        List.nth failtraces (i - 1)
      in
      [ Failtrace (region, msg, reason, prune_failtraces fts) ]
  | Root _ ->
      failwith "Invalid state. List of failtraces cannot have Root fail cause"
  | MismatchClause _ | Mismatch _ -> deepest_failtraces failtraces
  | Unknown -> failtraces

let prettify_failtraces ?(depth_limit = false) (failtraces : failtrace list) :
    string =
  match failtraces with
  | [] -> ""
  | [ failtrace ] ->
      let depth =
        if depth_limit then
          let depth = depth failtrace in
          max 0 (depth - 3)
        else 0
      in
      string_of_failtrace ~depth ~bullet:"-" failtrace
  | failtraces ->
      List.mapi
        (fun idx failtrace ->
          let depth =
            if depth_limit then
              let depth = depth failtrace in
              max 0 (depth - 3)
            else 0
          in
          string_of_failtrace ~depth
            ~bullet:(string_of_int (idx + 1) ^ ".")
            failtrace)
        failtraces
      |> String.concat ""
