open Source
open Print

(* Backtracking *)

type reason = Clause of int | Root | Mismatch | Unknown
type failtrace = Failtrace of region * string * reason * failtrace list
type 'a attempt = Ok of 'a | Fail of failtrace list

let string_of_reason = function
  | Clause i -> "Clause " ^ string_of_int i
  | Mismatch -> "Mismatch"
  | Root -> "Root"
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
         | Root, _ -> Clause i
         | Clause _, _ -> acc
         | _, Root | _, Clause _ -> Clause i
         | _, Mismatch -> Mismatch
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
