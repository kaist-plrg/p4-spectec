open Source
open Print

(* Backtracking *)

type 'note failtrace =
  | Failtrace of region * string * 'note * 'note failtrace list

type ('a, 'note) attempt = Ok of 'a | Fail of 'note failtrace list

(* Failures *)

let rec depth_of (failtrace : 'note failtrace) : int =
  let (Failtrace (_, _, _, subfailtraces)) = failtrace in
  let depth_sub = List.map depth_of subfailtraces |> List.fold_left max 0 in
  depth_sub + 1

let fail (at : region) (note : 'note) (msg : string) : ('a, 'note) attempt =
  Fail [ Failtrace (at, msg, note, []) ]

let fail_silent : ('a, 'note) attempt = Fail []

let rec choice = function
  | [] -> fail_silent
  | f :: fs -> (
      match f () with
      | Ok a -> Ok a
      | Fail failtraces_h -> (
          match choice fs with
          | Ok a -> Ok a
          | Fail failtraces_t -> Fail (failtraces_h @ failtraces_t)))

let nest at note msg attempt =
  match attempt with
  | Ok a -> Ok a
  | Fail failtraces -> Fail [ Failtrace (at, msg, note, failtraces) ]

(* Error with backfailtraces *)

let rec string_of_failtrace ?(level = 0) ~(limit : int) ~(bullet : string)
    (failtrace : 'note failtrace) : string =
  let (Failtrace (region, msg, _note, subfailtraces)) = failtrace in
  let smsg =
    if level < limit then ""
    else
      Format.asprintf "%s%s because %s (%s)\n"
        (indent (level - limit))
        bullet msg (string_of_region region)
  in
  Format.asprintf "%s%s" smsg
    (string_of_failtraces ~level:(level + 1) ~limit subfailtraces)

and string_of_failtraces ?(level = 0) ~(limit : int) (failtraces : 'note failtrace list) :
    string =
  match failtraces with
  | [] -> ""
  | [ failtrace ] -> string_of_failtrace ~level ~limit ~bullet:"-" failtrace
  | failtraces ->
      List.mapi
        (fun idx failtrace ->
          string_of_failtrace ~level ~limit
            ~bullet:(string_of_int (idx + 1) ^ ".")
            failtrace)
        failtraces
      |> String.concat ""

and string_of_failtraces_short (failtraces : 'note failtrace list) :
    string =
  match failtraces with
  | [] -> ""
  | [ failtrace ] ->
      let depth = depth_of failtrace in
      let limit = max 0 (depth - 10) in
      string_of_failtrace ~limit ~bullet:"-" failtrace
  | failtraces ->
      List.mapi
        (fun idx failtrace ->
          let depth = depth_of failtrace in
          let limit = max 0 (depth - 10) in
          string_of_failtrace ~limit
            ~bullet:(string_of_int (idx + 1) ^ ".")
            failtrace)
        failtraces
      |> String.concat ""
