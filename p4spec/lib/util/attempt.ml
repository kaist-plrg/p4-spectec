open Source

(* Backtracking *)

type failtrace = Failtrace of region * string * failtrace list
type 'a attempt = Ok of 'a | Fail of failtrace list

(* Failures *)

let rec depth_of (failtrace : failtrace) : int =
  let (Failtrace (_, _, subfailtraces)) = failtrace in
  let depth_sub = List.map depth_of subfailtraces |> List.fold_left max 0 in
  depth_sub + 1

let fail (at : region) (msg : string) : 'a attempt =
  Fail [ Failtrace (at, msg, []) ]

let fail_silent : 'a attempt = Fail []

(* Choosing between attempts *)

let rec choose_sequential = function
  | [] -> fail_silent
  | f :: fs -> (
      match f () with
      | Ok a -> Ok a
      | Fail failtraces_h -> (
          match choose_sequential fs with
          | Ok a -> Ok a
          | Fail failtraces_t -> Fail (failtraces_h @ failtraces_t)))

(* Nesting attempts *)

let nest at msg attempt =
  match attempt with
  | Ok a -> Ok a
  | Fail failtraces -> Fail [ Failtrace (at, msg, failtraces) ]

(* Error with backfailtraces *)

let rec string_of_failtrace ?(indent = "") ?(level = 0) ~(last : bool)
    ~(limit : int) ~(bullet : string) (failtrace : failtrace) : string =
  let (Failtrace (region, msg, failtraces_sub)) = failtrace in
  let root = level = 0 in
  let root_limit = level = limit in
  let sfailtrace =
    if root then
      Format.asprintf "%s%s%s%s\n" indent
        (if region = no_region then ""
         else string_of_region region ^ "\n" ^ indent)
        bullet msg
    else if level < limit then ""
    else
      let prefix = if root_limit then "" else if last then "└── " else "├── " in
      let indent_prefix = if root_limit then "" else String.make 4 ' ' in
      Format.asprintf "%s%s%s%s%s\n" indent prefix
        (if region = no_region then ""
         else string_of_region region ^ "\n" ^ indent ^ indent_prefix)
        bullet msg
  in
  let indent =
    if level < limit then indent
    else if root_limit then indent
    else if last then indent ^ "    "
    else indent ^ "│   "
  in
  if root then
    Format.asprintf "%s│ ··· omitting %d traces ···\n%s" sfailtrace (limit - 1)
      (string_of_failtraces ~indent ~level:(level + 1) ~limit failtraces_sub)
  else
    Format.asprintf "%s%s" sfailtrace
      (string_of_failtraces ~indent ~level:(level + 1) ~limit failtraces_sub)

and string_of_failtraces ?(indent = "") ?(level = 0) ~(limit : int)
    (failtraces : failtrace list) : string =
  match failtraces with
  | [] -> ""
  | [ failtrace ] ->
      string_of_failtrace ~indent ~level ~last:true ~limit ~bullet:"" failtrace
  | failtraces ->
      List.mapi
        (fun idx failtrace ->
          let last = idx = List.length failtraces - 1 in
          let bullet = string_of_int (idx + 1) ^ ". " in
          string_of_failtrace ~indent ~level ~last ~limit ~bullet failtrace)
        failtraces
      |> String.concat ""

and string_of_failtraces_short (failtraces : failtrace list) : string =
  match failtraces with
  | [] -> ""
  | [ failtrace ] ->
      let depth = depth_of failtrace in
      let limit = max 0 (depth - 10) in
      string_of_failtrace ~last:true ~limit ~bullet:"" failtrace
  | failtraces ->
      List.mapi
        (fun idx failtrace ->
          let depth = depth_of failtrace in
          let limit = max 0 (depth - 10) in
          let last = idx = List.length failtraces - 1 in
          let bullet = string_of_int (idx + 1) ^ ". " in
          string_of_failtrace ~last ~limit ~bullet failtrace)
        failtraces
      |> String.concat ""
