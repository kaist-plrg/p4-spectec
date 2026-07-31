open Source

(* Backtracking *)

type failtrace = Failtrace of region * (unit -> string) * failtrace list
type 'a attempt = Ok of 'a | Fail of failtrace list

(* Failures *)

let rec depth_of (failtrace : failtrace) : int =
  let (Failtrace (_, _, subfailtraces)) = failtrace in
  let depth_sub = List.map depth_of subfailtraces |> List.fold_left max 0 in
  depth_sub + 1

let fail (at : region) (msg : string) : 'a attempt =
  Fail [ Failtrace (at, (fun () -> msg), []) ]

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
  | Fail failtraces -> Fail [ Failtrace (at, (fun () -> msg), failtraces) ]

(* Error with backfailtraces

   Show at most [short_window] frames of each root-to-leaf path *)

let short_window = 10

let region_line (indent : string) (region : region) : string =
  if region = no_region then "" else string_of_region region ^ "\n" ^ indent

let rec string_of_failtrace ~(indent : string) ~(run : int) ~(root : bool)
    ~(last : bool) ~(bullet : string) (failtrace : failtrace) : string =
  let (Failtrace (region, msg, failtraces_sub)) = failtrace in
  if (not root) && depth_of failtrace > short_window then
    string_of_failtraces ~indent ~run:(run + 1) failtraces_sub
  else
    let msg = msg () in
    let marker =
      if run > 0 then
        Format.asprintf "%s│ ··· omitting %d traces ···\n" indent run
      else ""
    in
    let boundary = root || run > 0 in
    let node, indent_sub =
      if boundary then
        ( Format.asprintf "%s%s%s%s\n" indent
            (region_line indent region)
            bullet msg,
          indent )
      else
        let prefix = if last then "└── " else "├── " in
        let indent_sub = if last then indent ^ "    " else indent ^ "│   " in
        ( Format.asprintf "%s%s%s%s%s\n" indent prefix
            (region_line (indent ^ "    ") region)
            bullet msg,
          indent_sub )
    in
    marker ^ node
    ^ string_of_failtraces ~indent:indent_sub ~run:0 failtraces_sub

and string_of_failtraces ~(indent : string) ~(run : int)
    (failtraces : failtrace list) : string =
  match failtraces with
  | [] -> ""
  | [ failtrace ] ->
      string_of_failtrace ~indent ~run ~root:false ~last:true ~bullet:""
        failtrace
  | failtraces ->
      List.mapi
        (fun idx failtrace ->
          let last = idx = List.length failtraces - 1 in
          let bullet = string_of_int (idx + 1) ^ ". " in
          string_of_failtrace ~indent ~run ~root:false ~last ~bullet failtrace)
        failtraces
      |> String.concat ""

let string_of_failtraces_short (failtraces : failtrace list) : string =
  match failtraces with
  | [] -> ""
  | [ failtrace ] ->
      string_of_failtrace ~indent:"" ~run:0 ~root:true ~last:true ~bullet:""
        failtrace
  | failtraces ->
      List.mapi
        (fun idx failtrace ->
          let last = idx = List.length failtraces - 1 in
          let bullet = string_of_int (idx + 1) ^ ". " in
          string_of_failtrace ~indent:"" ~run:0 ~root:true ~last ~bullet
            failtrace)
        failtraces
      |> String.concat ""
