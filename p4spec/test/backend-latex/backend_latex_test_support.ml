module Renderer = Backend_latex__El_latex__Renderer
module Doc = Backend_latex__El_latex__Tex__Doc
module Link = Backend_latex__El_latex__Tex__Link
module Width = Backend_latex__El_latex__Tex__Width
module Layout = Backend_latex__El_latex__Tex__Layout
module Serialize = Backend_latex__El_latex__Tex__Serialize
module Latex_error = Backend_latex__El_latex__Error

let print (name : string) (value : string) : unit =
  Printf.printf "[%s]\n%s\n" name value

let print_nonempty (name : string) (value : string) : unit =
  if String.equal value "" then Printf.printf "[%s]\n" name
  else print name value

let print_doc (name : string) (doc : Doc.t) : unit =
  print name (Serialize.to_string doc)

let print_width (name : string) (doc : Doc.t) : unit =
  Printf.printf "[%s]\n%d\n" name (Width.flat doc)

let print_resolved (name : string) ~(width : int) (doc : Doc.t) : unit =
  doc |> Layout.resolve ~width |> print_doc name

let rejected (message : string) (thunk : unit -> unit) : bool =
  try
    thunk ();
    false
  with Latex_error.LatexError error ->
    let at, actual = Backend_latex.to_region_msg error in
    at = Util.Source.no_region && String.equal message actual

let braces_balanced (text : string) : bool =
  let depth = ref 0 in
  let balanced = ref true in
  String.iter
    (function
      | '{' -> incr depth
      | '}' ->
          decr depth;
          if !depth < 0 then balanced := false
      | _ -> ())
    text;
  !balanced && !depth = 0

let list_init (length : int) (f : int -> 'a) : 'a list =
  let rec init (index : int) (values : 'a list) : 'a list =
    if index = length then List.rev values
    else init (index + 1) (f index :: values)
  in
  init 0 []
