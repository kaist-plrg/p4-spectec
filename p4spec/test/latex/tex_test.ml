module Doc = Lang__El__Latex__Tex__Doc
module Link = Lang__El__Latex__Tex__Link
module Serialize = Lang__El__Latex__Tex__Serialize

let print_doc (name : string) (doc : Doc.t) : unit =
  Printf.printf "[%s]\n%s\n" name (Serialize.to_string doc)

let () =
  let open Doc in
  concat_spaced
    [ styled_mathit "x_i"; fixed Equal; styled_texttt "#$%&_{}\\^~" ]
  |> print_doc "escaping";
  fraction (styled_mathit "p") (styled_mathit "q") |> print_doc "fraction";
  link (Link.target_of_string "call_fn'") (styled_mathrm "call_fn'")
  |> print_doc "link";
  numbered [ empty; styled_mathit "first"; empty; styled_mathit "second" ]
  |> print_doc "numbered-filtered"
