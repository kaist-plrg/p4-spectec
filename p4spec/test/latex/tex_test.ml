module Doc = Lang__El__Latex__Tex__Doc
module Link = Lang__El__Latex__Tex__Link
module Width = Lang__El__Latex__Tex__Width
module Layout = Lang__El__Latex__Tex__Layout
module Serialize = Lang__El__Latex__Tex__Serialize

let print_doc (name : string) (doc : Doc.t) : unit =
  Printf.printf "[%s]\n%s\n" name (Serialize.to_string doc)

let print_width (name : string) (doc : Doc.t) : unit =
  Printf.printf "[%s]\n%d\n" name (Width.flat doc)

let print_resolved (name : string) ~(width : int) (doc : Doc.t) : unit =
  doc |> Layout.resolve ~width |> print_doc name

let () =
  let open Doc in
  concat_spaced
    [ styled_mathit "x_i"; fixed Equal; styled_texttt "#$%&_{}\\^~" ]
  |> print_doc "escaping";
  fraction (styled_mathit "p") (styled_mathit "q") |> print_doc "fraction";
  link (Link.target_of_string "call_fn'") (styled_mathrm "call_fn'")
  |> print_doc "link";
  numbered [ empty; styled_mathit "first"; empty; styled_mathit "second" ]
  |> print_doc "numbered-filtered";
  print_width "styled-width" (styled_mathit "x_escaped");
  let boundary =
    layout_group
      (concat
         [
           styled_mathit "1234";
           nest 2 (concat [ soft_space; styled_mathit "5678" ]);
         ])
  in
  print_resolved "flat-at-boundary" ~width:9 boundary;
  print_resolved "broken-over-boundary" ~width:8 boundary;
  grid [ Left; Center; Left ]
    [
      cells [ styled_mathit "f(x)"; fixed Equal; styled_mathit "first" ];
      row_gap;
      cells [ styled_mathit "f(long)"; fixed Equal; styled_mathit "second" ];
    ]
  |> print_doc "grid-row-gap"
