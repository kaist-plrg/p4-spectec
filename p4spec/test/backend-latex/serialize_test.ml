open Backend_latex_test_support
open Doc

let () =
  concat_spaced
    [ styled_mathit "x_i"; fixed Equal; styled_texttt "#$%&_{}\\^~" ]
  |> print_doc "escaping";
  styled_mathit "#$%&_{}\\^~" |> print_doc "math-escaping";
  badge "Rule_$#%&{}\\^~" |> print_doc "badge";
  let badge_balanced =
    badge "Rule_$#%&{}\\^~" |> Serialize.to_string |> braces_balanced
  in
  Printf.printf "[badge-balanced]\n%b\n" badge_balanced;
  let balanced =
    superscript
      (delimited Brace
         (fraction
            (styled_text "left_{value}")
            (delimited Bracket (styled_mathit "right^value"))))
      (subscript (styled_mathit "n") (fixed Ast))
    |> Serialize.to_string |> braces_balanced
  in
  Printf.printf "[balanced]\n%b\n" balanced;
  let numbered_balanced =
    numbered [ styled_text "left_{}\\^~"; styled_mathit "right" ]
    |> Serialize.to_string |> braces_balanced
  in
  Printf.printf "[numbered-balanced]\n%b\n" numbered_balanced
