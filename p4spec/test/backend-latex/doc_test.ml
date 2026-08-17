open Backend_latex_test_support
open Doc

let () =
  badge "" |> print_doc "empty-badge";
  superscript
    (delimited Paren
       (concat_spaced [ styled_mathit "x"; fixed Plus; styled_mathit "y" ]))
    (fixed Ast)
  |> print_doc "nesting";
  fraction (styled_mathit "p") (styled_mathit "q") |> print_doc "fraction";
  displaystyle (fraction (styled_mathit "p") (styled_mathit "q"))
  |> print_doc "display-fraction";
  aligned
    [
      [ styled_mathit "a"; fixed Equal; styled_mathit "b" ];
      [ styled_mathit "c"; fixed Equal; styled_mathit "d" ];
    ]
  |> print_doc "aligned";
  gathered [ line (styled_mathit "a"); gap; line (styled_mathit "b") ]
  |> print_doc "gathered";
  gathered
    [
      gap;
      gap;
      line empty;
      line (styled_mathit "a");
      gap;
      gap;
      line empty;
      line (styled_mathit "b");
      gap;
      gap;
    ]
  |> print_doc "gathered-normalized";
  concat [] |> print_doc "empty";
  concat_comma_separated [ styled_mathit "x"; empty; styled_mathit "y" ]
  |> print_doc "comma-separated";
  concat_comma_separated [] |> print_doc "empty-comma-separated";
  concat_comma_separated [ empty; empty ]
  |> print_doc "all-empty-comma-separated";
  concat_juxtaposed [ styled_mathit "p"; empty; styled_mathit "TC" ]
  |> print_doc "juxtaposed";
  fill ~separator:thin_space
    [ empty; styled_mathit "aa"; empty; styled_mathit "bb" ]
  |> print_doc "fill-flat-filtered";
  fill ~separator:thin_space [] |> print_doc "fill-empty";
  fill ~separator:thin_space [ empty; empty ] |> print_doc "fill-all-empty";
  fill ~separator:thin_space [ styled_mathit "only" ]
  |> print_doc "fill-singleton";
  Printf.printf "[fill-negative-indent]\n%b\n"
    (rejected "Doc.fill: indent must be non-negative" (fun () ->
         ignore (fill ~indent:(-1) ~separator:space [ styled_mathit "x" ])));
  Printf.printf "[nest-negative-indent]\n%b\n"
    (rejected "Doc.nest: indent must be non-negative" (fun () ->
         ignore (nest (-1) (styled_mathit "x"))));
  stacked [ styled_mathit "condition_1"; styled_mathit "condition_2" ]
  |> print_doc "stacked";
  stacked [] |> print_doc "empty-stacked";
  numbered [ empty; styled_mathit "first"; empty; styled_mathit "second" ]
  |> print_doc "numbered-filtered";
  numbered [] |> print_doc "empty-numbered";
  numbered [ empty; empty ] |> print_doc "all-empty-numbered";
  list_init 10 (fun _ -> styled_mathit "x")
  |> numbered |> print_doc "numbered-ten";
  left_stack
    [
      styled_mathtt "Rule-name";
      fraction (styled_mathit "premise") (styled_mathit "result");
    ]
  |> print_doc "left-stack";
  left_stack [] |> print_doc "empty-left-stack";
  grid [] [] |> print_doc "empty-grid";
  Printf.printf "[bad-grid-row]\n%b\n"
    (rejected "Doc.grid: cell count does not match columns" (fun () ->
         ignore (grid [ Left; Right ] [ cells [ styled_mathit "x" ] ])));
  Printf.printf "[grid-missing-columns]\n%b\n"
    (rejected "Doc.grid: rows require columns" (fun () ->
         ignore (grid [] [ spanning (styled_mathit "x") ])))
