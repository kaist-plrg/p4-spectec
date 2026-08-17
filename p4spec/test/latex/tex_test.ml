open Domain
open Lang
open El
module Renderer = Backend_latex__El_latex__Renderer
module Doc = Backend_latex__El_latex__Tex__Doc
module Link = Backend_latex__El_latex__Tex__Link
module Width = Backend_latex__El_latex__Tex__Width
module Layout = Backend_latex__El_latex__Tex__Layout
module Serialize = Backend_latex__El_latex__Tex__Serialize

let print_doc name doc =
  Printf.printf "[%s]\n%s\n" name (Serialize.to_string doc)

let print_width name doc = Printf.printf "[%s]\n%d\n" name (Width.flat doc)

let print_resolved name ~width doc =
  doc |> Layout.resolve ~width |> print_doc name

let list_init length f =
  let rec go index docs =
    if index = length then List.rev docs else go (index + 1) (f index :: docs)
  in
  go 0 []

let rejected message thunk =
  try
    thunk ();
    false
  with Backend_latex.El.LatexError (at, actual) ->
    at = Util.Source.no_region && String.equal message actual

let print_atom atom =
  match Renderer.render_atom atom with
  | Renderer.EmptyAtom -> "<empty>"
  | Renderer.PlainAtom doc -> Serialize.to_string doc
  | Renderer.SubscriptedAtom doc -> "<sub>" ^ Serialize.to_string doc

let print_binop binop =
  match Renderer.render_binop binop with
  | Renderer.InfixBinop doc -> Serialize.to_string doc
  | Renderer.ExponentBinop -> "<superscript>"

let braces_balanced text =
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

let () =
  let open Doc in
  concat_spaced
    [ styled_mathit "x_i"; fixed Equal; styled_texttt "#$%&_{}\\^~" ]
  |> print_doc "escaping";
  styled_mathit "#$%&_{}\\^~" |> print_doc "math-escaping";
  badge "Rule_$#%&{}\\^~" |> print_doc "badge";
  badge "" |> print_doc "empty-badge";
  print_width "badge-width" (badge "Rule_label");
  link (Link.target_of_string "rule_label") (badge "Rule_label")
  |> print_doc "linked-badge";
  let badge_balanced =
    badge "Rule_$#%&{}\\^~" |> Serialize.to_string |> braces_balanced
  in
  Printf.printf "[badge-balanced]\n%b\n" badge_balanced;
  grid [ Right; Center; Left ]
    [
      cells
        [
          link (Link.target_of_string "badge") (badge "Rule_label");
          fixed Equal;
          styled_mathit "value";
        ];
      spanning
        (concat
           [
             quad;
             styled_text "if";
             thin_space;
             styled_mathit "condition_that_is_deliberately_wider_than_the_badge";
           ]);
    ]
  |> print_doc "badge-link-stripping";
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
  print_width "fill-width"
    (fill ~separator:thin_space [ styled_mathit "aa"; styled_mathit "bbb" ]);
  stacked [ styled_mathit "condition_1"; styled_mathit "condition_2" ]
  |> print_doc "stacked";
  stacked [] |> print_doc "empty-stacked";
  link (Link.target_of_string "call_fn'") (styled_mathrm "call_fn'")
  |> print_doc "link";
  link (Link.target_of_string "call_fn") empty |> print_doc "empty-link";
  Link.link_unowned_doc
    (Link.target_of_string "relation_target")
    (concat_spaced
       [
         styled_mathit "p";
         fixed Turnstile;
         link (Link.target_of_string "call_fn") (styled_mathrm "call_fn");
         delimited Paren (styled_mathit "x");
         fixed Equal;
         styled_mathit "y";
       ])
  |> print_doc "fallback-link";
  Link.link_unowned_doc (Link.target_of_string "relation_target") empty
  |> print_doc "empty-fallback-link";
  Link.link_unowned_doc
    (Link.target_of_string "relation_target")
    (concat_spaced [ styled_mathit "x"; fixed Equal; styled_mathit "y" ])
  |> print_doc "unlinked-fallback-link";
  numbered [ empty; styled_mathit "first"; empty; styled_mathit "second" ]
  |> print_doc "numbered-filtered";
  numbered [] |> print_doc "empty-numbered";
  numbered [ empty; empty ] |> print_doc "all-empty-numbered";
  print_width "numbered-width"
    (numbered [ styled_mathit "first"; styled_mathit "second" ]);
  list_init 10 (fun _ -> styled_mathit "x")
  |> numbered |> print_doc "numbered-ten";
  let wrapped_numbered =
    numbered
      [
        layout_group
          (concat
             [
               styled_mathit "alpha";
               nest 2 (concat [ soft_space; styled_mathit "beta" ]);
             ]);
        styled_mathit "tail";
      ]
  in
  print_resolved "numbered-continuation" ~width:10 wrapped_numbered;
  let linked_numbered =
    numbered
      [
        link (Link.target_of_string "first_premise") (styled_mathit "first");
        styled_mathit "second";
      ]
    |> Link.link_unowned_doc (Link.target_of_string "relation")
  in
  linked_numbered |> print_doc "numbered-links";
  numbered [ styled_mathit "first"; styled_mathit "second" ]
  |> Link.link_unowned_doc (Link.target_of_string "relation")
  |> print_doc "numbered-unlinked-fallback";
  displaystyle
    (concat
       [
         styled_mathit "before";
         space;
         group (numbered [ styled_mathit "first"; styled_mathit "second" ]);
         space;
         styled_mathit "after";
       ])
  |> Link.link_unowned_doc (Link.target_of_string "relation")
  |> print_doc "numbered-nested-fallback";
  let numbered_balanced =
    numbered [ styled_text "left_{}\\^~"; styled_mathit "right" ]
    |> Serialize.to_string |> braces_balanced
  in
  Printf.printf "[numbered-balanced]\n%b\n" numbered_balanced;
  print_width "styled-width" (styled_mathit "x_escaped");
  print_width "decimal-width"
    (decimal (Bigint.of_string "123456789012345678901234567890"));
  print_width "hex-width"
    (hexadecimal (Bigint.Hex.of_string "0x123456789abcdef0123456789abcdef"));
  print_width "spacing-width"
    (concat [ styled_mathit "x"; space; thin_space; quad; styled_mathit "y" ]);
  print_width "script-width"
    (subsup (styled_mathit "base")
       (styled_mathit "subscript")
       (styled_mathit "sup"));
  print_width "fraction-width"
    (fraction (styled_mathit "numerator") (styled_mathit "den"));
  print_width "link-width"
    (link (Link.target_of_string "target") (styled_mathit "linked_value"));
  let boundary =
    layout_group
      (concat
         [
           styled_mathit "1234";
           nest 2 (concat [ soft_space; styled_mathit "5678" ]);
         ])
  in
  print_resolved "flat-above-boundary" ~width:10 boundary;
  print_resolved "flat-at-boundary" ~width:9 boundary;
  print_resolved "broken-over-boundary" ~width:8 boundary;
  print_resolved "group-with-pending-suffix" ~width:8
    (concat
       [
         layout_group
           (concat [ styled_mathit "abc"; soft_space; styled_mathit "def" ]);
         styled_mathit "XYZ";
       ]);
  print_resolved "broken-suffix-line-awareness" ~width:9
    (layout_group
       (concat
          [
            layout_group
              (concat [ styled_mathit "aaa"; soft_space; styled_mathit "bbb" ]);
            soft_space;
            styled_mathit "cc";
          ]));
  let nested =
    layout_group
      (concat
         [
           styled_mathit "outer";
           nest 2
             (concat
                [
                  soft_space;
                  layout_group
                    (concat
                       [
                         styled_mathit "inner";
                         nest 2
                           (concat [ soft_space; styled_mathit "continuation" ]);
                       ]);
                ]);
         ])
  in
  print_resolved "nested-layout" ~width:12 nested;
  let nested_delimiter =
    layout_group
      (concat
         [
           styled_mathit "head";
           nest 2
             (concat
                [
                  soft_space;
                  delimited Paren
                    (layout_group
                       (concat
                          [
                            styled_mathit "abc"; soft_space; styled_mathit "def";
                          ]));
                ]);
         ])
  in
  print_resolved "nested-delimiter-column" ~width:9 nested_delimiter;
  let delimiter_boundary =
    delimited Paren
      (layout_group
         (concat [ styled_mathit "1234"; soft_space; styled_mathit "5678" ]))
  in
  print_resolved "delimiter-closing-boundary" ~width:10 delimiter_boundary;
  print_resolved "direct-delimiter-closing-boundary" ~width:10
    (layout_group delimiter_boundary);
  let subscript_base_boundary =
    subscript
      (layout_group
         (concat [ styled_mathit "12345"; soft_space; styled_mathit "6789" ]))
      (styled_mathit "xy")
  in
  print_resolved "subscript-base-reservation-boundary" ~width:10
    subscript_base_boundary;
  let superscript_half_width_boundary =
    superscript (styled_mathit "12345")
      (layout_group
         (concat [ styled_mathit "abcd"; soft_space; styled_mathit "efgh" ]))
  in
  print_resolved "direct-superscript-half-width-boundary" ~width:10
    (layout_group superscript_half_width_boundary);
  print_resolved "soft-comma-list" ~width:8
    (delimited Paren
       (layout_group_soft_comma_separated
          [ styled_mathit "alpha"; styled_mathit "beta"; styled_mathit "gamma" ]));
  let fill_three =
    fill ~separator:space
      [ styled_mathit "aaa"; styled_mathit "bbb"; styled_mathit "ccc" ]
  in
  print_resolved "fill-exact-fit" ~width:7
    (fill ~separator:space [ styled_mathit "aaa"; styled_mathit "bbb" ]);
  print_resolved "fill-one-over" ~width:6
    (fill ~separator:space [ styled_mathit "aaa"; styled_mathit "bbb" ]);
  print_resolved "fill-greedy-two-lines" ~width:7 fill_three;
  print_resolved "fill-greedy-three-lines" ~width:6 fill_three;
  print_resolved "fill-indented" ~width:7
    (fill ~indent:2 ~separator:space
       [ styled_mathit "aaa"; styled_mathit "bbb"; styled_mathit "ccc" ]);
  print_resolved "fill-pending-suffix" ~width:9
    (concat
       [
         fill ~separator:space [ styled_mathit "aaa"; styled_mathit "bbb" ];
         styled_mathit "XYZ";
       ]);
  let nested_fill_item =
    layout_group
      (concat [ styled_mathit "dddd"; soft_space; styled_mathit "eeee" ])
  in
  print_resolved "fill-nested-item" ~width:8
    (fill ~indent:2 ~separator:space
       [ styled_mathit "aaa"; nested_fill_item; styled_mathit "f" ]);
  let linked_fill =
    fill ~separator:space
      [
        link (Link.target_of_string "first") (styled_mathit "aaa");
        styled_mathit "bbb";
        styled_mathit "ccc";
      ]
    |> Link.link_unowned_doc (Link.target_of_string "relation")
  in
  print_resolved "fill-links" ~width:7 linked_fill;
  let nested_fill =
    displaystyle
      (concat
         [
           styled_mathit "before";
           space;
           fill ~separator:space [ styled_mathit "aaa"; styled_mathit "bbb" ];
           space;
           styled_mathit "after";
         ])
    |> Link.link_unowned_doc (Link.target_of_string "relation")
  in
  print_resolved "fill-nested-fallback" ~width:12 nested_fill;
  grid [ Left ]
    [
      cells
        [
          fill ~separator:space
            [
              link (Link.target_of_string "first") (styled_mathit "aaa");
              link (Link.target_of_string "second") (styled_mathit "bbb");
            ];
        ];
      spanning (styled_mathit "tail");
    ]
  |> print_doc "fill-grid-link-stripping";
  grid [ Right; Center; Left ]
    [
      cells [ styled_mathit "f(x)"; fixed Equal; styled_mathit "short" ];
      spanning
        (concat
           [ quad; styled_text "if"; thin_space; styled_mathit "condition" ]);
    ]
  |> print_doc "spanning-grid";
  grid [ Left; Center; Left ]
    [
      cells [ styled_mathit "f(x)"; fixed Equal; styled_mathit "first" ];
      row_gap;
      cells [ styled_mathit "f(long)"; fixed Equal; styled_mathit "second" ];
    ]
  |> print_doc "grid-row-gap";
  let linked_lhs =
    concat
      [
        styled_mathrm "call";
        delimited Paren
          (link (Link.target_of_string "lhs_arg") (styled_mathit "argument"));
      ]
  in
  grid [ Right; Center; Left ]
    [
      cells [ linked_lhs; fixed Equal; styled_mathit "result" ];
      spanning
        (concat
           [
             quad;
             styled_text "if";
             thin_space;
             link
               (Link.target_of_string "condition")
               (styled_mathit "condition");
           ]);
    ]
  |> print_doc "mathjax-spanning-grid";
  grid [ Right; Center; Left ]
    [
      spanning
        (concat
           [ quad; styled_text "if"; thin_space; styled_mathit "condition" ]);
      spanning (styled_mathit "a_much_longer_condition");
    ]
  |> print_doc "spanning-only-grid";
  grid [ Right; Center; Left ]
    [
      cells
        [
          link (Link.target_of_string "cell") (styled_mathit "x");
          fixed Equal;
          styled_mathit "y";
        ];
      spanning
        (concat
           [
             quad;
             styled_text "if";
             thin_space;
             link
               (Link.target_of_string "wide_condition")
               (styled_mathit
                  "condition_that_is_deliberately_wider_than_the_equation");
           ]);
    ]
  |> print_doc "mixed-grid-width-envelope";
  grid [ Left ]
    [
      cells [ styled_mathit "cell_first" ];
      spanning (styled_mathit "span_first");
      cells [ styled_mathit "cell_second" ];
      spanning (styled_mathit "span_second");
    ]
  |> print_doc "mixed-grid-order";
  let wide_cell prefix suffix =
    layout_group
      (concat
         [
           styled_mathit prefix;
           nest 2 (concat [ soft_space; styled_mathit suffix ]);
         ])
  in
  let complementary_cells =
    [
      [ wide_cell "aaaaa" "bbbbb"; fixed Equal; styled_mathit "x" ];
      [ styled_mathit "y"; fixed Equal; wide_cell "ccccc" "ddddd" ];
    ]
  in
  print_resolved "aligned-complementary-cell-budgets" ~width:19
    (aligned complementary_cells);
  print_resolved "grid-complementary-cell-budgets" ~width:19
    (grid [ Right; Center; Left ] (List.map cells complementary_cells));
  let stable_lhs =
    layout_group
      (concat
         [ styled_mathit "aaaaaaaaaa"; soft_cut; styled_mathit "bbbbbbbbbb" ])
  in
  let stable_rhs =
    layout_group
      (concat [ styled_mathit "ccccc"; soft_space; styled_mathit "ddddd" ])
  in
  print_resolved "grid-stable-column-recompute" ~width:27
    (grid [ Right; Center; Left ]
       [
         cells [ stable_lhs; fixed Equal; styled_mathit "x" ];
         cells [ styled_mathit "y"; fixed Equal; stable_rhs ];
       ]);
  let nested_rhs =
    layout_group
      (concat
         [
           styled_mathit "z";
           nest 2
             (concat
                [
                  soft_space;
                  layout_group
                    (concat
                       [
                         styled_mathit "cccc"; soft_space; styled_mathit "dddd";
                       ]);
                ]);
         ])
  in
  print_resolved "grid-nested-cell-budget" ~width:16
    (grid [ Right; Center; Left ]
       [ cells [ styled_mathit "lllll"; fixed Equal; nested_rhs ] ]);
  left_stack
    [
      styled_mathtt "Rule-name";
      fraction (styled_mathit "premise") (styled_mathit "result");
    ]
  |> print_doc "left-stack";
  left_stack [] |> print_doc "empty-left-stack";
  grid [] [] |> print_doc "empty-grid";
  Printf.printf "[bad-width]\n%b\n"
    (rejected "Layout.resolve: width must be positive" (fun () ->
         ignore (Layout.resolve ~width:0 (styled_mathit "x"))));
  Printf.printf "[bad-grid-row]\n%b\n"
    (rejected "Doc.grid: cell count does not match columns" (fun () ->
         ignore (grid [ Left; Right ] [ cells [ styled_mathit "x" ] ])));
  Printf.printf "[grid-missing-columns]\n%b\n"
    (rejected "Doc.grid: rows require columns" (fun () ->
         ignore (grid [] [ spanning (styled_mathit "x") ])));
  let linked_break =
    Link.link_unowned_doc
      (Link.target_of_string "relation")
      (layout_group
         (concat
            [
              styled_mathit "left";
              nest 2
                (concat
                   [
                     soft_space;
                     link
                       (Link.target_of_string "function")
                       (styled_mathrm "call");
                     soft_space;
                     styled_mathit "right";
                   ]);
            ]))
  in
  print_resolved "line-wise-link-fallback" ~width:10 linked_break;
  if
    not
      (rejected "invalid LaTeX link target" (fun () ->
           ignore (Link.target_of_string "bad#target")))
  then failwith "local targets must reject arbitrary URIs";
  [
    Renderer.tex_of_bool_type ();
    Renderer.tex_of_num_type `NatT;
    Renderer.tex_of_num_type `IntT;
    Renderer.tex_of_text_type ();
  ]
  |> Doc.concat_spaced |> print_doc "base-types";
  [ `NotOp; `PlusOp; `MinusOp ]
  |> List.map Renderer.tex_of_unop
  |> Doc.concat_spaced
  |> print_doc "unary-operators";
  [
    `AndOp;
    `OrOp;
    `ImplOp;
    `EquivOp;
    `AddOp;
    `SubOp;
    `MulOp;
    `DivOp;
    `ModOp;
    `PowOp;
  ]
  |> List.map print_binop |> String.concat " "
  |> Printf.printf "[binary-operators]\n%s\n";
  [ `EqOp; `NeOp; `LtOp; `GtOp; `LeOp; `GeOp ]
  |> List.map Renderer.tex_of_cmpop
  |> Doc.concat_spaced
  |> print_doc "comparison-operators";
  [
    Atom.Keyword "BOOL";
    Atom.Tag "META";
    Atom.Operator "<+>";
    Atom.Sub;
    Atom.Sup;
    Atom.Turnstile;
    Atom.Tilesturn;
    Atom.Arrow;
    Atom.ArrowSub;
    Atom.DoubleArrowSub;
    Atom.DoubleArrowLong;
    Atom.SqArrow;
    Atom.SqArrowStar;
    Atom.Dot;
    Atom.Dot2;
    Atom.Dot3;
    Atom.Semicolon;
    Atom.Colon;
    Atom.ColonEq;
    Atom.Tilde2;
    Atom.Backslash;
    Atom.LAngle;
    Atom.RAngle;
    Atom.LParen;
    Atom.RParen;
    Atom.LBrack;
    Atom.RBrack;
    Atom.LBrace;
    Atom.RBrace;
  ]
  |> List.map print_atom |> String.concat " | " |> print_endline
