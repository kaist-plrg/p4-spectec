open Backend_latex_test_support
open Doc

let () =
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
  Printf.printf "[bad-width]\n%b\n"
    (rejected "Layout.resolve: width must be positive" (fun () ->
         ignore (Layout.resolve ~width:0 (styled_mathit "x"))))
