open Backend_latex_test_support
open Doc

let () =
  link (Link.target_of_string "rule_label") (badge "Rule_label")
  |> print_doc "linked-badge";
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
  then failwith "local targets must reject arbitrary URIs"
