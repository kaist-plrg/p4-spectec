open Domain
open Lang
open El
open Util.Source
open Backend_latex_test_support

let at = no_region
let print = print_nonempty

let print_def name definition =
  print name (Backend_latex.El.render_def definition)

let bad_hint =
  { hintid = "latex" $ at; hintexp = LatexE "must not be visited" $ at }

let bool_type = BoolT $ at
let nat_type = NumT `NatT $ at
let text_type = TextT $ at
let variable_definition = VarD ("x_v" $ at, nat_type, [ bad_hint ]) $ at

let function_equation name argument body prems =
  FuncDefD
    (name $ at, [], [ ExpA (VarE (argument $ at) $ at) $ at ], body, prems)
  $ at

let assert_function_layout expected name argument body prems =
  let actual =
    Renderer.layout_func ~anchors:None (name $ at) []
      [ ExpA (VarE (argument $ at) $ at) $ at ]
      body prems
  in
  match (expected, actual) with
  | `OneRow, Renderer.OneRow _ | `ConditionBelow, Renderer.ConditionBelow _ ->
      ()
  | _ -> failwith "unexpected function-clause layout"

let assert_flat_width expected doc =
  let actual = Width.flat doc in
  if actual <> expected then
    failwith (Printf.sprintf "expected flat width %d, got %d" expected actual)

let assert_flat_width_exceeds limit doc =
  let actual = Width.flat doc in
  if actual <= limit then
    failwith
      (Printf.sprintf "expected flat width above %d, got %d" limit actual)

let relation_definition =
  RelD
    ( "Eval_rel" $ at,
      InfixT
        ( PlainT (VarT ("lhs_t" $ at, []) $ at),
          Atom.Turnstile $ at,
          PlainT (VarT ("rhs_t" $ at, []) $ at) )
      $ at,
      [ bad_hint ] )
  $ at

let anchors =
  Backend_latex.El.anchors
    ~func:(function "lookup_fn" -> Some "lookup_fn" | _ -> None)
    ~rel:(function "Eval_rel" -> Some "Eval_rel" | _ -> None)

let judgment =
  InfixE (VarE ("p" $ at) $ at, Atom.Turnstile $ at, VarE ("v" $ at) $ at) $ at

let linked_judgment =
  InfixE
    ( VarE ("p" $ at) $ at,
      Atom.Turnstile $ at,
      CallE ("lookup_fn" $ at, [], [ ExpA (VarE ("x" $ at) $ at) $ at ]) $ at )
  $ at

let long_operator_premise =
  IfPr
    (InfixE
       ( VarE ("operatorpremiseleftoperandwithsubstantialpadding" $ at) $ at,
         Atom.Operator "<+>" $ at,
         VarE ("operatorpremiserightoperandwithsubstantialpadding" $ at) $ at )
    $ at)
  $ at

let long_linked_premise =
  RulePr
    ( "Eval_rel" $ at,
      InfixE
        ( VarE ("judgmentinputwithsubstantialpadding" $ at) $ at,
          Atom.Turnstile $ at,
          CallE
            ( "lookup_fn" $ at,
              [],
              [
                ExpA (VarE ("lookupargumentalphawithpadding" $ at) $ at) $ at;
                ExpA (VarE ("lookupargumentbetawithpadding" $ at) $ at) $ at;
              ] )
          $ at )
      $ at )
  $ at

let long_rule_conclusion =
  TupleE
    [
      CallE
        ( "lookup_fn" $ at,
          [],
          [
            ExpA (VarE ("conclusionargumentalphawithpadding" $ at) $ at) $ at;
            ExpA (VarE ("conclusionargumentbetawithpadding" $ at) $ at) $ at;
          ] )
      $ at;
      TupleE
        [
          VarE ("nestedtupleleftwithpadding" $ at) $ at;
          VarE ("nestedtuplerightwithpadding" $ at) $ at;
        ]
      $ at;
    ]
  $ at

let short_label_rule =
  RuleGroupD
    ( "R" $ at,
      "short_group" $ at,
      [ ("R" $ at, "s" $ at, VarE ("v" $ at) $ at, []) $ at ] )
  $ at

let long_label_rule =
  RuleGroupD
    ( "Eval_rel" $ at,
      "long_group" $ at,
      [
        ( "Eval_rel" $ at,
          "rule_name_that_is_deliberately_long_and_still_above" $ at,
          long_rule_conclusion,
          [ long_operator_premise; long_linked_premise ] )
        $ at;
      ] )
  $ at

let linked_definitions =
  [
    FuncDefD
      ( "caller_fn" $ at,
        [],
        [],
        TupleE
          [
            CallE
              ( "lookup_fn" $ at,
                [ VarT ("T" $ at, []) $ at ],
                [ ExpA (VarE ("x" $ at) $ at) $ at ] )
            $ at;
            CallE ("missing_fn" $ at, [], []) $ at;
          ]
        $ at,
        [] )
    $ at;
    RuleGroupD
      ( "Result_rel" $ at,
        "links" $ at,
        [
          ( "Result_rel" $ at,
            "linked" $ at,
            VarE ("result" $ at) $ at,
            [
              RulePr ("Eval_rel" $ at, judgment) $ at;
              RuleNotPr ("Eval_rel" $ at, judgment) $ at;
              RulePr ("Eval_rel" $ at, linked_judgment) $ at;
              RuleNotPr ("Eval_rel" $ at, linked_judgment) $ at;
              IterPr (RulePr ("Eval_rel" $ at, judgment) $ at, List) $ at;
              RulePr ("Missing_rel" $ at, linked_judgment) $ at;
              RulePr ("Missing_rel" $ at, judgment) $ at;
            ] )
          $ at;
        ] )
    $ at;
  ]

let contains text substring =
  let text_length = String.length text in
  let substring_length = String.length substring in
  let rec at index =
    index + substring_length <= text_length
    &&
    if String.sub text index substring_length = substring then true
    else at (index + 1)
  in
  at 0

let () =
  print_def "extern-syntax" (ExternSynD ("Token_t" $ at, [ bad_hint ]) $ at);
  print_def "syntax-family"
    (SynD [ ("Packet_t" $ at, [ "T_t" $ at ]); ("Empty_t" $ at, []) ] $ at);
  print_def "empty-syntax-family" (SynD [] $ at);
  print_def "plain-type-definition"
    (TypD ("Flag_t" $ at, [], PlainTD bool_type $ at, [ bad_hint ]) $ at);
  print_def "struct-type-definition"
    (TypD
       ( "Node_t" $ at,
         [ "T_t" $ at ],
         StructTD
           [
             (Atom.Keyword "HEAD" $ at, bool_type, [ bad_hint ]);
             (Atom.Keyword "TAIL" $ at, VarT ("node_t" $ at, []) $ at, []);
           ]
         $ at,
         [] )
    $ at);
  print_def "variant-type-definition"
    (TypD
       ( "Value_t" $ at,
         [],
         VariantTD
           [
             (PlainT bool_type, [ bad_hint ]);
             (NotationT (AtomT (Atom.Keyword "UNKNOWN" $ at) $ at), []);
           ]
         $ at,
         [] )
    $ at);
  print_def "variable" variable_definition;
  print_def "external-relation"
    (ExternRelD
       ( "Matches_rel" $ at,
         SeqT
           [
             PlainT (VarT ("input_t" $ at, []) $ at);
             NotationT (AtomT (Atom.Keyword "MATCHES" $ at) $ at);
             PlainT (VarT ("pattern_t" $ at, []) $ at);
           ]
         $ at,
         [ bad_hint ] )
    $ at);
  print_def "relation" relation_definition;
  print_def "rules"
    (RuleGroupD
       ( "Eval_rel" $ at,
         "core_group" $ at,
         [
           ( "Eval_rel" $ at,
             "s" $ at,
             VarE ("result_v" $ at) $ at,
             [
               VarPr ("x_v" $ at, nat_type) $ at;
               IfPr (VarE ("condition_v" $ at) $ at) $ at;
             ] )
           $ at;
           ( "Eval_rel" $ at,
             "rule_name_that_is_deliberately_long_in_a_group" $ at,
             VarE ("value_v" $ at) $ at,
             [] )
           $ at;
           ( "Eval_rel" $ at,
             "fallback_rule" $ at,
             VarE ("fallback_v" $ at) $ at,
             [] )
           $ at;
         ] )
    $ at);
  print_def "short-label-rule" short_label_rule;
  assert_flat_width_exceeds 80 (Renderer.tex_of_prem long_operator_premise);
  print "long-label-rule"
    (Backend_latex.El.render_defs ~anchors [ long_label_rule ]);
  print_def "single-rule"
    (RuleGroupD
       ( "Check_rel" $ at,
         "single_group" $ at,
         [
           ( "Check_rel" $ at,
             "only_rule" $ at,
             VarE ("checked_v" $ at) $ at,
             [ RuleNotPr ("Bad_rel" $ at, VarE ("bad_v" $ at) $ at) $ at ] )
           $ at;
         ] )
    $ at);
  print_def "negated-judgment"
    (RuleGroupD
       ( "Check_rel" $ at,
         "negated_group" $ at,
         [
           ( "Check_rel" $ at,
             "negated_rule" $ at,
             VarE ("checked_v" $ at) $ at,
             [
               RuleNotPr
                 ( "Bad_rel" $ at,
                   InfixE
                     ( VarE ("a" $ at) $ at,
                       Atom.Turnstile $ at,
                       VarE ("b" $ at) $ at )
                   $ at )
               $ at;
             ] )
           $ at;
         ] )
    $ at);
  print_def "empty-rules"
    (RuleGroupD ("Empty_rel" $ at, "none_group" $ at, []) $ at);
  print_def "external-function"
    (ExternDecD
       ( "parse_fn" $ at,
         [ "T_t" $ at ],
         [ ExpP text_type $ at ],
         VarT ("T_t" $ at, []) $ at,
         [ bad_hint ] )
    $ at);
  print_def "builtin-function"
    (BuiltinDecD ("size_fn" $ at, [], [ ExpP text_type $ at ], nat_type, [])
    $ at);
  print_def "table-declaration"
    (TableDecD ("lookup_tbl" $ at, [ ExpP nat_type $ at ], bool_type, []) $ at);
  print_def "function-declaration"
    (FuncDecD
       ( "apply_fn" $ at,
         [ "T_t" $ at ],
         [
           DefP ("callback_fn" $ at, [], [ ExpP bool_type $ at ], text_type)
           $ at;
           ExpP bool_type $ at;
         ],
         text_type,
         [] )
    $ at);
  print_def "function-equation"
    (FuncDefD
       ( "apply_fn" $ at,
         [ "T_t" $ at ],
         [ ExpA (VarE ("x_v" $ at) $ at) $ at; DefA ("callback_fn" $ at) $ at ],
         VarE ("result_v" $ at) $ at,
         [
           IfPr (VarE ("condition_v" $ at) $ at) $ at;
           VarPr ("n_v" $ at, nat_type) $ at;
         ] )
    $ at);
  print_def "function-equation-no-premises"
    (FuncDefD
       ( "identity_fn" $ at,
         [],
         [ ExpA (VarE ("x_v" $ at) $ at) $ at ],
         VarE ("x_v" $ at) $ at,
         [] )
    $ at);
  print_def "function-equation-one-premise"
    (FuncDefD
       ( "one_fn" $ at,
         [],
         [ ExpA (VarE ("x" $ at) $ at) $ at ],
         VarE ("y" $ at) $ at,
         [ IfPr (VarE ("c1" $ at) $ at) $ at ] )
    $ at);
  print_def "function-equation-many-premises"
    (FuncDefD
       ( "many_fn" $ at,
         [],
         [ ExpA (VarE ("x" $ at) $ at) $ at ],
         VarE ("y" $ at) $ at,
         [
           IfPr (VarE ("c1" $ at) $ at) $ at;
           IfPr (VarE ("c2" $ at) $ at) $ at;
           IfPr (VarE ("c3" $ at) $ at) $ at;
         ] )
    $ at);
  print_def "function-equation-silent-premise"
    (FuncDefD
       ( "silent_fn" $ at,
         [],
         [ ExpA (VarE ("x" $ at) $ at) $ at ],
         VarE ("y" $ at) $ at,
         [ IfPr (AtomE (Atom.Tag "META" $ at) $ at) $ at ] )
    $ at);
  print_def "over-budget-condition-premise"
    (function_equation "condition_fn" "x"
       (VarE ("y" $ at) $ at)
       [
         IfPr (VarE (String.make 80 'p' $ at) $ at) $ at;
         IfPr (VarE ("tail" $ at) $ at) $ at;
       ]);
  print_def "table-equations"
    (TableDefD
       ( "lookup_tbl" $ at,
         [
           (VarE ("zero_v" $ at) $ at, BoolE false $ at) $ at;
           (VarE ("other_v" $ at) $ at, BoolE true $ at) $ at;
         ] )
    $ at);
  print_def "empty-table" (TableDefD ("empty_tbl" $ at, []) $ at);
  print_def "separator" (SepD $ at);
  print "definitions"
    (Backend_latex.El.render_defs
       [
         SepD $ at;
         variable_definition;
         SepD $ at;
         SepD $ at;
         relation_definition;
         SepD $ at;
       ]);
  print "left-aligned-function-clauses"
    (Backend_latex.El.render_defs
       [
         function_equation "left_fn" "x" (VarE ("first" $ at) $ at) [];
         function_equation "left_fn" "long_argument"
           (VarE ("second" $ at) $ at)
           [];
       ]);
  print "aligned-function-equations"
    (let condition_at_80 = String.make 56 'c' in
     let condition_at_81 = String.make 57 'c' in
     let breakable_condition =
       CallE
         ( "lookup_fn" $ at,
           [],
           [
             ExpA (VarE (String.make 22 'a' $ at) $ at) $ at;
             ExpA (VarE (String.make 20 'b' $ at) $ at) $ at;
             ExpA (VarE (String.make 20 'c' $ at) $ at) $ at;
           ] )
       $ at
     in
     assert_function_layout `OneRow "cases_fn" "s"
       (VarE ("v" $ at) $ at)
       [ IfPr (VarE (condition_at_80 $ at) $ at) $ at ];
     assert_function_layout `ConditionBelow "cases_fn" "s"
       (VarE ("v" $ at) $ at)
       [ IfPr (VarE (condition_at_81 $ at) $ at) $ at ];
     assert_flat_width 77 (Renderer.tex_of_exp breakable_condition);
     Backend_latex.El.render_defs ~anchors
       [
         function_equation "cases_fn" "x" (VarE ("x" $ at) $ at) [];
         function_equation "cases_fn" "s"
           (VarE ("v" $ at) $ at)
           [ IfPr (VarE (condition_at_80 $ at) $ at) $ at ];
         function_equation "cases_fn" "s"
           (VarE ("v" $ at) $ at)
           [ IfPr (VarE (condition_at_81 $ at) $ at) $ at ];
         function_equation "cases_fn" "m"
           (VarE ("v" $ at) $ at)
           [
             IfPr (VarE ("c1" $ at) $ at) $ at;
             IfPr (VarE ("c2" $ at) $ at) $ at;
             IfPr (VarE ("c3" $ at) $ at) $ at;
           ];
         function_equation "cases_fn" "l"
           (VarE ("v" $ at) $ at)
           [ IfPr breakable_condition $ at; IfPr (VarE ("c2" $ at) $ at) $ at ];
         function_equation "cases_fn" "silent"
           (VarE ("v" $ at) $ at)
           [ IfPr (AtomE (Atom.Tag "META" $ at) $ at) $ at ];
         SepD $ at;
         function_equation "cases_fn" "after_sep"
           (VarE ("after_sep" $ at) $ at)
           [];
         function_equation "other_fn" "other" (VarE ("other" $ at) $ at) [];
         variable_definition;
         function_equation "other_fn" "after_var"
           (VarE ("after_var" $ at) $ at)
           [];
       ]);
  print "empty-definitions" (Backend_latex.El.render_defs []);
  print "only-separators"
    (Backend_latex.El.render_defs [ SepD $ at; SepD $ at ]);
  print "linked-definitions"
    (Backend_latex.El.render_defs ~anchors linked_definitions);
  let unlinked_definitions = Backend_latex.El.render_defs linked_definitions in
  if contains unlinked_definitions "\\href" then
    failwith "default definition rendering must not emit links";
  print "unlinked-definitions" unlinked_definitions
