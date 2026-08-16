open Domain
open Lang
open El
open Util.Source
module Renderer = Lang__El__Latex__Renderer
module Doc = Lang__El__Latex__Tex__Doc
module Width = Lang__El__Latex__Tex__Width

let at = no_region
let phrase it = it $ at
let id name = phrase name
let atom value = phrase value
let plaintyp value = phrase value
let nottyp value = phrase value
let deftyp value = phrase value
let exp value = phrase value
let param value = phrase value
let arg value = phrase value
let prem value = phrase value
let rule value = phrase value
let tablerow value = phrase value
let def value = phrase value
let var name = exp (VarE (id name))

let print name value =
  if String.equal value "" then Printf.printf "[%s]\n" name
  else Printf.printf "[%s]\n%s\n" name value

let print_def name definition = print name (El.Latex.render_def definition)

let bad_hint =
  { hintid = id "latex"; hintexp = exp (LatexE "must not be visited") }

let bool_type = plaintyp BoolT
let nat_type = plaintyp (NumT `NatT)
let text_type = plaintyp TextT
let variable_definition = def (VarD (id "x_v", nat_type, [ bad_hint ]))

let function_equation name argument body prems =
  def (FuncDefD (id name, [], [ arg (ExpA (var argument)) ], body, prems))

let render_function_equation_candidate name argument body prems =
  let lhs =
    Doc.concat
      [
        Doc.styled_mathrm name;
        Renderer.tex_of_tparams [];
        Renderer.tex_of_args [ arg (ExpA (var argument)) ];
      ]
  in
  let body = Renderer.tex_of_exp body in
  let prems = Renderer.texs_of_prems prems in
  let condition =
    match prems with
    | [] -> Doc.empty
    | [ prem ] -> Doc.concat_juxtaposed [ Doc.styled_text "if"; prem ]
    | prems -> Doc.concat_juxtaposed [ Doc.styled_text "if"; Doc.stacked prems ]
  in
  let rhs =
    if Doc.is_empty condition then body
    else Doc.concat_spaced [ body; Doc.quad; condition ]
  in
  Doc.aligned [ [ lhs; Doc.fixed Equal; rhs ] ]

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
  def
    (RelD
       ( id "Eval_rel",
         nottyp
           (InfixT
              ( PlainT (plaintyp (VarT (id "lhs_t", []))),
                atom Atom.Turnstile,
                PlainT (plaintyp (VarT (id "rhs_t", []))) )),
         [ bad_hint ] ))

let anchors =
  El.Latex.anchors
    ~func:(function "lookup_fn" -> Some "lookup_fn" | _ -> None)
    ~rel:(function "Eval_rel" -> Some "Eval_rel" | _ -> None)

let judgment = exp (InfixE (var "p", atom Atom.Turnstile, var "v"))

let linked_judgment =
  exp
    (InfixE
       ( var "p",
         atom Atom.Turnstile,
         exp (CallE (id "lookup_fn", [], [ arg (ExpA (var "x")) ])) ))

let long_operator_premise =
  prem
    (IfPr
       (exp
          (InfixE
             ( var "operatorpremiseleftoperandwithsubstantialpadding",
               atom (Atom.Operator "<+>"),
               var "operatorpremiserightoperandwithsubstantialpadding" ))))

let long_linked_premise =
  prem
    (RulePr
       ( id "Eval_rel",
         exp
           (InfixE
              ( var "judgmentinputwithsubstantialpadding",
                atom Atom.Turnstile,
                exp
                  (CallE
                     ( id "lookup_fn",
                       [],
                       [
                         arg (ExpA (var "lookupargumentalphawithpadding"));
                         arg (ExpA (var "lookupargumentbetawithpadding"));
                       ] )) )) ))

let long_rule_conclusion =
  exp
    (TupleE
       [
         exp
           (CallE
              ( id "lookup_fn",
                [],
                [
                  arg (ExpA (var "conclusionargumentalphawithpadding"));
                  arg (ExpA (var "conclusionargumentbetawithpadding"));
                ] ));
         exp
           (TupleE
              [
                var "nestedtupleleftwithpadding";
                var "nestedtuplerightwithpadding";
              ]);
       ])

let short_label_rule =
  def
    (RuleGroupD
       (id "R", id "short_group", [ rule (id "R", id "s", var "v", []) ]))

let long_label_rule =
  def
    (RuleGroupD
       ( id "Eval_rel",
         id "long_group",
         [
           rule
             ( id "Eval_rel",
               id "rule_name_that_is_deliberately_long_and_still_above",
               long_rule_conclusion,
               [ long_operator_premise; long_linked_premise ] );
         ] ))

let linked_definitions =
  [
    def
      (FuncDefD
         ( id "caller_fn",
           [],
           [],
           exp
             (TupleE
                [
                  exp
                    (CallE
                       ( id "lookup_fn",
                         [ plaintyp (VarT (id "T", [])) ],
                         [ arg (ExpA (var "x")) ] ));
                  exp (CallE (id "missing_fn", [], []));
                ]),
           [] ));
    def
      (RuleGroupD
         ( id "Result_rel",
           id "links",
           [
             rule
               ( id "Result_rel",
                 id "linked",
                 var "result",
                 [
                   prem (RulePr (id "Eval_rel", judgment));
                   prem (RuleNotPr (id "Eval_rel", judgment));
                   prem (RulePr (id "Eval_rel", linked_judgment));
                   prem (RuleNotPr (id "Eval_rel", linked_judgment));
                   prem (IterPr (prem (RulePr (id "Eval_rel", judgment)), List));
                   prem (RulePr (id "Missing_rel", linked_judgment));
                   prem (RulePr (id "Missing_rel", judgment));
                 ] );
           ] ));
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
  print_def "extern-syntax" (def (ExternSynD (id "Token_t", [ bad_hint ])));
  print_def "syntax-family"
    (def (SynD [ (id "Packet_t", [ phrase "T_t" ]); (id "Empty_t", []) ]));
  print_def "empty-syntax-family" (def (SynD []));
  print_def "plain-type-definition"
    (def (TypD (id "Flag_t", [], deftyp (PlainTD bool_type), [ bad_hint ])));
  print_def "struct-type-definition"
    (def
       (TypD
          ( id "Node_t",
            [ phrase "T_t" ],
            deftyp
              (StructTD
                 [
                   (atom (Atom.Keyword "HEAD"), bool_type, [ bad_hint ]);
                   ( atom (Atom.Keyword "TAIL"),
                     plaintyp (VarT (id "node_t", [])),
                     [] );
                 ]),
            [] )));
  print_def "variant-type-definition"
    (def
       (TypD
          ( id "Value_t",
            [],
            deftyp
              (VariantTD
                 [
                   (PlainT bool_type, [ bad_hint ]);
                   ( NotationT (nottyp (AtomT (atom (Atom.Keyword "UNKNOWN")))),
                     [] );
                 ]),
            [] )));
  print_def "variable" variable_definition;
  print_def "external-relation"
    (def
       (ExternRelD
          ( id "Matches_rel",
            nottyp
              (SeqT
                 [
                   PlainT (plaintyp (VarT (id "input_t", [])));
                   NotationT (nottyp (AtomT (atom (Atom.Keyword "MATCHES"))));
                   PlainT (plaintyp (VarT (id "pattern_t", [])));
                 ]),
            [ bad_hint ] )));
  print_def "relation" relation_definition;
  print_def "rules"
    (def
       (RuleGroupD
          ( id "Eval_rel",
            id "core_group",
            [
              rule
                ( id "Eval_rel",
                  id "s",
                  var "result_v",
                  [
                    prem (VarPr (id "x_v", nat_type));
                    prem (IfPr (var "condition_v"));
                  ] );
              rule
                ( id "Eval_rel",
                  id "rule_name_that_is_deliberately_long_in_a_group",
                  var "value_v",
                  [] );
              rule (id "Eval_rel", id "fallback_rule", var "fallback_v", []);
            ] )));
  print_def "short-label-rule" short_label_rule;
  assert_flat_width_exceeds 80 (Renderer.tex_of_prem long_operator_premise);
  print "long-label-rule" (El.Latex.render_defs ~anchors [ long_label_rule ]);
  print_def "single-rule"
    (def
       (RuleGroupD
          ( id "Check_rel",
            id "single_group",
            [
              rule
                ( id "Check_rel",
                  id "only_rule",
                  var "checked_v",
                  [ prem (RuleNotPr (id "Bad_rel", var "bad_v")) ] );
            ] )));
  print_def "negated-judgment"
    (def
       (RuleGroupD
          ( id "Check_rel",
            id "negated_group",
            [
              rule
                ( id "Check_rel",
                  id "negated_rule",
                  var "checked_v",
                  [
                    prem
                      (RuleNotPr
                         ( id "Bad_rel",
                           exp (InfixE (var "a", atom Atom.Turnstile, var "b"))
                         ));
                  ] );
            ] )));
  print_def "empty-rules"
    (def (RuleGroupD (id "Empty_rel", id "none_group", [])));
  print_def "external-function"
    (def
       (ExternDecD
          ( id "parse_fn",
            [ phrase "T_t" ],
            [ param (ExpP text_type) ],
            plaintyp (VarT (id "T_t", [])),
            [ bad_hint ] )));
  print_def "builtin-function"
    (def
       (BuiltinDecD (id "size_fn", [], [ param (ExpP text_type) ], nat_type, [])));
  print_def "table-declaration"
    (def
       (TableDecD (id "lookup_tbl", [ param (ExpP nat_type) ], bool_type, [])));
  print_def "function-declaration"
    (def
       (FuncDecD
          ( id "apply_fn",
            [ phrase "T_t" ],
            [
              param
                (DefP
                   (id "callback_fn", [], [ param (ExpP bool_type) ], text_type));
              param (ExpP bool_type);
            ],
            text_type,
            [] )));
  print_def "function-equation"
    (def
       (FuncDefD
          ( id "apply_fn",
            [ phrase "T_t" ],
            [ arg (ExpA (var "x_v")); arg (DefA (id "callback_fn")) ],
            var "result_v",
            [
              prem (IfPr (var "condition_v")); prem (VarPr (id "n_v", nat_type));
            ] )));
  print_def "function-equation-no-premises"
    (def
       (FuncDefD
          (id "identity_fn", [], [ arg (ExpA (var "x_v")) ], var "x_v", [])));
  print_def "function-equation-one-premise"
    (def
       (FuncDefD
          ( id "one_fn",
            [],
            [ arg (ExpA (var "x")) ],
            var "y",
            [ prem (IfPr (var "c1")) ] )));
  print_def "function-equation-many-premises"
    (def
       (FuncDefD
          ( id "many_fn",
            [],
            [ arg (ExpA (var "x")) ],
            var "y",
            [
              prem (IfPr (var "c1"));
              prem (IfPr (var "c2"));
              prem (IfPr (var "c3"));
            ] )));
  print_def "function-equation-silent-premise"
    (def
       (FuncDefD
          ( id "silent_fn",
            [],
            [ arg (ExpA (var "x")) ],
            var "y",
            [ prem (IfPr (exp (AtomE (atom (Atom.Tag "META"))))) ] )));
  print_def "over-budget-condition-premise"
    (function_equation "condition_fn" "x" (var "y")
       [ prem (IfPr (var (String.make 80 'p'))); prem (IfPr (var "tail")) ]);
  print_def "table-equations"
    (def
       (TableDefD
          ( id "lookup_tbl",
            [
              tablerow (var "zero_v", exp (BoolE false));
              tablerow (var "other_v", exp (BoolE true));
            ] )));
  print_def "empty-table" (def (TableDefD (id "empty_tbl", [])));
  print_def "separator" (def SepD);
  print "definitions"
    (El.Latex.render_defs
       [
         def SepD;
         variable_definition;
         def SepD;
         def SepD;
         relation_definition;
         def SepD;
       ]);
  print "left-aligned-function-clauses"
    (El.Latex.render_defs
       [
         function_equation "left_fn" "x" (var "first") [];
         function_equation "left_fn" "long_argument" (var "second") [];
       ]);
  print "aligned-function-equations"
    (let condition_at_80 = String.make 56 'c' in
     let condition_at_81 = String.make 57 'c' in
     let breakable_condition =
       exp
         (CallE
            ( id "lookup_fn",
              [],
              [
                arg (ExpA (var (String.make 22 'a')));
                arg (ExpA (var (String.make 20 'b')));
                arg (ExpA (var (String.make 20 'c')));
              ] ))
     in
     assert_flat_width 80
       (render_function_equation_candidate "cases_fn" "s" (var "v")
          [ prem (IfPr (var condition_at_80)) ]);
     assert_flat_width 81
       (render_function_equation_candidate "cases_fn" "s" (var "v")
          [ prem (IfPr (var condition_at_81)) ]);
     assert_flat_width 77 (Renderer.tex_of_exp breakable_condition);
     El.Latex.render_defs ~anchors
       [
         function_equation "cases_fn" "x" (var "x") [];
         function_equation "cases_fn" "s" (var "v")
           [ prem (IfPr (var condition_at_80)) ];
         function_equation "cases_fn" "s" (var "v")
           [ prem (IfPr (var condition_at_81)) ];
         function_equation "cases_fn" "m" (var "v")
           [
             prem (IfPr (var "c1"));
             prem (IfPr (var "c2"));
             prem (IfPr (var "c3"));
           ];
         function_equation "cases_fn" "l" (var "v")
           [ prem (IfPr breakable_condition); prem (IfPr (var "c2")) ];
         function_equation "cases_fn" "silent" (var "v")
           [ prem (IfPr (exp (AtomE (atom (Atom.Tag "META"))))) ];
         def SepD;
         function_equation "cases_fn" "after_sep" (var "after_sep") [];
         function_equation "other_fn" "other" (var "other") [];
         variable_definition;
         function_equation "other_fn" "after_var" (var "after_var") [];
       ]);
  print "empty-definitions" (El.Latex.render_defs []);
  print "only-separators" (El.Latex.render_defs [ def SepD; def SepD ]);
  print "linked-definitions" (El.Latex.render_defs ~anchors linked_definitions);
  let unlinked_definitions = El.Latex.render_defs linked_definitions in
  if contains unlinked_definitions "\\href" then
    failwith "default definition rendering must not emit links";
  print "unlinked-definitions" unlinked_definitions
