open Domain
open Lang
open El
open Util.Source

let at =
  {
    left = { file = "fixture.watsup"; line = 8; column = 2 };
    right = { file = "fixture.watsup"; line = 8; column = 5 };
  }

let nat = NumT `NatT $ at

let spec =
  [
    RelD ("Check" $ at, AtomT (Atom.Keyword "CHECK" $ at) $ at, []) $ at;
    RelD ("Step" $ at, AtomT (Atom.Keyword "STEP" $ at) $ at, []) $ at;
    RuleGroupD
      ( "Check" $ at,
        "value" $ at,
        [
          ( "Check" $ at,
            "ok" $ at,
            VarE ("x" $ at) $ at,
            [
              RulePr ("Step" $ at, VarE ("x" $ at) $ at) $ at;
              RulePr ("Missing_rel" $ at, VarE ("x" $ at) $ at) $ at;
            ] )
          $ at;
          ( "Check" $ at,
            "rule_name_that_is_deliberately_long_for_splice_layout" $ at,
            TupleE
              [
                CallE
                  ( "identity" $ at,
                    [],
                    [
                      ExpA
                        (VarE ("conclusionargumentalphawithpadding" $ at) $ at)
                      $ at;
                      ExpA (VarE ("conclusionargumentbetawithpadding" $ at) $ at)
                      $ at;
                    ] )
                $ at;
                VarE ("conclusiontailwithpadding" $ at) $ at;
              ]
            $ at,
            [
              IfPr
                (InfixE
                   ( VarE ("premiseleftoperandwithsubstantialpadding" $ at) $ at,
                     Atom.Operator "<+>" $ at,
                     VarE ("premiserightoperandwithsubstantialpadding" $ at)
                     $ at )
                $ at)
              $ at;
            ] )
          $ at;
        ] )
    $ at;
    RuleGroupD ("Check" $ at, "empty" $ at, []) $ at;
    FuncDecD ("identity" $ at, [], [ ExpP nat $ at ], nat, []) $ at;
    FuncDefD
      ( "identity" $ at,
        [],
        [ ExpA (VarE ("x" $ at) $ at) $ at ],
        TupleE
          [
            CallE ("identity" $ at, [], [ ExpA (VarE ("x" $ at) $ at) $ at ])
            $ at;
            CallE ("missing_fn" $ at, [], []) $ at;
          ]
        $ at,
        [] )
    $ at;
    FuncDefD
      ( "identity" $ at,
        [],
        [ ExpA (NumE (`DecOp, `Nat (Bigint.of_string "0")) $ at) $ at ],
        NumE (`DecOp, `Nat (Bigint.of_string "0")) $ at,
        [] )
    $ at;
    FuncDefD
      ( "layout_fn" $ at,
        [],
        [ ExpA (VarE ("short" $ at) $ at) $ at ],
        VarE ("value" $ at) $ at,
        [] )
    $ at;
    FuncDefD
      ( "layout_fn" $ at,
        [],
        [ ExpA (VarE ("long" $ at) $ at) $ at ],
        VarE ("body" $ at) $ at,
        [
          IfPr
            (CallE
               ( "identity" $ at,
                 [],
                 [ ExpA (VarE (String.make 55 'q' $ at) $ at) $ at ] )
            $ at)
          $ at;
          IfPr (VarE ("condition_two" $ at) $ at) $ at;
        ] )
    $ at;
    TableDefD
      ( "truth" $ at,
        [
          (NumE (`DecOp, `Nat (Bigint.of_string "0")) $ at, BoolE false $ at)
          $ at;
        ] )
    $ at;
  ]

let skeleton =
  {|RELATIONS
${relation-title-latex: Check Step}
RULE
${rulegroup-latex: Check/value}
EMPTY RULES
${rulegroup-latex: Check/empty}
FUNCTION TITLE
${func-title-latex: identity}
FUNCTION
${func-latex: identity}
FUNCTION LAYOUT
${func-latex: layout_fn}
TABLE
${table-latex: truth}|}

let source content =
  Backend_splice.Source.{ file = "fixture.adoc"; s = content; i = 0 }

let splice content =
  Backend_splice.Driver.splice_string (source content) content

let invalid_spec =
  [
    RuleGroupD
      ( "Invalid" $ at,
        "value" $ at,
        [ ("Invalid" $ at, "raw" $ at, LatexE "unchecked" $ at, []) $ at ] )
    $ at;
  ]

let () =
  let latex =
    Backend_splice.Ctx.
      {
        func =
          (function "identity" -> Some "function_latex_identity" | _ -> None);
        rel =
          (function
          | "Check" -> Some "relation_latex_Check"
          | "Step" -> Some "relation_latex_Step"
          | _ -> None);
      }
  in
  let context =
    Backend_splice.Ctx.make ~anchors_prose:Backend_splice.Ctx.empty_anchors
      ~anchors_latex:latex
  in
  Backend_splice.Driver.init ~context spec [];
  print_endline (splice skeleton);
  print_endline "[missing-function]";
  print_endline (splice "${func-latex: absent}");
  Backend_splice.Driver.init invalid_spec [];
  try ignore (splice "${rulegroup-latex: Invalid/value}")
  with Backend_splice__Error.SpliceError error ->
    let at, message = Backend_splice.to_region_msg error in
    print_endline "[latex-error]";
    print_endline (Util.Error.string_of_error at message)
