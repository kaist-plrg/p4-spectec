open Domain
open Lang
open El
open Util.Source

let at =
  {
    left = { file = "fixture.watsup"; line = 8; column = 2 };
    right = { file = "fixture.watsup"; line = 8; column = 5 };
  }

module Fixture = Test_common.El_fixture.Make (struct
  let at = at
end)

open Fixture

let nat = plaintyp (NumT `NatT)

let spec =
  [
    def (RelD (id "Check", nottyp (AtomT (atom (Atom.Keyword "CHECK"))), []));
    def (RelD (id "Step", nottyp (AtomT (atom (Atom.Keyword "STEP"))), []));
    def
      (RuleGroupD
         ( id "Check",
           id "value",
           [
             rule
               ( id "Check",
                 id "ok",
                 var "x",
                 [
                   phrase (RulePr (id "Step", var "x"));
                   phrase (RulePr (id "Missing_rel", var "x"));
                 ] );
             rule
               ( id "Check",
                 id "rule_name_that_is_deliberately_long_for_splice_layout",
                 exp
                   (TupleE
                      [
                        exp
                          (CallE
                             ( id "identity",
                               [],
                               [
                                 arg
                                   (ExpA
                                      (var "conclusionargumentalphawithpadding"));
                                 arg
                                   (ExpA
                                      (var "conclusionargumentbetawithpadding"));
                               ] ));
                        var "conclusiontailwithpadding";
                      ]),
                 [
                   phrase
                     (IfPr
                        (exp
                           (InfixE
                              ( var "premiseleftoperandwithsubstantialpadding",
                                atom (Atom.Operator "<+>"),
                                var "premiserightoperandwithsubstantialpadding"
                              ))));
                 ] );
           ] ));
    def (RuleGroupD (id "Check", id "empty", []));
    def (FuncDecD (id "identity", [], [ param (ExpP nat) ], nat, []));
    def
      (FuncDefD
         ( id "identity",
           [],
           [ arg (ExpA (var "x")) ],
           exp
             (TupleE
                [
                  exp (CallE (id "identity", [], [ arg (ExpA (var "x")) ]));
                  exp (CallE (id "missing_fn", [], []));
                ]),
           [] ));
    def
      (FuncDefD
         ( id "identity",
           [],
           [ arg (ExpA (exp (NumE (`DecOp, `Nat (Bigint.of_string "0"))))) ],
           exp (NumE (`DecOp, `Nat (Bigint.of_string "0"))),
           [] ));
    def
      (FuncDefD
         (id "layout_fn", [], [ arg (ExpA (var "short")) ], var "value", []));
    def
      (FuncDefD
         ( id "layout_fn",
           [],
           [ arg (ExpA (var "long")) ],
           var "body",
           [
             phrase
               (IfPr
                  (exp
                     (CallE
                        ( id "identity",
                          [],
                          [ arg (ExpA (var (String.make 55 'q'))) ] ))));
             phrase (IfPr (var "condition_two"));
           ] ));
    def
      (TableDefD
         ( id "truth",
           [
             row
               ( exp (NumE (`DecOp, `Nat (Bigint.of_string "0"))),
                 exp (BoolE false) );
           ] ));
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
    def
      (RuleGroupD
         ( id "Invalid",
           id "value",
           [ rule (id "Invalid", id "raw", exp (LatexE "unchecked"), []) ] ));
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
    Backend_splice.Ctx.
      {
        anchors_prose = Backend_splice.Ctx.empty_anchors;
        anchors_latex = latex;
      }
  in
  Backend_splice.Driver.init ~context spec [];
  print_endline (splice skeleton);
  print_endline "[missing-function]";
  print_endline (splice "${func-latex: absent}");
  Backend_splice.Driver.init invalid_spec [];
  try ignore (splice "${rulegroup-latex: Invalid/value}")
  with Backend_latex.El.LatexError (at, message) ->
    print_endline "[latex-error]";
    print_endline (Util.Error.string_of_error at message)
