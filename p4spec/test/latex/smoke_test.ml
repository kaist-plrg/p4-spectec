open Domain
open Lang
open El
open Util.Source

let phrase it = it $ no_region
let id name = phrase name
let atom value = phrase value
let plaintyp value = phrase value
let deftyp value = phrase value
let exp value = phrase value
let arg value = phrase value
let prem value = phrase value
let rule value = phrase value
let def value = phrase value
let var name = exp (VarE (id name))

let anchors =
  Backend_latex.El.anchors
    ~func:(function
      | "check_condition_fn" -> Some "check_condition_fn" | _ -> None)
    ~rel:(fun _ -> None)

let fixture =
  [
    def
      (TypD
         ( id "Value_t",
           [ phrase "T_t" ],
           deftyp
             (VariantTD
                [
                  (PlainT (plaintyp BoolT), []);
                  ( NotationT (phrase (AtomT (atom (Atom.Keyword "UNKNOWN")))),
                    [] );
                ]),
           [] ));
    def SepD;
    def
      (RuleGroupD
         ( id "Eval_rel",
           id "core_group",
           [
             rule
               ( id "Eval_rel",
                 id "step_rule",
                 var "result_v",
                 [
                   prem (VarPr (id "x_v", plaintyp (NumT `NatT)));
                   prem (IfPr (var "condition_v"));
                 ] );
             rule
               ( id "Eval_rel",
                 id "rule_name_that_is_deliberately_long_for_pdf_smoke",
                 exp
                   (TupleE
                      [
                        exp
                          (CallE
                             ( id "assemble_fn",
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
                   prem
                     (IfPr
                        (exp
                           (InfixE
                              ( var "premiseleftoperandwithsubstantialpadding",
                                atom (Atom.Operator "<+>"),
                                var "premiserightoperandwithsubstantialpadding"
                              ))));
                 ] );
           ] ));
    def SepD;
    def
      (FuncDefD
         ( id "render_fn",
           [ phrase "T_t" ],
           [ arg (ExpA (var "x_v")) ],
           exp (TextE "escaped_#%&{}\\^~"),
           [ prem (RuleNotPr (id "Invalid_rel", var "invalid_v")) ] ));
    def
      (FuncDefD
         ( id "condition_layout_fn",
           [],
           [ arg (ExpA (var "input")) ],
           var "output",
           [
             prem
               (IfPr
                  (exp
                     (CallE
                        ( id "check_condition_fn",
                          [],
                          [
                            arg (ExpA (var "conditionargumentalphawithpadding"));
                            arg (ExpA (var "conditionargumentbetawithpadding"));
                            arg (ExpA (var "conditionargumentgammawithpadding"));
                          ] ))));
           ] ));
  ]

let () =
  print_endline "\\documentclass{article}";
  print_endline "\\usepackage{amsmath}";
  print_endline "\\usepackage{amssymb}";
  print_endline "\\usepackage{mathtools}";
  print_endline "\\usepackage{xcolor}";
  print_endline "\\setlength{\\fboxsep}{2pt}";
  print_endline "\\setlength{\\fboxrule}{0.4pt}";
  print_endline "\\newcommand{\\href}[2]{#2}";
  print_endline "\\begin{document}";
  print_endline "\\[";
  print_endline (Backend_latex.El.render_defs ~anchors fixture);
  print_endline "\\]";
  print_endline "\\end{document}"
