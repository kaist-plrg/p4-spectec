open Domain
open Lang
open El
open Util.Source

let at =
  {
    left = { file = "fixture.watsup"; line = 8; column = 2 };
    right = { file = "fixture.watsup"; line = 8; column = 5 };
  }

let phrase it = it $ at
let id value = phrase value
let atom value = phrase value
let plaintyp value = phrase value
let nottyp value = phrase value
let exp value = phrase value
let arg value = phrase value
let param value = phrase value
let rule value = phrase value
let row value = phrase value
let def value = phrase value
let var value = exp (VarE (id value))
let nat = plaintyp (NumT `NatT)
let bool_pl = phrase Il.BoolT

let def_pl name =
  Pl.FuncDecD (id name, [], [], bool_pl, [], None)
  |> phrase |> Pl.Annot.no_hints

let spec_pl =
  [
    def_pl "overlap_fn";
    def_pl "title_only_fn";
    def_pl "full_only_fn";
    def_pl "apostrophe_fn'";
  ]

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
  {|SOURCE
${relation-title-source: Check}
RELATIONS
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

let count_substring needle text =
  let needle_length = String.length needle in
  let text_length = String.length text in
  let rec count index total =
    if index + needle_length > text_length then total
    else if String.equal (String.sub text index needle_length) needle then
      count (index + needle_length) (total + 1)
    else count (index + 1) total
  in
  count 0 0

let anchor_skeleton =
  {|
${func-title-prose: overlap_fn title_only_fn}
${func-title-prose: overlap_fn}
${func-prose: overlap_fn full_only_fn apostrophe_fn'}
|}

let print_anchor_counts label rendered =
  let print_count kind name needle =
    Printf.printf "%s %s %s: %d\n" label kind name
      (count_substring needle rendered)
  in
  List.iter
    (fun name ->
      print_count "anchors" name
        ("<span id=\"function_prose_" ^ name ^ "\"></span>"))
    [ "overlap_fn"; "title_only_fn"; "full_only_fn"; "apostrophe_fn'" ];
  List.iter
    (fun name ->
      print_count "presentations" name ("xref:function_prose_" ^ name ^ "["))
    [ "overlap_fn"; "title_only_fn"; "full_only_fn"; "apostrophe_fn'" ]

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
  let prose =
    Backend_splice.Ctx.
      {
        func =
          (fun name ->
            if List.mem name [ "overlap_fn"; "title_only_fn" ] then
              Some ("function_prose_" ^ name)
            else None);
        rel = (fun _ -> None);
      }
  in
  let context =
    Backend_splice.Ctx.{ anchors_prose = prose; anchors_latex = latex }
  in
  Backend_splice.Driver.init ~context spec [];
  print_endline (splice skeleton);
  print_endline "[missing-function]";
  print_endline (splice "${func-latex: absent}");
  print_endline "[anchor-ownership]";
  Backend_splice.Driver.init ~context spec spec_pl;
  splice anchor_skeleton |> print_anchor_counts "first";
  Backend_splice.Driver.init ~context spec spec_pl;
  splice anchor_skeleton |> print_anchor_counts "second";
  Backend_splice.Driver.init invalid_spec [];
  try ignore (splice "${rulegroup-latex: Invalid/value}")
  with El.Latex.LatexError (at, message) ->
    print_endline "[latex-error]";
    print_endline (Util.Error.string_of_error at message)
