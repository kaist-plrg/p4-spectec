open Domain
open Lang
open El
open Util.Source
open Backend_latex_test_support

let at =
  {
    left = { file = "fixture.el"; line = 7; column = 2 };
    right = { file = "fixture.el"; line = 7; column = 5 };
  }

module Fixture = Test_common.El_fixture.Make (struct
  let at = at
end)

open Fixture

let silent = exp (AtomE (atom (Atom.Tag "META")))

let print_typ name typ =
  print name (Serialize.to_string (Renderer.tex_of_typ typ))

let print_exp name exp =
  print name (Serialize.to_string (Renderer.tex_of_exp exp))

let print_doc name doc = doc |> Serialize.to_string |> print name

let print_doc_at_width name width doc =
  doc |> Layout.resolve ~width |> Serialize.to_string |> print name

let print_exp_at_width name width expression =
  expression |> Renderer.tex_of_exp |> Layout.resolve ~width
  |> Serialize.to_string |> print name

let linked_anchors : Renderer.anchors =
  {
    func = (function "linked_call" -> Some "linked_call_anchor" | _ -> None);
    rel =
      (function
      | "linked_relation" -> Some "linked_relation_anchor" | _ -> None);
  }

let print_linked_exp name expression =
  expression
  |> Renderer.tex_of_exp ~anchors:linked_anchors
  |> Serialize.to_string |> print name

let print_linked_exp_at_width name width expression =
  expression
  |> Renderer.tex_of_exp ~anchors:linked_anchors
  |> Layout.resolve ~width |> Serialize.to_string |> print name

let print_prem_at_width ?(anchors : Renderer.anchors option) name width premise
    =
  premise
  |> Renderer.tex_of_prem ?anchors
  |> Layout.resolve ~width |> Serialize.to_string |> print name

let print_error name exp =
  try print_exp name exp
  with Backend_latex.El.LatexError (region, message) ->
    print name (Util.Source.string_of_region region ^ ": " ^ message)

let () =
  print_typ "bool-type" (PlainT (plaintyp BoolT));
  print_typ "number-types"
    (PlainT
       (plaintyp
          (TupleT
             [ plaintyp (NumT `NatT); plaintyp (NumT `IntT); plaintyp TextT ])));
  print_typ "type-application"
    (PlainT
       (plaintyp (VarT (id "Box_t", [ plaintyp BoolT; plaintyp (NumT `NatT) ]))));
  print_typ "type-iteration"
    (PlainT (plaintyp (IterT (plaintyp (VarT (id "item_t", [])), List))));
  print_typ "empty-sequence-type" (NotationT (nottyp (SeqT [])));
  print_typ "singleton-sequence-type"
    (NotationT (nottyp (SeqT [ PlainT (plaintyp (VarT (id "p", []))) ])));
  print_typ "multiple-sequence-type"
    (NotationT
       (nottyp
          (SeqT
             [
               PlainT (plaintyp (VarT (id "p", [])));
               PlainT (plaintyp (VarT (id "TC", [])));
               PlainT (plaintyp (VarT (id "x", [])));
             ])));
  print_typ "notation-type"
    (NotationT
       (nottyp
          (BrackT
             ( atom Atom.LAngle,
               NotationT
                 (nottyp
                    (SeqT
                       [
                         NotationT (nottyp (AtomT (atom (Atom.Keyword "CASE"))));
                         PlainT (plaintyp (VarT (id "x_t", [])));
                       ])),
               atom Atom.RAngle ))));
  print_typ "subscripted-arrow-type"
    (NotationT
       (nottyp
          (InfixT
             ( PlainT (plaintyp (VarT (id "x", []))),
               atom Atom.ArrowSub,
               NotationT
                 (nottyp
                    (SeqT
                       [
                         PlainT (plaintyp (VarT (id "n", [])));
                         PlainT (plaintyp (VarT (id "y", [])));
                         PlainT (plaintyp (VarT (id "z", [])));
                       ])) ))));
  print_typ "singleton-arrow-sub-type"
    (NotationT
       (nottyp
          (InfixT
             ( PlainT (plaintyp (VarT (id "x", []))),
               atom Atom.ArrowSub,
               PlainT (plaintyp (VarT (id "n", []))) ))));
  print_typ "singleton-double-arrow-sub-type"
    (NotationT
       (nottyp
          (InfixT
             ( PlainT (plaintyp (VarT (id "p", []))),
               atom Atom.DoubleArrowSub,
               PlainT (plaintyp (VarT (id "k", []))) ))));
  let decimal = Bigint.of_string "123456789012345678901234567890" in
  let hexadecimal = Bigint.Hex.of_string "0x123456789abcdef0123456789abcdef" in
  print_exp "empty-sequence-expression" (exp (SeqE []));
  print_exp "singleton-sequence-expression" (exp (SeqE [ var "p" ]));
  print_exp "literals"
    (exp
       (SeqE
          [
            exp (BoolE true);
            exp (NumE (`DecOp, `Nat decimal));
            exp (NumE (`HexOp, `Nat hexadecimal));
            exp (TextE "a_#%&{}\\^~");
            exp EpsE;
          ]));
  print_exp "multiple-sequence-expression"
    (exp (SeqE [ var "p"; var "TC"; var "x" ]));
  print_exp "variable-subscripts"
    (exp
       (SeqE
          [ var "plain"; var "TC_0"; var "typeId_fresh_local"; var "_ignored" ]));
  print_exp "precedence-stronger-right"
    (exp (BinE (var "a", `AddOp, exp (BinE (var "b", `MulOp, var "c")))));
  print_exp "precedence-weaker-left"
    (exp (BinE (exp (BinE (var "a", `AddOp, var "b")), `MulOp, var "c")));
  print_exp "left-associative-right-child"
    (exp (BinE (var "a", `SubOp, exp (BinE (var "b", `SubOp, var "c")))));
  print_exp "right-associative-left-child"
    (exp (BinE (exp (BinE (var "a", `ImplOp, var "b")), `ImplOp, var "c")));
  print_exp "non-associative-child"
    (exp
       (InfixE
          ( exp (InfixE (var "a", atom Atom.Turnstile, var "b")),
            atom Atom.Turnstile,
            var "c" )));
  print_exp "explicit-parentheses" (exp (ParenE (var "x_under")));
  print_exp "power"
    (exp (BinE (exp (BinE (var "a", `PowOp, var "b")), `PowOp, var "c")));
  print_exp "collections"
    (exp
       (SeqE
          [
            exp (ListE [ var "x"; var "y" ]);
            exp (TupleE [ var "x"; var "y" ]);
            exp
              (StrE
                 [
                   (atom (Atom.Keyword "FIELD"), var "x");
                   (atom (Atom.Tag "META"), var "ignored_label");
                 ]);
          ]));
  print_exp "list-leading-silent-item"
    (exp (ListE [ silent; var "x"; var "y" ]));
  print_exp "tuple-middle-silent-item"
    (exp (TupleE [ var "x"; silent; var "y" ]));
  print_exp "call-trailing-silent-argument"
    (exp
       (CallE
          ( id "call_fn",
            [],
            [ arg (ExpA (var "x")); arg (ExpA (var "y")); arg (ExpA silent) ] )));
  print_exp "all-silent-list" (exp (ListE [ silent; silent ]));
  print_exp "list-operators"
    (exp (CatE (exp (ConsE (var "x", var "xs")), exp (ListE [ var "y" ]))));
  print_exp "access"
    (exp
       (UpdE
          ( exp
              (SliceE
                 ( exp (DotE (var "record", atom (Atom.Keyword "FIELD"))),
                   var "lo",
                   var "hi" )),
            path
              (DotP
                 (path (IdxP (path RootP, var "i")), atom (Atom.Keyword "NEXT"))),
            exp (LenE (var "value")) )));
  print_exp "membership-and-subtype"
    (exp
       (SubE
          (exp (MemE (var "x", var "xs")), plaintyp (VarT (id "element_t", [])))));
  print_exp "call"
    (exp
       (CallE
          ( id "lookup_fn",
            [ plaintyp (VarT (id "key_t", [])) ],
            [ arg (ExpA (var "key_v")); arg (DefA (id "fallback_fn")) ] )));
  print_exp "iteration" (exp (IterE (var "entry", Opt)));
  print_exp "bracket-notation"
    (exp (BrackE (atom Atom.LBrace, var "payload", atom Atom.RBrace)));
  print_exp "subscripted-arrows"
    (exp
       (SeqE
          [
            exp
              (InfixE
                 ( var "x",
                   atom Atom.ArrowSub,
                   exp (SeqE [ var "n"; var "y"; var "z" ]) ));
            exp
              (InfixE
                 ( var "p",
                   atom Atom.DoubleArrowSub,
                   exp (SeqE [ var "k"; var "q" ]) ));
          ]));
  print_exp "singleton-arrow-sub-expression"
    (exp (InfixE (var "x", atom Atom.ArrowSub, var "n")));
  print_exp "singleton-double-arrow-sub-expression"
    (exp (InfixE (var "p", atom Atom.DoubleArrowSub, var "k")));
  print_exp "silent-tag"
    (exp
       (SeqE [ var "left"; exp (AtomE (atom (Atom.Tag "META"))); var "right" ]));
  let silent_left_infix =
    exp (InfixE (silent, atom (Atom.Operator "<+>"), var "rightvisible"))
  in
  print_exp "silent-left-infix-flat" silent_left_infix;
  print_exp_at_width "silent-left-infix-constrained" 8 silent_left_infix;
  let silent_operator_infix =
    exp (InfixE (var "leftvisible", atom (Atom.Tag "META"), var "rightvisible"))
  in
  print_exp "silent-operator-infix-flat" silent_operator_infix;
  print_exp_at_width "silent-operator-infix-constrained" 8 silent_operator_infix;
  let long_sequence =
    exp
      (SeqE [ var "aaaaaaaa"; var "bbbbbbbb"; var "cccccccc"; var "dddddddd" ])
  in
  print_exp "long-sequence-flat" long_sequence;
  print_exp_at_width "long-sequence-broken" 17 long_sequence;
  let arithmetic =
    exp (BinE (var "arithmetic_alpha", `AddOp, var "arithmetic_beta"))
  in
  print_exp "arithmetic-flat" arithmetic;
  print_exp_at_width "arithmetic-broken" 24 arithmetic;
  let comparison =
    exp (CmpE (var "comparison_alpha", `LeOp, var "comparison_beta"))
  in
  print_exp "comparison-flat" comparison;
  print_exp_at_width "comparison-broken" 24 comparison;
  let generic_infix =
    exp
      (InfixE
         (var "operator_alpha", atom (Atom.Operator "<+>"), var "operator_beta"))
  in
  print_exp "generic-infix-flat" generic_infix;
  print_exp_at_width "generic-infix-broken" 24 generic_infix;
  let turnstile_infix =
    exp
      (InfixE (var "turnstile_alpha", atom Atom.Turnstile, var "turnstile_beta"))
  in
  print_exp "turnstile-infix-flat" turnstile_infix;
  print_exp_at_width "turnstile-infix-broken" 24 turnstile_infix;
  let long_call =
    exp
      (CallE
         ( id "assemble_parts",
           [],
           [
             arg (ExpA (var "argument_alpha"));
             arg (ExpA (var "argument_beta"));
             arg (ExpA (var "argument_gamma"));
           ] ))
  in
  print_exp "long-call-flat" long_call;
  print_exp_at_width "long-call-broken" 24 long_call;
  let long_tuple =
    exp (TupleE [ var "tuple_alpha"; var "tuple_beta"; var "tuple_gamma" ])
  in
  print_exp "long-tuple-flat" long_tuple;
  print_exp_at_width "long-tuple-broken" 24 long_tuple;
  let long_list =
    exp (ListE [ var "list_alpha"; var "list_beta"; var "list_gamma" ])
  in
  print_exp "long-list-flat" long_list;
  print_exp_at_width "long-list-broken" 24 long_list;
  let long_struct =
    exp
      (StrE
         [
           (atom (Atom.Keyword "LABEL_ALPHA"), var "value_alpha");
           (atom (Atom.Keyword "LABEL_BETA"), var "value_beta");
           (atom (Atom.Keyword "LABEL_GAMMA"), var "value_gamma");
         ])
  in
  print_exp "long-struct-flat" long_struct;
  print_exp_at_width "long-struct-broken" 24 long_struct;
  let nested_binary =
    exp
      (BinE
         ( exp
             (CallE
                ( id "combine_parts",
                  [],
                  [
                    arg (ExpA (var "argument_alpha"));
                    arg (ExpA (var "argument_beta"));
                    arg (ExpA (var "argument_gamma"));
                  ] )),
           `AddOp,
           var "result_tail" ))
  in
  print_exp "nested-binary-flat" nested_binary;
  print_exp_at_width "nested-binary-broken" 28 nested_binary;
  let linked_infix =
    exp
      (InfixE
         ( exp
             (CallE
                ( id "linked_call",
                  [],
                  [
                    arg (ExpA (var "linked_alpha"));
                    arg (ExpA (var "linked_beta"));
                    arg (ExpA (var "linked_gamma"));
                  ] )),
           atom (Atom.Operator "<+>"),
           var "linked_result" ))
  in
  print_linked_exp "linked-infix-flat" linked_infix;
  print_linked_exp_at_width "linked-infix-broken" 28 linked_infix;
  let linked_premise =
    prem
      (RulePr
         ( id "linked_relation",
           exp
             (InfixE
                ( var "premise_alpha",
                  atom (Atom.Operator "<+>"),
                  exp
                    (CallE
                       ( id "linked_call",
                         [],
                         [
                           arg (ExpA (var "linked_alpha"));
                           arg (ExpA (var "linked_beta"));
                           arg (ExpA (var "linked_gamma"));
                         ] )) )) ))
  in
  print_prem_at_width "linked-premise-unlinked-broken" 28 linked_premise;
  print_prem_at_width ~anchors:linked_anchors "linked-premise-linked-broken" 28
    linked_premise;
  let subtype = exp (SubE (var "subtypeleft", named_type "subtyperight")) in
  let subtype = Renderer.tex_of_exp subtype in
  print_doc "direct-subtype-flat" subtype;
  print_doc_at_width "direct-subtype-constrained" 20 subtype;
  let type_arguments =
    plaintyp
      (VarT
         ( id "container",
           [
             named_type "typealpha";
             named_type "typebeta";
             named_type "typegamma";
           ] ))
    |> Renderer.tex_of_plaintyp
  in
  print_doc "direct-type-arguments-flat" type_arguments;
  print_doc_at_width "direct-type-arguments-constrained" 24 type_arguments;
  let tuple_type =
    plaintyp
      (TupleT
         [
           named_type "tuplealpha";
           named_type "tuplebeta";
           named_type "tuplegamma";
         ])
    |> Renderer.tex_of_plaintyp
  in
  print_doc "direct-tuple-type-flat" tuple_type;
  print_doc_at_width "direct-tuple-type-constrained" 18 tuple_type;
  let struct_type =
    deftyp
      (StructTD
         [
           (atom (Atom.Keyword "FIRST"), named_type "fieldtypealpha", []);
           (atom (Atom.Keyword "SECOND"), named_type "fieldtypebeta", []);
           (atom (Atom.Keyword "THIRD"), named_type "fieldtypegamma", []);
         ])
    |> Renderer.tex_of_deftyp
  in
  print_doc "direct-struct-type-flat" struct_type;
  print_doc_at_width "direct-struct-type-constrained" 24 struct_type;
  let render_args names =
    names
    |> List.map (fun name -> arg (ExpA (var name)))
    |> Renderer.tex_of_args
  in
  let args_l = render_args [ "leftaaaa"; "leftbbbb"; "leftcccc" ] in
  let args_r = render_args [ "rightaaa"; "rightbbb"; "rightccc" ] in
  print_doc "direct-render-args-flat" args_l;
  print_doc_at_width "direct-render-args-grid-constrained" 27
    (Doc.grid
       [ Doc.Right; Doc.Center; Doc.Left ]
       [
         Doc.cells [ args_l; Doc.fixed Equal; Doc.styled_mathsf "x" ];
         Doc.cells [ Doc.styled_mathsf "y"; Doc.fixed Equal; args_r ];
       ]);
  let params =
    [
      param (ExpP (named_type "paramalpha"));
      param (ExpP (named_type "parambeta"));
      param (ExpP (named_type "paramgamma"));
    ]
    |> Renderer.tex_of_params
  in
  print_doc "direct-render-params-flat" params;
  print_doc_at_width "direct-render-params-constrained" 18 params;
  print_error "hole-error" (exp (HoleE `Next));
  print_error "fuse-error" (exp (FuseE (var "x", var "y")));
  print_error "unparen-error" (exp (UnparenE (var "x")));
  print_error "latex-error" (exp (LatexE "x_1"))
