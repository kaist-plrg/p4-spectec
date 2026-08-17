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

let silent = AtomE (Atom.Tag "META" $ at) $ at

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
  with Backend_latex.El.LatexError error ->
    let region, message = Backend_latex.to_region_msg error in
    print name (Util.Source.string_of_region region ^ ": " ^ message)

let () =
  print_typ "bool-type" (PlainT (BoolT $ at));
  print_typ "number-types"
    (PlainT (TupleT [ NumT `NatT $ at; NumT `IntT $ at; TextT $ at ] $ at));
  print_typ "type-application"
    (PlainT (VarT ("Box_t" $ at, [ BoolT $ at; NumT `NatT $ at ]) $ at));
  print_typ "type-iteration"
    (PlainT (IterT (VarT ("item_t" $ at, []) $ at, List) $ at));
  print_typ "empty-sequence-type" (NotationT (SeqT [] $ at));
  print_typ "singleton-sequence-type"
    (NotationT (SeqT [ PlainT (VarT ("p" $ at, []) $ at) ] $ at));
  print_typ "multiple-sequence-type"
    (NotationT
       (SeqT
          [
            PlainT (VarT ("p" $ at, []) $ at);
            PlainT (VarT ("TC" $ at, []) $ at);
            PlainT (VarT ("x" $ at, []) $ at);
          ]
       $ at));
  print_typ "notation-type"
    (NotationT
       (BrackT
          ( Atom.LAngle $ at,
            NotationT
              (SeqT
                 [
                   NotationT (AtomT (Atom.Keyword "CASE" $ at) $ at);
                   PlainT (VarT ("x_t" $ at, []) $ at);
                 ]
              $ at),
            Atom.RAngle $ at )
       $ at));
  print_typ "subscripted-arrow-type"
    (NotationT
       (InfixT
          ( PlainT (VarT ("x" $ at, []) $ at),
            Atom.ArrowSub $ at,
            NotationT
              (SeqT
                 [
                   PlainT (VarT ("n" $ at, []) $ at);
                   PlainT (VarT ("y" $ at, []) $ at);
                   PlainT (VarT ("z" $ at, []) $ at);
                 ]
              $ at) )
       $ at));
  print_typ "singleton-arrow-sub-type"
    (NotationT
       (InfixT
          ( PlainT (VarT ("x" $ at, []) $ at),
            Atom.ArrowSub $ at,
            PlainT (VarT ("n" $ at, []) $ at) )
       $ at));
  print_typ "singleton-double-arrow-sub-type"
    (NotationT
       (InfixT
          ( PlainT (VarT ("p" $ at, []) $ at),
            Atom.DoubleArrowSub $ at,
            PlainT (VarT ("k" $ at, []) $ at) )
       $ at));
  let decimal = Bigint.of_string "123456789012345678901234567890" in
  let hexadecimal = Bigint.Hex.of_string "0x123456789abcdef0123456789abcdef" in
  print_exp "empty-sequence-expression" (SeqE [] $ at);
  print_exp "singleton-sequence-expression" (SeqE [ VarE ("p" $ at) $ at ] $ at);
  print_exp "literals"
    (SeqE
       [
         BoolE true $ at;
         NumE (`DecOp, `Nat decimal) $ at;
         NumE (`HexOp, `Nat hexadecimal) $ at;
         TextE "a_#%&{}\\^~" $ at;
         EpsE $ at;
       ]
    $ at);
  print_exp "multiple-sequence-expression"
    (SeqE [ VarE ("p" $ at) $ at; VarE ("TC" $ at) $ at; VarE ("x" $ at) $ at ]
    $ at);
  print_exp "variable-subscripts"
    (SeqE
       [
         VarE ("plain" $ at) $ at;
         VarE ("TC_0" $ at) $ at;
         VarE ("typeId_fresh_local" $ at) $ at;
         VarE ("_ignored" $ at) $ at;
       ]
    $ at);
  print_exp "precedence-stronger-right"
    (BinE
       ( VarE ("a" $ at) $ at,
         `AddOp,
         BinE (VarE ("b" $ at) $ at, `MulOp, VarE ("c" $ at) $ at) $ at )
    $ at);
  print_exp "precedence-weaker-left"
    (BinE
       ( BinE (VarE ("a" $ at) $ at, `AddOp, VarE ("b" $ at) $ at) $ at,
         `MulOp,
         VarE ("c" $ at) $ at )
    $ at);
  print_exp "left-associative-right-child"
    (BinE
       ( VarE ("a" $ at) $ at,
         `SubOp,
         BinE (VarE ("b" $ at) $ at, `SubOp, VarE ("c" $ at) $ at) $ at )
    $ at);
  print_exp "right-associative-left-child"
    (BinE
       ( BinE (VarE ("a" $ at) $ at, `ImplOp, VarE ("b" $ at) $ at) $ at,
         `ImplOp,
         VarE ("c" $ at) $ at )
    $ at);
  print_exp "non-associative-child"
    (InfixE
       ( InfixE (VarE ("a" $ at) $ at, Atom.Turnstile $ at, VarE ("b" $ at) $ at)
         $ at,
         Atom.Turnstile $ at,
         VarE ("c" $ at) $ at )
    $ at);
  print_exp "explicit-parentheses" (ParenE (VarE ("x_under" $ at) $ at) $ at);
  print_exp "power"
    (BinE
       ( BinE (VarE ("a" $ at) $ at, `PowOp, VarE ("b" $ at) $ at) $ at,
         `PowOp,
         VarE ("c" $ at) $ at )
    $ at);
  print_exp "collections"
    (SeqE
       [
         ListE [ VarE ("x" $ at) $ at; VarE ("y" $ at) $ at ] $ at;
         TupleE [ VarE ("x" $ at) $ at; VarE ("y" $ at) $ at ] $ at;
         StrE
           [
             (Atom.Keyword "FIELD" $ at, VarE ("x" $ at) $ at);
             (Atom.Tag "META" $ at, VarE ("ignored_label" $ at) $ at);
           ]
         $ at;
       ]
    $ at);
  print_exp "list-leading-silent-item"
    (ListE [ silent; VarE ("x" $ at) $ at; VarE ("y" $ at) $ at ] $ at);
  print_exp "tuple-middle-silent-item"
    (TupleE [ VarE ("x" $ at) $ at; silent; VarE ("y" $ at) $ at ] $ at);
  print_exp "call-trailing-silent-argument"
    (CallE
       ( "call_fn" $ at,
         [],
         [
           ExpA (VarE ("x" $ at) $ at) $ at;
           ExpA (VarE ("y" $ at) $ at) $ at;
           ExpA silent $ at;
         ] )
    $ at);
  print_exp "all-silent-list" (ListE [ silent; silent ] $ at);
  print_exp "list-operators"
    (CatE
       ( ConsE (VarE ("x" $ at) $ at, VarE ("xs" $ at) $ at) $ at,
         ListE [ VarE ("y" $ at) $ at ] $ at )
    $ at);
  print_exp "access"
    (UpdE
       ( SliceE
           ( DotE (VarE ("record" $ at) $ at, Atom.Keyword "FIELD" $ at) $ at,
             VarE ("lo" $ at) $ at,
             VarE ("hi" $ at) $ at )
         $ at,
         DotP
           ( IdxP (RootP $ at, VarE ("i" $ at) $ at) $ at,
             Atom.Keyword "NEXT" $ at )
         $ at,
         LenE (VarE ("value" $ at) $ at) $ at )
    $ at);
  print_exp "membership-and-subtype"
    (SubE
       ( MemE (VarE ("x" $ at) $ at, VarE ("xs" $ at) $ at) $ at,
         VarT ("element_t" $ at, []) $ at )
    $ at);
  print_exp "call"
    (CallE
       ( "lookup_fn" $ at,
         [ VarT ("key_t" $ at, []) $ at ],
         [
           ExpA (VarE ("key_v" $ at) $ at) $ at; DefA ("fallback_fn" $ at) $ at;
         ] )
    $ at);
  print_exp "iteration" (IterE (VarE ("entry" $ at) $ at, Opt) $ at);
  print_exp "bracket-notation"
    (BrackE (Atom.LBrace $ at, VarE ("payload" $ at) $ at, Atom.RBrace $ at)
    $ at);
  print_exp "subscripted-arrows"
    (SeqE
       [
         InfixE
           ( VarE ("x" $ at) $ at,
             Atom.ArrowSub $ at,
             SeqE
               [
                 VarE ("n" $ at) $ at;
                 VarE ("y" $ at) $ at;
                 VarE ("z" $ at) $ at;
               ]
             $ at )
         $ at;
         InfixE
           ( VarE ("p" $ at) $ at,
             Atom.DoubleArrowSub $ at,
             SeqE [ VarE ("k" $ at) $ at; VarE ("q" $ at) $ at ] $ at )
         $ at;
       ]
    $ at);
  print_exp "singleton-arrow-sub-expression"
    (InfixE (VarE ("x" $ at) $ at, Atom.ArrowSub $ at, VarE ("n" $ at) $ at)
    $ at);
  print_exp "singleton-double-arrow-sub-expression"
    (InfixE
       (VarE ("p" $ at) $ at, Atom.DoubleArrowSub $ at, VarE ("k" $ at) $ at)
    $ at);
  print_exp "silent-tag"
    (SeqE
       [
         VarE ("left" $ at) $ at;
         AtomE (Atom.Tag "META" $ at) $ at;
         VarE ("right" $ at) $ at;
       ]
    $ at);
  let silent_left_infix =
    InfixE (silent, Atom.Operator "<+>" $ at, VarE ("rightvisible" $ at) $ at)
    $ at
  in
  print_exp "silent-left-infix-flat" silent_left_infix;
  print_exp_at_width "silent-left-infix-constrained" 8 silent_left_infix;
  let silent_operator_infix =
    InfixE
      ( VarE ("leftvisible" $ at) $ at,
        Atom.Tag "META" $ at,
        VarE ("rightvisible" $ at) $ at )
    $ at
  in
  print_exp "silent-operator-infix-flat" silent_operator_infix;
  print_exp_at_width "silent-operator-infix-constrained" 8 silent_operator_infix;
  let long_sequence =
    SeqE
      [
        VarE ("aaaaaaaa" $ at) $ at;
        VarE ("bbbbbbbb" $ at) $ at;
        VarE ("cccccccc" $ at) $ at;
        VarE ("dddddddd" $ at) $ at;
      ]
    $ at
  in
  print_exp "long-sequence-flat" long_sequence;
  print_exp_at_width "long-sequence-broken" 17 long_sequence;
  let arithmetic =
    BinE
      ( VarE ("arithmetic_alpha" $ at) $ at,
        `AddOp,
        VarE ("arithmetic_beta" $ at) $ at )
    $ at
  in
  print_exp "arithmetic-flat" arithmetic;
  print_exp_at_width "arithmetic-broken" 24 arithmetic;
  let comparison =
    CmpE
      ( VarE ("comparison_alpha" $ at) $ at,
        `LeOp,
        VarE ("comparison_beta" $ at) $ at )
    $ at
  in
  print_exp "comparison-flat" comparison;
  print_exp_at_width "comparison-broken" 24 comparison;
  let generic_infix =
    InfixE
      ( VarE ("operator_alpha" $ at) $ at,
        Atom.Operator "<+>" $ at,
        VarE ("operator_beta" $ at) $ at )
    $ at
  in
  print_exp "generic-infix-flat" generic_infix;
  print_exp_at_width "generic-infix-broken" 24 generic_infix;
  let turnstile_infix =
    InfixE
      ( VarE ("turnstile_alpha" $ at) $ at,
        Atom.Turnstile $ at,
        VarE ("turnstile_beta" $ at) $ at )
    $ at
  in
  print_exp "turnstile-infix-flat" turnstile_infix;
  print_exp_at_width "turnstile-infix-broken" 24 turnstile_infix;
  let long_call =
    CallE
      ( "assemble_parts" $ at,
        [],
        [
          ExpA (VarE ("argument_alpha" $ at) $ at) $ at;
          ExpA (VarE ("argument_beta" $ at) $ at) $ at;
          ExpA (VarE ("argument_gamma" $ at) $ at) $ at;
        ] )
    $ at
  in
  print_exp "long-call-flat" long_call;
  print_exp_at_width "long-call-broken" 24 long_call;
  let long_tuple =
    TupleE
      [
        VarE ("tuple_alpha" $ at) $ at;
        VarE ("tuple_beta" $ at) $ at;
        VarE ("tuple_gamma" $ at) $ at;
      ]
    $ at
  in
  print_exp "long-tuple-flat" long_tuple;
  print_exp_at_width "long-tuple-broken" 24 long_tuple;
  let long_list =
    ListE
      [
        VarE ("list_alpha" $ at) $ at;
        VarE ("list_beta" $ at) $ at;
        VarE ("list_gamma" $ at) $ at;
      ]
    $ at
  in
  print_exp "long-list-flat" long_list;
  print_exp_at_width "long-list-broken" 24 long_list;
  let long_struct =
    StrE
      [
        (Atom.Keyword "LABEL_ALPHA" $ at, VarE ("value_alpha" $ at) $ at);
        (Atom.Keyword "LABEL_BETA" $ at, VarE ("value_beta" $ at) $ at);
        (Atom.Keyword "LABEL_GAMMA" $ at, VarE ("value_gamma" $ at) $ at);
      ]
    $ at
  in
  print_exp "long-struct-flat" long_struct;
  print_exp_at_width "long-struct-broken" 24 long_struct;
  let nested_binary =
    BinE
      ( CallE
          ( "combine_parts" $ at,
            [],
            [
              ExpA (VarE ("argument_alpha" $ at) $ at) $ at;
              ExpA (VarE ("argument_beta" $ at) $ at) $ at;
              ExpA (VarE ("argument_gamma" $ at) $ at) $ at;
            ] )
        $ at,
        `AddOp,
        VarE ("result_tail" $ at) $ at )
    $ at
  in
  print_exp "nested-binary-flat" nested_binary;
  print_exp_at_width "nested-binary-broken" 28 nested_binary;
  let linked_infix =
    InfixE
      ( CallE
          ( "linked_call" $ at,
            [],
            [
              ExpA (VarE ("linked_alpha" $ at) $ at) $ at;
              ExpA (VarE ("linked_beta" $ at) $ at) $ at;
              ExpA (VarE ("linked_gamma" $ at) $ at) $ at;
            ] )
        $ at,
        Atom.Operator "<+>" $ at,
        VarE ("linked_result" $ at) $ at )
    $ at
  in
  print_linked_exp "linked-infix-flat" linked_infix;
  print_linked_exp_at_width "linked-infix-broken" 28 linked_infix;
  let linked_premise =
    RulePr
      ( "linked_relation" $ at,
        InfixE
          ( VarE ("premise_alpha" $ at) $ at,
            Atom.Operator "<+>" $ at,
            CallE
              ( "linked_call" $ at,
                [],
                [
                  ExpA (VarE ("linked_alpha" $ at) $ at) $ at;
                  ExpA (VarE ("linked_beta" $ at) $ at) $ at;
                  ExpA (VarE ("linked_gamma" $ at) $ at) $ at;
                ] )
            $ at )
        $ at )
    $ at
  in
  print_prem_at_width "linked-premise-unlinked-broken" 28 linked_premise;
  print_prem_at_width ~anchors:linked_anchors "linked-premise-linked-broken" 28
    linked_premise;
  let subtype =
    SubE (VarE ("subtypeleft" $ at) $ at, VarT ("subtyperight" $ at, []) $ at)
    $ at
  in
  let subtype = Renderer.tex_of_exp subtype in
  print_doc "direct-subtype-flat" subtype;
  print_doc_at_width "direct-subtype-constrained" 20 subtype;
  let type_arguments =
    VarT
      ( "container" $ at,
        [
          VarT ("typealpha" $ at, []) $ at;
          VarT ("typebeta" $ at, []) $ at;
          VarT ("typegamma" $ at, []) $ at;
        ] )
    $ at |> Renderer.tex_of_plaintyp
  in
  print_doc "direct-type-arguments-flat" type_arguments;
  print_doc_at_width "direct-type-arguments-constrained" 24 type_arguments;
  let tuple_type =
    TupleT
      [
        VarT ("tuplealpha" $ at, []) $ at;
        VarT ("tuplebeta" $ at, []) $ at;
        VarT ("tuplegamma" $ at, []) $ at;
      ]
    $ at |> Renderer.tex_of_plaintyp
  in
  print_doc "direct-tuple-type-flat" tuple_type;
  print_doc_at_width "direct-tuple-type-constrained" 18 tuple_type;
  let struct_type =
    StructTD
      [
        (Atom.Keyword "FIRST" $ at, VarT ("fieldtypealpha" $ at, []) $ at, []);
        (Atom.Keyword "SECOND" $ at, VarT ("fieldtypebeta" $ at, []) $ at, []);
        (Atom.Keyword "THIRD" $ at, VarT ("fieldtypegamma" $ at, []) $ at, []);
      ]
    $ at |> Renderer.tex_of_deftyp
  in
  print_doc "direct-struct-type-flat" struct_type;
  print_doc_at_width "direct-struct-type-constrained" 24 struct_type;
  let render_args names =
    names
    |> List.map (fun name -> ExpA (VarE (name $ at) $ at) $ at)
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
      ExpP (VarT ("paramalpha" $ at, []) $ at) $ at;
      ExpP (VarT ("parambeta" $ at, []) $ at) $ at;
      ExpP (VarT ("paramgamma" $ at, []) $ at) $ at;
    ]
    |> Renderer.tex_of_params
  in
  print_doc "direct-render-params-flat" params;
  print_doc_at_width "direct-render-params-constrained" 18 params;
  print_error "hole-error" (HoleE `Next $ at);
  print_error "fuse-error"
    (FuseE (VarE ("x" $ at) $ at, VarE ("y" $ at) $ at) $ at);
  print_error "unparen-error" (UnparenE (VarE ("x" $ at) $ at) $ at);
  print_error "latex-error" (LatexE "x_1" $ at)
