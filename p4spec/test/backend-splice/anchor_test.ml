open Domain
open Lang
open El
open Util.Source

let at = no_region
let nat = NumT `NatT $ at
let relation_type = AtomT (Atom.Keyword "RELATION" $ at) $ at
let bool_pl = Il.BoolT $ at

let def_pl name =
  Pl.FuncDecD (name $ at, [], [], bool_pl, [], None) $ at |> Pl.Annot.no_hints

let spec_pl =
  [
    def_pl "overlap_fn";
    def_pl "title_only_fn";
    def_pl "full_only_fn";
    def_pl "apostrophe_fn'";
  ]

let spec =
  [
    ExternDecD ("external_title_fn" $ at, [], [ ExpP nat $ at ], nat, []) $ at;
    BuiltinDecD ("builtin_title_fn" $ at, [], [ ExpP nat $ at ], nat, []) $ at;
    ExternDecD ("external_full_fn" $ at, [], [ ExpP nat $ at ], nat, []) $ at;
    BuiltinDecD ("builtin_full_fn" $ at, [], [ ExpP nat $ at ], nat, []) $ at;
    FuncDecD ("documented_fn" $ at, [], [ ExpP nat $ at ], nat, []) $ at;
    FuncDecD ("missing_fn" $ at, [], [ ExpP nat $ at ], nat, []) $ at;
    FuncDecD ("prose_only_fn" $ at, [], [ ExpP nat $ at ], nat, []) $ at;
    RelD ("Documented_rel" $ at, relation_type, []) $ at;
    RelD ("Missing_rel" $ at, relation_type, []) $ at;
  ]

let source_a =
  {|
${func-title-prose: documented_fn external_title_fn builtin_title_fn absent_fn}
${relation-title-prose: Documented_rel}
${func-title-latex: documented_fn external_title_fn builtin_title_fn absent_fn}
${relation-title-latex: Missing_rel}
|}

let source_b =
  {|
${func-title-prose: documented_fn}
${func-prose: prose_only_fn external_full_fn builtin_full_fn}
${func-latex: documented_fn external_full_fn builtin_full_fn}
|}

let print_anchor anchor kind name =
  let value = match anchor name with Some anchor -> anchor | None -> "none" in
  Printf.printf "%s -> %s\n" kind value

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

let index_substring needle text =
  let needle_length = String.length needle in
  let text_length = String.length text in
  let rec find index =
    if index + needle_length > text_length then None
    else if String.equal (String.sub text index needle_length) needle then
      Some index
    else find (index + 1)
  in
  find 0

let anchor_skeleton =
  {|first occurrence
${func-title-prose: overlap_fn title_only_fn}
second occurrence
${func-title-prose: overlap_fn}
${func-prose: overlap_fn full_only_fn apostrophe_fn'}|}

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

let print_first_anchor_position rendered =
  let index_anchor =
    index_substring {|id="function_prose_overlap_fn"|} rendered
  in
  let index_second = index_substring "second occurrence" rendered in
  let before_second =
    match (index_anchor, index_second) with
    | Some index_anchor, Some index_second -> index_anchor < index_second
    | None, _ | _, None -> false
  in
  Printf.printf "first anchor precedes second occurrence: %b\n" before_second

let () =
  let open Backend_splice in
  let context =
    Anchor.collect spec [ ("a.adoc", source_a); ("b.adoc", source_b) ]
  in
  print_anchor context.anchors_prose.func "prose function documented_fn"
    "documented_fn";
  print_anchor context.anchors_latex.func "latex function documented_fn"
    "documented_fn";
  print_anchor context.anchors_prose.func "prose function external_title_fn"
    "external_title_fn";
  print_anchor context.anchors_latex.func "latex function external_title_fn"
    "external_title_fn";
  print_anchor context.anchors_prose.func "prose function builtin_title_fn"
    "builtin_title_fn";
  print_anchor context.anchors_latex.func "latex function builtin_title_fn"
    "builtin_title_fn";
  print_anchor context.anchors_prose.func "prose function prose_only_fn"
    "prose_only_fn";
  print_anchor context.anchors_latex.func "latex function prose_only_fn"
    "prose_only_fn";
  print_anchor context.anchors_prose.func "prose function external_full_fn"
    "external_full_fn";
  print_anchor context.anchors_latex.func "latex function external_full_fn"
    "external_full_fn";
  print_anchor context.anchors_prose.func "prose function missing_fn"
    "missing_fn";
  print_anchor context.anchors_latex.func "latex function missing_fn"
    "missing_fn";
  print_anchor context.anchors_prose.func "prose function absent_fn" "absent_fn";
  print_anchor context.anchors_latex.func "latex function absent_fn" "absent_fn";
  print_anchor context.anchors_prose.rel "prose relation Documented_rel"
    "Documented_rel";
  print_anchor context.anchors_latex.rel "latex relation Documented_rel"
    "Documented_rel";
  print_anchor context.anchors_prose.rel "prose relation Missing_rel"
    "Missing_rel";
  print_anchor context.anchors_latex.rel "latex relation Missing_rel"
    "Missing_rel";
  let flatten_call =
    CallE
      ("prose_only_fn" $ at, [], [ ExpA (VarE ("p4program" $ at) $ at) $ at ])
    $ at
  in
  let equation =
    CmpE (IterE (VarE ("declaration" $ at) $ at, List) $ at, `EqOp, flatten_call)
    $ at
  in
  let definition =
    RuleGroupD
      ( "Documented_rel" $ at,
        "prose_call" $ at,
        [
          ( "Documented_rel" $ at,
            "prose_call" $ at,
            VarE ("result" $ at) $ at,
            [ IfPr equation $ at ] )
          $ at;
        ] )
    $ at
  in
  Printf.printf "[prose-call-premise]\n%s\n"
    (Backend_latex.El.render_defs
       ~anchors:
         (Backend_latex.El.anchors ~func:context.anchors_latex.func
            ~rel:context.anchors_latex.rel)
       [ definition ]
    |> Result.get_ok);
  let anchors_prose =
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
    Backend_splice.Ctx.make ~anchors_prose
      ~anchors_latex:Backend_splice.Ctx.empty_anchors
  in
  print_endline "[anchor-ownership]";
  let splice_anchor () =
    let source =
      Backend_splice.Source.
        { file = "fixture.adoc"; s = anchor_skeleton; i = 0 }
    in
    Backend_splice.Driver.splice_string source anchor_skeleton
  in
  Backend_splice.Driver.init ~context [] spec_pl;
  let rendered = splice_anchor () in
  rendered |> print_anchor_counts "first";
  rendered |> print_first_anchor_position;
  Backend_splice.Driver.init ~context [] spec_pl;
  let rendered = splice_anchor () in
  rendered |> print_anchor_counts "second"
