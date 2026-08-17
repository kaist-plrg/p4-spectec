open Domain
open Lang
open El
open Util.Source

let phrase it = it $ no_region
let id value = phrase value
let atom value = phrase value
let exp value = phrase value
let arg value = phrase value
let prem value = phrase value
let rule value = phrase value
let var name = exp (VarE (id name))
let nottyp value = phrase value
let plaintyp value = phrase value
let param value = phrase value
let def value = phrase value
let nat = plaintyp (NumT `NatT)
let relation_type = nottyp (AtomT (atom (Atom.Keyword "RELATION")))

let spec =
  [
    def (ExternDecD (id "external_title_fn", [], [ param (ExpP nat) ], nat, []));
    def (BuiltinDecD (id "builtin_title_fn", [], [ param (ExpP nat) ], nat, []));
    def (ExternDecD (id "external_full_fn", [], [ param (ExpP nat) ], nat, []));
    def (BuiltinDecD (id "builtin_full_fn", [], [ param (ExpP nat) ], nat, []));
    def (FuncDecD (id "documented_fn", [], [ param (ExpP nat) ], nat, []));
    def (FuncDecD (id "missing_fn", [], [ param (ExpP nat) ], nat, []));
    def (FuncDecD (id "prose_only_fn", [], [ param (ExpP nat) ], nat, []));
    def (RelD (id "Documented_rel", relation_type, []));
    def (RelD (id "Missing_rel", relation_type, []));
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
    exp (CallE (id "prose_only_fn", [], [ arg (ExpA (var "p4program")) ]))
  in
  let equation =
    exp (CmpE (exp (IterE (var "declaration", List)), `EqOp, flatten_call))
  in
  let definition =
    def
      (RuleGroupD
         ( id "Documented_rel",
           id "prose_call",
           [
             rule
               ( id "Documented_rel",
                 id "prose_call",
                 var "result",
                 [ prem (IfPr equation) ] );
           ] ))
  in
  Printf.printf "[prose-call-premise]\n%s\n"
    (Backend_latex.El.render_defs
       ~anchors:
         (Backend_latex.El.anchors ~func:context.anchors_latex.func
            ~rel:context.anchors_latex.rel)
       [ definition ])
