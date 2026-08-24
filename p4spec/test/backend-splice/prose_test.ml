open Domain
open Lang
open Util.Source

let bool_pl : Il.typ = Il.BoolT $ no_region
let rel_signature : Pl.rel_signature = (Mixfix.Arg bool_pl $ no_region, [ 0 ])

let bool_exp (value : bool) : Pl.exp =
  Pl.BoolE value $$ (no_region, Il.BoolT) |> Pl.Annot.no_hints

let instr (instr_tier : 'a) : 'a Pl.instr =
  Pl.TierI instr_tier
  $$ (no_region, { Pl.iid = 0; fallthrough = None })
  |> Pl.Annot.no_hints

let def_pl (name : string) : Pl.def =
  Pl.FuncDecD (name $ no_region, [], [], bool_pl, [], None)
  $ no_region |> Pl.Annot.no_hints

let rel_def_pl (name : string) : Pl.def =
  let id_rel = name $ no_region in
  let exp = bool_exp true in
  let result = instr (Pl.ResultI (rel_signature, [ exp ])) in
  let elsegroup =
    instr
      (Pl.GroupI
         ("fallback" $ no_region, id_rel, rel_signature, [ exp ], [ result ]))
  in
  Pl.RelD (id_rel, rel_signature, [ exp ], [], Some [ elsegroup ])
  $ no_region |> Pl.Annot.no_hints

let skeleton : string =
  {|FUNCTION TITLE
${func-title-prose: identity}
FUNCTION
${func-prose: identity}
RELATION ELSE
${rulegroup-prose-else: fallback}|}

let () =
  let anchors_prose =
    Backend_splice.Ctx.
      {
        func =
          (function "identity" -> Some "function_prose_identity" | _ -> None);
        rel =
          (function "fallback" -> Some "relation_prose_fallback" | _ -> None);
      }
  in
  let context =
    Backend_splice.Ctx.make ~anchors_prose
      ~anchors_latex:Backend_splice.Ctx.empty_anchors
  in
  let source =
    Backend_splice.Source.{ file = "fixture.adoc"; s = skeleton; i = 0 }
  in
  Backend_splice.Driver.init ~context []
    [ def_pl "identity"; rel_def_pl "fallback" ];
  Backend_splice.Driver.splice_string source skeleton |> print_endline
