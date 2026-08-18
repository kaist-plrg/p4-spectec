open Domain
open Lang
open Util.Source

let bool_pl : Il.typ = Il.BoolT $ no_region

let def_pl (name : string) : Pl.def =
  Pl.FuncDecD (name $ no_region, [], [], bool_pl, [], None)
  $ no_region |> Pl.Annot.no_hints

let skeleton : string =
  {|FUNCTION TITLE
${func-title-prose: identity}
FUNCTION
${func-prose: identity}|}

let () =
  let anchors_prose =
    Backend_splice.Ctx.
      {
        func =
          (function "identity" -> Some "function_prose_identity" | _ -> None);
        rel = (fun _ -> None);
      }
  in
  let context =
    Backend_splice.Ctx.
      { anchors_prose; anchors_latex = Backend_splice.Ctx.empty_anchors }
  in
  let source =
    Backend_splice.Source.{ file = "fixture.adoc"; s = skeleton; i = 0 }
  in
  Backend_splice.Driver.init ~context [] [ def_pl "identity" ];
  Backend_splice.Driver.splice_string source skeleton |> print_endline
