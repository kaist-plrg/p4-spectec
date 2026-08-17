open Domain
open Lang
open El
open Util.Source

let at = no_region
let relation_type : nottyp = AtomT (Atom.Keyword "CHECK" $ at) $ at

let spec : spec =
  [
    RelD ("Check" $ at, relation_type, []) $ at;
    FuncDefD
      ( "identity" $ at,
        [],
        [ ExpA (VarE ("x" $ at) $ at) $ at ],
        VarE ("x" $ at) $ at,
        [ IfPr (BoolE true $ at) $ at ] )
    $ at;
  ]

let () =
  spec
  |> List.map Backend_adoc.El.render_def
  |> String.concat "\n\n" |> print_endline
