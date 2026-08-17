open Domain
open Lang
open El
open Util.Source

module Fixture = Test_common.El_fixture.Make (struct
  let at = no_region
end)

open Fixture

let relation_type : nottyp = nottyp (AtomT (atom (Atom.Keyword "CHECK")))

let spec : spec =
  [
    def (RelD (id "Check", relation_type, []));
    def
      (FuncDefD
         ( id "identity",
           [],
           [ arg (ExpA (var "x")) ],
           var "x",
           [ prem (IfPr (exp (BoolE true))) ] ));
  ]

let () =
  spec
  |> List.map Backend_adoc.El.render_def
  |> String.concat "\n\n" |> print_endline
