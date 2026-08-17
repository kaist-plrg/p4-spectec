open Domain
open Lang
open El
open Util.Source

module Fixture = Test_common.El_fixture.Make (struct
  let at = no_region
end)

open Fixture

let nat : plaintyp = plaintyp (NumT `NatT)
let relation_type : nottyp = nottyp (AtomT (atom (Atom.Keyword "CHECK")))

let spec : spec =
  [
    def (TypD (id "Flag", [], deftyp (PlainTD nat), []));
    def (RelD (id "Check", relation_type, []));
    def
      (RuleGroupD
         (id "Check", id "value", [ rule (id "Check", id "ok", var "x", []) ]));
    def (FuncDecD (id "identity", [], [ param (ExpP nat) ], nat, []));
    def (FuncDefD (id "identity", [], [ arg (ExpA (var "x")) ], var "x", []));
    def
      (TableDefD
         ( id "truth",
           [
             row (exp (NumE (`DecOp, `Nat (Bigint.of_int 0))), exp (BoolE false));
           ] ));
  ]

let skeleton : string =
  {|SYNTAX
${syntax: Flag}
RELATION
${relation-title-source: Check}
RULE
${rulegroup-source: Check/value}
FUNCTION TITLE
${func-title-source: identity}
FUNCTION
${func-source: identity}
TABLE
${table-source: truth}|}

let () =
  let source =
    Backend_splice.Source.{ file = "fixture.adoc"; s = skeleton; i = 0 }
  in
  Backend_splice.Driver.init spec [];
  Backend_splice.Driver.splice_string source skeleton |> print_endline
