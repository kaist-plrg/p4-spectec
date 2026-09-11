open Domain
open Lang
open El
open Util.Source

let at = no_region
let nat : plaintyp = NumT `NatT $ at
let relation_type : nottyp = AtomT (Atom.Keyword "CHECK" $ at) $ at

let spec : spec =
  [
    TypD ("Flag" $ at, [], PlainTD nat $ at, []) $ at;
    RelD ("Check" $ at, relation_type, []) $ at;
    RuleGroupD
      ( "Check" $ at,
        "value" $ at,
        [ ("Check" $ at, "ok" $ at, VarE ("x" $ at) $ at, []) $ at ] )
    $ at;
    FuncDecD ("identity" $ at, [], [ ExpP nat $ at ], nat, []) $ at;
    FuncDefD
      ( "identity" $ at,
        [],
        [ ExpA (VarE ("x" $ at) $ at) $ at ],
        VarE ("x" $ at) $ at,
        [] )
    $ at;
    TableDefD
      ( "truth" $ at,
        [ (NumE (`DecOp, `Nat (Bigint.of_int 0)) $ at, BoolE false $ at) $ at ]
      )
    $ at;
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
