// Entry point.
//
// Mirrors spec-meta/3-execution/3.13-entry.watsup. The outcome of the run is
// printed last: a value or `passed` for `OK`, `fail` for `UNMATCH`, `error`
// for `ERROR`.

requires "3.13-eval-call-rel.k"

module AL
  imports CALL-REL

  // Calls $main(), assuming it exists in the given spec.
  rule <k> afterLoad()
        => logDebug(textV("load complete"))
           ~> logDebug(textV("into call"))
           ~> try(callFunc("main", .TArgList, .ValList))
           ~> finishMain()
       ...
       </k>
       <p4prog> noP4() </p4prog>

  // Calls the entry relation with a P4 program.
  rule <k> afterLoad()
        => logDebug(textV("load complete"))
           ~> logDebug(textV("into call"))
           ~> try(callRel(entryRel(), (V, .ValList)))
           ~> finishRel()
       ...
       </k>
       <p4prog> someP4(V:Val) </p4prog>

  syntax Id ::= "entryRel" "(" ")"          [function, total, symbol(entryRel)]
  rule entryRel() => "Program_ok"

  syntax KItem ::= "finishRel" "(" ")"
                 | "finishMain" "(" ")"
  syntax KItem ::= result(String)          [symbol(result)]

  rule <k> result(S:String) => #write(1, S +String "\n") ... </k>

  rule <k> ok(_:ValList) ~> finishRel() => result("passed") ... </k>
  rule <k> UNMATCH ~> finishRel() => result("fail") ... </k>
  rule <k> ERROR ~> finishRel() => result("error") ... </k>

  rule <k> ok(V:Val) ~> finishMain() => result(JSON2String(jsonOfVal(V))) ... </k>
  rule <k> UNMATCH ~> finishMain() => result("fail") ... </k>
  rule <k> ERROR ~> finishMain() => result("error") ... </k>

endmodule
