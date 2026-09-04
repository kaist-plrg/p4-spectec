// Entry point.
//
// Mirrors spec-meta/al/6-entry.watsup.

requires "5.6-eval-call-rel.k"

module AL
  imports AL-CALL-REL

  // Calls $main(), assuming it exists in the given script.
  rule <k> afterLoad()
        => logDebug(textV("load complete"))
           ~> logDebug(textV("into call"))
           ~> callFunc("main", .TypList, .ValList)
           ~> finishMain()
       ...
       </k>
       <p4prog> noP4() </p4prog>

  // Calls entry relation with a P4 program
  rule <k> afterLoad()
        => logDebug(textV("load complete"))
           ~> logDebug(textV("into call"))
           ~> callRel(entryRel(), (V, .ValList))
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

  rule <k> _:Val ~> finishRel() => result("passed") ... </k>
  rule <k> _:ValList ~> finishRel() => result("passed") ... </k>
  rule <k> FAIL ~> finishRel() => result("fail") ... </k>

  rule <k> V:Val ~> finishMain() => result(JSON2String(jsonOfVal(V))) ... </k>
  rule <k> FAIL ~> finishMain() => result("fail") ... </k>

endmodule
