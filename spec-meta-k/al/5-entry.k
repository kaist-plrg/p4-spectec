// Entry point.
//
// Mirrors spec-meta/al/6-entry.watsup.

requires "4.6-eval-call-rel.k"

module AL
  imports AL-CALL-REL

  rule <k> logDebug(V:Val) => .K ... </k>
       <log> L:List => L ListItem(V) </log>

  rule <k> afterLoad()
        => logDebug(textV("load complete"))
           ~> logDebug(textV("into call"))
           ~> callFunc("main", .TypList, .ValList)
           ~> finish()
       ...
       </k>

  syntax KItem ::= "finish" "(" ")"
  rule <k> V:Val ~> finish() => logDebug(V) ... </k>
       <result> _ => V </result>
  rule <k> FAIL ~> finish() => .K ... </k>
       <result> _ => FAIL </result>

endmodule
