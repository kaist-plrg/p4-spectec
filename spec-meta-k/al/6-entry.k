// Entry point.
//
// Mirrors spec-meta/al/6-entry.watsup.

requires "5.6-eval-call-rel.k"

module AL
  imports AL-CALL-REL

  // Two entries, selected by whether a target program was supplied.
  //
  // Without one, the script is self-contained and defines its own `$main()`.
  rule <k> afterLoad()
        => logDebug(textV("load complete"))
           ~> logDebug(textV("into call"))
           ~> callFunc("main", .TypList, .ValList)
           ~> finishMain()
       ...
       </k>
       <p4prog> noP4() </p4prog>

  // With one, the script is a language spec and the program is its input, so
  // the entry is a relation rather than a function.
  rule <k> afterLoad()
        => logDebug(textV("load complete"))
           ~> logDebug(textV("into call"))
           ~> callRel(entryRel(), (V, .ValList))
           ~> finish()
       ...
       </k>
       <p4prog> someP4(V:Val) </p4prog>

  // Isolated rather than written inline, so a name the run depends on has one
  // place to be changed.
  syntax Id ::= "entryRel" "(" ")"          [function, total, symbol(entryRel)]
  rule entryRel() => "Program_ok"

  // `finish()` ends the relation entry: only success or failure is meaningful.
  // `finishMain()` ends the function entry, where the returned value is the
  // answer, so it is printed in place of the verdict.
  syntax KItem ::= "finish" "(" ")"
                 | "finishMain" "(" ")"
  syntax KItem ::= verdict(String)          [symbol(verdict)]

  rule <k> verdict(S:String) => #write(1, S +String "\n") ... </k>

  rule <k> _:Val ~> finish() => verdict("passed") ... </k>
  rule <k> _:ValList ~> finish() => verdict("passed") ... </k>
  rule <k> FAIL ~> finish() => verdict("fail") ... </k>

  rule <k> V:Val ~> finishMain() => verdict(JSON2String(jsonOfVal(V))) ... </k>
  rule <k> FAIL ~> finishMain() => verdict("fail") ... </k>

endmodule
