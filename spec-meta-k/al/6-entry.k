// Entry point.
//
// Mirrors spec-meta/al/6-entry.watsup.

requires "5.6-eval-call-rel.k"

module AL
  imports AL-CALL-REL

  rule <k> logDebug(V:Val) => .K ... </k>
       <log> L:List => L ListItem(V) </log>

  // Two entries, selected by whether a target program was supplied.
  //
  // Without one, the script is self-contained and defines its own `$main()`;
  // this is the `examples/*.watsup` case and mirrors 6-entry.watsup exactly.
  rule <k> afterLoad()
        => logDebug(textV("load complete"))
           ~> logDebug(textV("into call"))
           ~> callFunc("main", .TypList, .ValList)
           ~> finish()
       ...
       </k>
       <p4prog> noP4() </p4prog>

  // With one, the script is a *language* spec and the program is its input, so
  // the entry is a relation rather than a function.  Only the entry differs:
  // loading, evaluation and `finish()` are shared.
  rule <k> afterLoad()
        => logDebug(textV("load complete"))
           ~> logDebug(textV("into call"))
           ~> callRel(entryRel(), (V, .ValList))
           ~> finish()
       ...
       </k>
       <p4prog> someP4(V:Val) </p4prog>

  // Isolated here rather than written inline, so a name the run depends on has
  // exactly one place to be changed.  Unlike the target spec path -- which used
  // to be hardcoded the same way and is now the `$SPEC` configuration variable
  // (al/4.2-extern-ffi.k) -- this one stays put: it is a property of the P4
  // spec, which names this relation, not of the invocation.
  syntax Id ::= "entryRel" "(" ")"          [function, total, symbol(entryRel)]
  rule entryRel() => "Program_ok"

  syntax KItem ::= "finish" "(" ")"
  rule <k> V:Val ~> finish() => logDebug(V) ... </k>
       <result> _ => V </result>
  // A relation yields `val*`, not a bare `val` (`relCallDone()` in
  // 5.6-eval-call-rel.k), so the relation entry lands here instead.  Keeping the
  // two shapes distinct is deliberate: conflating them is what once fed a
  // `ValList` to a continuation expecting a `Val`.
  rule <k> Vs:ValList ~> finish() => .K ... </k>
       <result> _ => Vs </result>
  rule <k> FAIL ~> finish() => .K ... </k>
       <result> _ => FAIL </result>

endmodule
