// Entry point.
//
// Mirrors spec-meta/al/6-entry.watsup.

requires "5.6-eval-call-rel.k"

module AL
  imports AL-CALL-REL

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

  // The verdict.
  //
  // What the run produced used to be stashed in a `<result>` cell and read out
  // of the configuration dump.  With the dump suppressed (`--output none`)
  // there is nobody left to read a cell, so the outcome is *printed*, and
  // printed as a verdict rather than as a value: `passed` when the entry
  // succeeded, `fail` when it did not.
  //
  // Note this reports whether the entry *succeeded*, which for `k-typecheck`
  // is the question being asked -- `Program_ok` failing is a type error in the
  // program under test.  A `k-run` target's actual answer (`intN(119)` for
  // `add`) is no longer printed here; it reaches stdout only if the script
  // logs it with a `debug` premise.
  syntax KItem ::= "finish" "(" ")"
  syntax KItem ::= verdict(String)          [symbol(verdict)]

  rule <k> verdict(S:String) => #seqK(#write(1, S +String "\n"), .K) ... </k>

  rule <k> _:Val ~> finish() => verdict("passed") ... </k>
  // A relation yields `val*`, not a bare `val` (`relCallDone()` in
  // 5.6-eval-call-rel.k), so the relation entry lands here instead.  Keeping the
  // two shapes distinct is deliberate: conflating them is what once fed a
  // `ValList` to a continuation expecting a `Val`.
  rule <k> _:ValList ~> finish() => verdict("passed") ... </k>
  rule <k> FAIL ~> finish() => verdict("fail") ... </k>

endmodule
