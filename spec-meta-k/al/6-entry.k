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

  // Isolated here rather than written inline, like `builtinCmd()` and
  // `externArgs()` in al/4-extern-json.k: K rules cannot read the environment,
  // so a name the run depends on has exactly one place to be changed.
  syntax Id ::= "entryRel" "(" ")"          [function, total, symbol(entryRel)]
  rule entryRel() => "Program_ok"

  syntax KItem ::= "finish" "(" ")"
  rule <k> V:Val ~> finish() => logDebug(V) ~> dropBuiltinReq() ... </k>
       <result> _ => V </result>
  // A relation yields `val*`, not a bare `val` (`relCallDone()` in
  // 5.6-eval-call-rel.k), so the relation entry lands here instead.  Keeping the
  // two shapes distinct is deliberate: conflating them is what once fed a
  // `ValList` to a continuation expecting a `Val`.
  rule <k> Vs:ValList ~> finish() => dropBuiltinReq() ... </k>
       <result> _ => Vs </result>
  rule <k> FAIL ~> finish() => dropBuiltinReq() ... </k>
       <result> _ => FAIL </result>

  // Remove the scratch file that builtin calls shared over this run
  // (al/5.5-eval-call-func.k).  `#mkstemp` is documented to delete its files
  // when rewriting ends, but K 7.1.337's LLVM backend does not, so the run
  // takes it down itself.  `#remove` has sort `K`, hence the sequencing here
  // rather than a function call.
  syntax KItem ::= "dropBuiltinReq" "(" ")"
  rule <k> dropBuiltinReq() => #remove(P) ... </k>
       <builtinreq> P:String => "" </builtinreq>
    requires P =/=String ""
  //   No builtin ran, so there is nothing to remove.
  rule <k> dropBuiltinReq() => .K ... </k>
       <builtinreq> "" </builtinreq>

endmodule
