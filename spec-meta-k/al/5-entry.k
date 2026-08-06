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
  rule <k> V:Val ~> finish() => logDebug(V) ~> dropBuiltinReq() ... </k>
       <result> _ => V </result>
  rule <k> FAIL ~> finish() => dropBuiltinReq() ... </k>
       <result> _ => FAIL </result>

  // Remove the scratch file that builtin calls shared over this run
  // (al/4.5-eval-call-func.k).  `#mkstemp` is documented to delete its files
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
