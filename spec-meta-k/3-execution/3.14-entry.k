// Entry point: serves the host's calls.
//
// `serve`: sends the answer to the last call and dispatches the next.
// `exit` : ends the run.

requires "3.04.3-spec-json.k"
requires "3.13-eval-call-rel.k"

module AL
  imports CALL-REL
  imports SPEC-JSON

  syntax KItem ::= dispatch(JSON)

  rule <k> serve(J:JSON) => dispatch(String2JSON(hostExchange(B, S, L, JSON2String(J)))) ... </k>
       <hbuffer> B:Int </hbuffer>
       <hserve> S:Int </hserve>
       <hstrlen> L:Int </hstrlen>

  // ==========================================================================
  // Loading: $load_ctx($empty_ctx, spec)
  // ==========================================================================

  syntax KItem ::= loadSpec(Spec)

  rule <k> loadSpec(S:Spec) => S ... </k>
       <gtdenv> _ => .Map </gtdenv>
       <grenv> _ => .Map </grenv>
       <gfenv> _ => .Map </gfenv>

  rule <k> dispatch({ "load" : [ Js:JSONs ], .JSONs })
        => loadSpec(specOfJsons(Js)) ~> serve({ "ok" : [ .JSONs ], .JSONs })
       ...
       </k>

  // ==========================================================================
  // Call_rel and Call_func on the loaded spec
  // ==========================================================================

  syntax ErrorBoundary ::= "answerRel" "(" ")"
                         | "answerFunc" "(" ")"

  rule <k> dispatch({ "args" : [ Js:JSONs ], "call-rel" : X:String, .JSONs })
        => callRel(X, valsOfJsons(Js)) ~> answerRel()
       ...
       </k>
  rule <k> dispatch({ "args" : [ Jv:JSONs ], "call-func" : X:String,
                      "targs" : [ Jt:JSONs ], .JSONs })
        => callFunc(X, targsOfJsons(Jt), valsOfJsons(Jv)) ~> answerFunc()
       ...
       </k>

  rule <k> Vs:ValList ~> answerRel() => serve({ "ok" : jsonList(jsonOfVals(Vs)), .JSONs }) ... </k>
  rule <k> V:Val ~> answerFunc() => serve({ "ok" : jsonOfVal(V), .JSONs }) ... </k>
  rule <k> F:Failure ~> answerRel() => serve(failAnswer(F)) ... </k>
  rule <k> F:Failure ~> answerFunc() => serve(failAnswer(F)) ... </k>

  syntax JSON ::= failAnswer(Failure)       [function, total, symbol(failAnswer)]
  rule failAnswer(UNMATCH) => { "fail" : null, .JSONs }
  rule failAnswer(ERROR) => { "error" : "the specification evaluated to ERROR", .JSONs }

  // ==========================================================================
  // Exit
  // ==========================================================================

  rule <k> dispatch({ "exit" : null, .JSONs }) => .K ... </k>

endmodule
