// Entry point: serves the host's calls.
//
// Mirrors spec-meta/5-entry.watsup for Eval_AL, and serves the calls the
// host makes on a loaded spec as well ($load_ctx, Call_rel, Call_func), in
// the JSON of 3.04.1-extern-json.k. `serve(J)` sends J, the answer to the
// last call, and dispatches the next call. Each call runs as one `try`,
// answered with its values for `OK`, `fail` for `UNMATCH`, `error` for
// `ERROR`. `exit` empties `<k>`, which ends the run.

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
  //
  // The global layer holds the definitions of the last spec loaded.
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
  // Eval_AL: |- specAL : val
  //
  // The spec loaded, then $main(), which it is assumed to define.
  // ==========================================================================

  syntax KItem ::= "answerMain" "(" ")"

  rule <k> dispatch({ "eval-al" : [ Js:JSONs ], .JSONs })
        => logDebug(textV("entry-al"))
           ~> loadSpec(specOfJsons(Js))
           ~> logDebug(textV("load complete"))
           ~> logDebug(textV("into call"))
           ~> try(callFunc("main", .TArgList, .ValList))
           ~> answerMain()
       ...
       </k>

  rule <k> ok(V:Val) ~> answerMain() => logDebug(V) ~> serve({ "ok" : jsonOfVal(V), .JSONs }) ... </k>
  rule <k> F:Failure ~> answerMain() => serve(failAnswer(F)) ... </k>

  // ==========================================================================
  // Call_rel and Call_func on the loaded spec
  // ==========================================================================

  syntax KItem ::= "answerRel" "(" ")"
                 | "answerFunc" "(" ")"

  rule <k> dispatch({ "args" : [ Js:JSONs ], "call-rel" : X:String, .JSONs })
        => try(callRel(X, valsOfJsons(Js))) ~> answerRel()
       ...
       </k>
  rule <k> dispatch({ "args" : [ Jv:JSONs ], "call-func" : X:String,
                      "targs" : [ Jt:JSONs ], .JSONs })
        => try(callFunc(X, targsOfJsons(Jt), valsOfJsons(Jv))) ~> answerFunc()
       ...
       </k>

  rule <k> ok(Vs:ValList) ~> answerRel() => serve({ "ok" : jsonList(jsonOfVals(Vs)), .JSONs }) ... </k>
  rule <k> ok(V:Val) ~> answerFunc() => serve({ "ok" : jsonOfVal(V), .JSONs }) ... </k>
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
