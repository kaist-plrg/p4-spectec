// Results: recoverable mismatches, execution errors, and failure in flight.
//
// Mirrors spec-meta/2-runtime/2.3-result.watsup.
//
//   syntax ok<X> = OK X
//   syntax error = ERROR
//   syntax unmatch = UNMATCH
//   syntax failure = error | unmatch
//   syntax unitRes = OK | error

requires "../0-aux/0.1-config.k"

module RESULT
  imports CONFIG

  // ==========================================================================
  // Outcomes
  //
  // The outcome of one alternative (a clause, a rule path, a table row) or of
  // a total function (`$subtyp`, `$upcast`, `$sub_list`, ...): `ok(X)` for
  // `OK X`, or a failure. The unit result `OK` of `unitRes` is `.K`.
  // ==========================================================================

  syntax Error ::= "ERROR"                  [symbol(ERROR)]
  syntax Unmatch ::= "UNMATCH"              [symbol(UNMATCH)]
  syntax Failure ::= Error | Unmatch
  syntax Res ::= ok(KItem)                  [symbol(ok)]
               | Failure

  // A failure is not a result, so that no strict context takes it into its
  // hole.
  syntax KResult ::= Val | ValList | ValField | ValFieldList | ValCase
                   | ValListList

  // `OK X_h :: X_t*` from `OK X_h` and `OK X_t*`, or the first failure.
  syntax Res ::= consRes(Res, Res)          [function, symbol(consRes)]
  rule consRes(ok(V:Val), ok(Vs:ValList)) => ok((V, Vs))
  rule consRes(ok(T:Typ), ok(Ts:TypList)) => ok((T, Ts))
  rule consRes(ok(T:Typ), ok(Ts:TArgList)) => ok((T, Ts))
  rule consRes(F:Failure, _:Res) => F
  rule consRes(ok(_), F:Failure) => F

  // ==========================================================================
  // Failure in flight
  //
  // A rule with no applicable case rewrites to its failure at the head of
  // `<k>`: `UNMATCH` for a recoverable mismatch, `ERROR` for an execution
  // error. A failure discards the pending work up to the boundary that
  // handles it. `UNMATCH` stops at a catch frame, which goes on with the
  // next alternative; `ERROR` passes every catch frame up to the answer to
  // the host. A scope or frame boundary (3.02-context.k) restores its state
  // and lets either go on.
  // ==========================================================================

  syntax Catch                              // catches `UNMATCH`
  syntax Answer                             // catches both, at the entry
  syntax ScopeOrFrame ::= endScope(Map)     // lvenv
                        | "endFrame"
  syntax Boundary ::= Catch | Answer | ScopeOrFrame

  rule <k> UNMATCH ~> (K:KItem => .K) ... </k>
    requires notBool isBoundary(K)
  rule <k> ERROR ~> (K:KItem => .K) ... </k>
    requires notBool (isScopeOrFrame(K) orBool isAnswer(K))

  // ==========================================================================
  // Tries
  //
  // `try(K)` runs `K` as one alternative: its bindings are undone afterwards,
  // and its success is recorded as `ok(R)`. Its checkpoint restores its state
  // like a scope, so a failure goes on to the catch frame after it.
  // ==========================================================================

  syntax KItem ::= try(K)
  syntax ScopeOrFrame ::= checkpoint(Map, Map)
                                            // lfenv, lvenv

  rule <k> try(K:K) => K ~> checkpoint(F, V) ... </k>
       <lfenv> F:Map </lfenv>
       <lvenv> V:Map </lvenv>
  rule <k> R:KResult ~> checkpoint(F:Map, V:Map) => ok(R) ... </k>
       <lfenv> _ => F </lfenv>
       <lvenv> _ => V </lvenv>
  rule <k> E:Failure ~> checkpoint(F:Map, V:Map) => E ... </k>
       <lfenv> _ => F </lfenv>
       <lvenv> _ => V </lvenv>

  // `unres`: an outcome on `<k>`; `unresBool`: a `bool` outcome as its value.
  syntax KItem ::= unres(Res)
                 | unresBool(Res)
  rule <k> unres(ok(R:KItem)) => R ... </k>
  rule <k> unres(UNMATCH) => UNMATCH ... </k>
  rule <k> unres(ERROR) => ERROR ... </k>
  rule <k> unresBool(ok(B:Bool)) => boolV(B) ... </k>
  rule <k> unresBool(F:Failure) => unres(F) ... </k>

endmodule
