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

  syntax KResult ::= Val | ValList | ValField | ValFieldList | ValCase
                   | ValListList | Res

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
  // A rule with no applicable case rewrites to a signal: `unmatch` for a
  // recoverable mismatch, `error` for an execution error. A signal discards
  // the pending work of the current alternative up to the boundary that
  // started it. A checkpoint turns it into the outcome `UNMATCH` or `ERROR`;
  // a scope or frame boundary (3.02-context.k) restores its state and lets
  // it go on.
  // ==========================================================================

  syntax Signal ::= "unmatch" | "error"

  syntax Boundary ::= checkpoint(Map, Map)  // lfenv, lvenv
                    | endScope(Map)         // lvenv
                    | "endFrame"

  rule <k> _:Signal ~> (K:KItem => .K) ... </k> requires notBool isBoundary(K)

  // ==========================================================================
  // Tries
  //
  // `try(K)` runs `K` as one alternative: its failure is caught, its bindings
  // are undone afterwards, and its outcome is recorded.
  // ==========================================================================

  syntax KItem ::= try(K)

  rule <k> try(K:K) => K ~> checkpoint(F, V) ... </k>
       <lfenv> F:Map </lfenv>
       <lvenv> V:Map </lvenv>
  rule <k> R:KResult ~> checkpoint(F:Map, V:Map) => ok(R) ... </k>
       <lfenv> _ => F </lfenv>
       <lvenv> _ => V </lvenv>
  rule <k> unmatch ~> checkpoint(F:Map, V:Map) => UNMATCH ... </k>
       <lfenv> _ => F </lfenv>
       <lvenv> _ => V </lvenv>
  rule <k> error ~> checkpoint(F:Map, V:Map) => ERROR ... </k>
       <lfenv> _ => F </lfenv>
       <lvenv> _ => V </lvenv>

  // `unres`: an outcome back in flight; `unresBool`: a `bool` outcome as
  // its value.
  syntax KItem ::= unres(Res)
                 | unresBool(Res)
  rule <k> unres(ok(R:KItem)) => R ... </k>
  rule <k> unres(UNMATCH) => unmatch ... </k>
  rule <k> unres(ERROR) => error ... </k>
  rule <k> unresBool(ok(B:Bool)) => boolV(B) ... </k>
  rule <k> unresBool(F:Failure) => unres(F) ... </k>

endmodule
