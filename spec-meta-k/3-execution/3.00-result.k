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
  // The outcome of a total function (`$subtyp`, `$upcast`, `$sub_list`, ...):
  // `ok(X)` for `OK X`, or a failure. On `<k>`, `OK X` is `X` itself and the
  // unit result `OK` of `unitRes` is `.K`.
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
  // error. A failure discards the pending work up to a boundary for it.
  //
  // An `UnmatchBoundary` stops `UNMATCH`, and an `ErrorBoundary`, one of
  // them, stops `ERROR` too. The other `UnmatchBoundary`s are catch frames,
  // which go on with the next alternative. An `ErrorBoundary` is a scope or
  // frame boundary (3.02-context.k), which restores its state and lets the
  // failure go on, or the answer to the host (3.14-entry.k).
  //
  // A catch frame between alternatives (`afterRul`, `afterClause`) holds the
  // `<lfenv>` and `<lvenv>` from before its alternative and restores them
  // both when it catches `UNMATCH` and when it takes the value.
  // ==========================================================================

  syntax UnmatchBoundary ::= ErrorBoundary
  syntax ErrorBoundary ::= endScope(Map)    // lvenv
                         | "endFrame"

  rule <k> UNMATCH ~> (K:KItem => .K) ... </k>
    requires notBool isUnmatchBoundary(K)
  rule <k> ERROR ~> (K:KItem => .K) ... </k>
    requires notBool isErrorBoundary(K)

  // `fromRes`: the computation on `<k>` from a function's outcome, `X` for
  // `ok(X)` and a failure in flight; `fromResBool`: a `bool` outcome as its
  // value.
  syntax KItem ::= fromRes(Res)
                 | fromResBool(Res)
  rule <k> fromRes(ok(R:KItem)) => R ... </k>
  rule <k> fromRes(F:Failure) => F ... </k>
  rule <k> fromResBool(ok(B:Bool)) => boolV(B) ... </k>
  rule <k> fromResBool(F:Failure) => F ... </k>

endmodule
