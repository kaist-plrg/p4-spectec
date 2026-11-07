// Results: recoverable mismatches, execution errors, and failure propagation.
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
  // `ok(X)` for `OK X`, or a failure. On `<k>`, `OK X` is `X` and `OK` is `.K`.
  // ==========================================================================

  syntax Error ::= "ERROR"                  [symbol(ERROR)]
  syntax Unmatch ::= "UNMATCH"              [symbol(UNMATCH)]
  syntax Failure ::= Error | Unmatch
  syntax Res ::= ok(KItem)                  [symbol(ok)]
               | Failure

  syntax KResult ::= Val | ValList | ValField | ValFieldList | ValCase
                   | ValListList

  syntax Res ::= consRes(Res, Res)          [function, symbol(consRes)]
  rule consRes(ok(V:Val), ok(Vs:ValList)) => ok((V, Vs))
  rule consRes(ok(T:Typ), ok(Ts:TypList)) => ok((T, Ts))
  rule consRes(ok(T:Typ), ok(Ts:TArgList)) => ok((T, Ts))
  rule consRes(F:Failure, _:Res) => F
  rule consRes(ok(_), F:Failure) => F

  // ==========================================================================
  // Failure propagation
  // ==========================================================================

  syntax UnmatchBoundary ::= ErrorBoundary
  syntax ErrorBoundary ::= endScope(Map)    // lvenv
                         | "endFrame"

  rule <k> UNMATCH ~> (K:KItem => .K) ... </k>
    requires notBool isUnmatchBoundary(K)
  rule <k> ERROR ~> (K:KItem => .K) ... </k>
    requires notBool isErrorBoundary(K)

  // A function's outcome on `<k>`.
  syntax KItem ::= fromRes(Res)
                 | fromResBool(Res)
  rule <k> fromRes(ok(R:KItem)) => R ... </k>
  rule <k> fromRes(F:Failure) => F ... </k>
  rule <k> fromResBool(ok(B:Bool)) => boolV(B) ... </k>
  rule <k> fromResBool(F:Failure) => F ... </k>

endmodule
