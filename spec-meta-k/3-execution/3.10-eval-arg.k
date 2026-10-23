// Argument and type argument evaluation, and the call expression.
//
// Mirrors spec-meta/3-execution/3.09-eval-arg.watsup. An `Arg` at the head
// of `<k>` evaluates to a value (1.0-syntax.k); `callE` then evaluates its
// type arguments and calls the function.

requires "3.09-eval-path.k"

module EVAL-ARG
  imports EVAL-PATH

  // ==========================================================================
  // Eval_arg: ctx |- arg : argRes
  // ==========================================================================

  //   rule Eval_arg/exp
  rule <k> expA(V:Val) => V ... </k>

  //   rule Eval_arg/fun: the global function name of the parameter
  rule <k> funA(X:Id) => funcV(resolveFuncParam(X, LF)) ... </k>
       <lfenv> LF:Map </lfenv>

  // ==========================================================================
  // Eval_targs: ctx |- targ* : typsRes
  //
  //   rule Eval_exp/call: the type arguments under the local tdenv's aliases,
  //   then Call_func
  // ==========================================================================

  rule <k> callE(X:Id, Ts:TArgList, Vs:ValList)
        => callTargs(X, substTargs(thetaOfTdenv(L), Ts), Vs)
       ...
       </k>
       <ltdenv> L:Map </ltdenv>

  syntax KItem ::= callTargs(Id, Res, ValList)
  rule <k> callTargs(X:Id, ok(Ts:TArgList), Vs:ValList) => callFunc(X, Ts, Vs) ... </k>
  rule <k> callTargs(_:Id, ERROR, _:ValList) => error ... </k>

endmodule
