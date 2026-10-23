// Premise evaluation.
//
// Mirrors spec-meta/3-execution/3.10-eval-prem.watsup: Eval_prem, Eval_prems.
// A premise is a statement: one that holds rewrites to `.K`, one that does
// not to `unmatch`, one that cannot be evaluated to `error`. Its expressions
// evaluate in place (1.0-syntax.k).

requires "3.08-eval-exp.k"

module PREM
  imports EVAL-EXP

  // ==========================================================================
  // Eval_prems: ctx |- prem* : res<ctx>
  // ==========================================================================

  rule <k> (P:Prem, Ps:PremList) => P ~> Ps ... </k>
  rule <k> .PremList => .K ... </k>

  // ==========================================================================
  // Eval_prem: ctx |- prem : res<ctx>
  // ==========================================================================

  //   rule Eval_prem/relpr
  rule <k> relPr(X:Id, Vs:ValList, Eouts:ExpList) => assignExps(Eouts, callRel(X, Vs)) ... </k>

  //   rulegroup Eval_prem/ifpr
  rule <k> ifPr(boolV(true)) => .K ... </k>
  rule <k> ifPr(boolV(false)) => unmatch ... </k>
  rule <k> ifPr(_:Val) => error ... </k> [owise]

  //   rulegroup Eval_prem/ifholdpr: IFHOLD needs an empty output
  syntax KItem ::= holds(KItem)             [strict]

  rule <k> ifHoldPr(X:Id, Vs:ValList) => holds(callRel(X, Vs)) ... </k>
  rule <k> holds(.ValList) => .K ... </k>
  rule <k> holds((_:Val, _:ValList)) => error ... </k>

  //   rulegroup Eval_prem/ifnotholdpr: IFNOTHOLD needs the relation to
  //   mismatch; its error propagates
  syntax KItem ::= "notHolds" "(" ")"

  rule <k> ifNotHoldPr(X:Id, Vs:ValList) => try(callRel(X, Vs)) ~> notHolds() ... </k>
  rule <k> UNMATCH ~> notHolds() => .K ... </k>
  rule <k> ok(_:ValList) ~> notHolds() => unmatch ... </k>
  rule <k> ERROR ~> notHolds() => error ... </k>

  //   rule Eval_prem/letpr
  rule <k> letPr(El:Exp, V:Val) => assignExp(El, V) ... </k>

  //   rulegroup Eval_prem/iterpr-opt
  rule <k> iterPr(P:Prem, iterPrem(quest(), Bound:VariList, Bind:VariList))
        => iterPrOpt(Bound, Bind, P, subOpt(Bound, M))
       ...
       </k>
       <lvenv> M:Map </lvenv>

  syntax KItem ::= iterPrOpt(VariList, VariList, Prem, Res)

  //   rule Eval_prem/succ-none: every bound variable is `OPT eps`
  rule <k> iterPrOpt(_:VariList, Bind:VariList, _:Prem, ok(noSub())) => .K ... </k>
       <lvenv> M:Map => addsMap(M, iterVaris(Bind, quest()),
                                repeatVal(optV(noVal()), lengthVaris(Bind))) </lvenv>

  //   rule Eval_prem/succ-some: the premise under the sub-context, then its
  //   bindings as `OPT val`
  rule <k> iterPrOpt(Bound:VariList, Bind:VariList, P:Prem, ok(someSub(Vs:ValList)))
        => bindOpts(Bind, inScope(bindRow(Bound, Vs) ~> P ~> readVaris(Bind)))
       ...
       </k>

  rule <k> iterPrOpt(_:VariList, _:VariList, _:Prem, ERROR) => error ... </k>

  //   rulegroup Eval_prem/iterpr-list
  //
  //   rule Eval_prem/succ-empty, /succ-list: the premise under each
  //   sub-context in turn, then the rows of bindings as lists
  rule <k> iterPr(P:Prem, iterPrem(star(), Bound:VariList, Bind:VariList))
        => iterPrList(Bound, Bind, P, subList(Bound, M))
       ...
       </k>
       <lvenv> M:Map </lvenv>

  syntax KItem ::= iterPrList(VariList, VariList, Prem, Res)
  rule <k> iterPrList(Bound:VariList, Bind:VariList, P:Prem, ok(Rows:ValListList))
        => bindLists(Bind, premRows(Bound, Bind, P, Rows))
       ...
       </k>
  rule <k> iterPrList(_:VariList, _:VariList, _:Prem, ERROR) => error ... </k>

  syntax KItem ::= premRows(VariList, VariList, Prem, ValListList)
  rule <k> premRows(_:VariList, _:VariList, _:Prem, .ValListList) => .ValListList ... </k>
  rule <k> premRows(Bound:VariList, Bind:VariList, P:Prem, (Row:ValList ; Rows:ValListList))
        => consRow(inScope(bindRow(Bound, Row) ~> P ~> readVaris(Bind)),
                   premRows(Bound, Bind, P, Rows))
       ...
       </k>

  //   rule Eval_prem/dbg
  rule <k> debugPr(V:Val) => logDebug(V) ... </k>

endmodule
