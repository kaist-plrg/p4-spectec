// Premise evaluation.
//
// Mirrors spec-meta/3-execution/3.10-eval-prem.watsup: Eval_prem, Eval_prems.
// A premise is a statement: one that holds rewrites to `.K`, one that does
// not backtracks. Its expressions evaluate in place (1.0-syntax.k).

requires "3.08-eval-exp.k"

module PREM
  imports EVAL-EXP

  syntax KItem ::= callRel(Id, ValList)

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
  rule <k> ifPr(_:Val) => backtrack ... </k> [owise]

  //   rulegroup Eval_prem/ifholdpr: IFHOLD needs an empty output, IFNOTHOLD
  //   needs the relation to fail
  syntax KItem ::= holds(KItem)             [strict]
                 | notHolds(Try)            [strict]

  rule <k> ifHoldPr(X:Id, Vs:ValList) => holds(callRel(X, Vs)) ... </k>
  rule <k> holds(.ValList) => .K ... </k>
  rule <k> holds((_:Val, _:ValList)) => backtrack ... </k>

  rule <k> ifNotHoldPr(X:Id, Vs:ValList) => notHolds(try(callRel(X, Vs))) ... </k>
  rule <k> notHolds(FAIL) => .K ... </k>
  rule <k> notHolds(ok(_:ValList)) => backtrack ... </k>

  //   rule Eval_prem/letpr
  rule <k> letPr(El:Exp, V:Val) => assignExp(El, V) ... </k>

  //   rulegroup Eval_prem/iterpr-opt
  rule <k> iterPr(P:Prem, iterPrem(quest(), Bound:VariList, Bind:VariList))
        => iterPrOpt(Bound, Bind, P, finds(iterVaris(Bound, quest()), M))
       ...
       </k>
       <lvenv> M:Map </lvenv>

  syntax KItem ::= iterPrOpt(VariList, VariList, Prem, ValList)

  //   rule Eval_prem/none: every bound variable is `OPT eps`
  rule <k> iterPrOpt(_:VariList, Bind:VariList, _:Prem, Vs:ValList) => .K ... </k>
       <lvenv> M:Map => addsMap(M, iterVaris(Bind, quest()),
                                repeatVal(optV(noVal()), lengthVaris(Bind))) </lvenv>
    requires allNoneOpts(Vs)

  //   rule Eval_prem/some: the premise under the sub-context, then its
  //   bindings as `OPT val`
  rule <k> iterPrOpt(Bound:VariList, Bind:VariList, P:Prem, Vs:ValList)
        => bindOpts(Bind, inScope(bindRow(Bound, unwrapSomeOpts(Vs)) ~> P ~> readVaris(Bind)))
       ...
       </k>
    requires allSomeOpts(Vs)

  syntax KItem ::= bindOpts(VariList, KItem)
                                            [strict(2)]
  rule <k> bindOpts(Bind:VariList, Vs:ValList) => .K ... </k>
       <lvenv> M:Map => addsMap(M, iterVaris(Bind, quest()), someVals(Vs)) </lvenv>

  //   rulegroup Eval_prem/iterpr-list
  //
  //   rule Eval_prem/empty, /list: the premise under each sub-context in
  //   turn, then the rows of bindings as lists
  rule <k> iterPr(P:Prem, iterPrem(star(), Bound:VariList, Bind:VariList))
        => bindLists(Bind, premRows(Bound, Bind, P, rows(finds(iterVaris(Bound, star()), M))))
       ...
       </k>
       <lvenv> M:Map </lvenv>

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
