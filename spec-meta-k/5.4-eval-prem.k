// Premise evaluation.
//
// Mirrors spec-meta/5.5-eval-prem.watsup: Eval_prem, Eval_prems. A premise
// is a statement: one that holds rewrites to `.K`, one that does not
// backtracks. Its expressions evaluate in place (1-syntax.k).

requires "5.3-eval-exp.k"

module PREM
  imports EVAL-EXP

  syntax KItem ::= callRel(Id, ValList)

  // ==========================================================================
  // Eval_prems: ctx |- prem* : res<ctx>
  // ==========================================================================

  rule (P:Prem, Ps:PremList) => P ~> Ps
  rule .PremList => .K

  // ==========================================================================
  // Eval_prem: ctx |- prem : res<ctx>
  // ==========================================================================

  //   rule Eval_prem/relpr
  rule relPr(X:Id, Vs:ValList, Eouts:ExpList) => assignExps(Eouts, callRel(X, Vs))

  //   rulegroup Eval_prem/ifpr
  rule ifPr(boolV(true)) => .K
  rule ifPr(_:Val) => backtrack [owise]

  //   rulegroup Eval_prem/ifholdpr: IFHOLD needs an empty output, IFNOTHOLD
  //   needs the relation to fail
  syntax KItem ::= holds(KItem)             [strict]
                 | notHolds(Try)            [strict]

  rule ifHoldPr(X:Id, Vs:ValList) => holds(callRel(X, Vs))
  rule holds(.ValList) => .K
  rule holds((_:Val, _:ValList)) => backtrack

  rule ifNotHoldPr(X:Id, Vs:ValList) => notHolds(try(callRel(X, Vs)))
  rule notHolds(FAIL) => .K
  rule notHolds(ok(_:ValList)) => backtrack

  //   rule Eval_prem/letpr
  rule letPr(El:Exp, V:Val) => assignExp(El, V)

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
  rule iterPrOpt(Bound:VariList, Bind:VariList, P:Prem, Vs:ValList)
    => bindOpts(Bind, inScope(bindRow(Bound, unwrapSomeOpts(Vs)) ~> P ~> readVaris(Bind)))
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
  rule premRows(_:VariList, _:VariList, _:Prem, .ValListList) => .ValListList
  rule premRows(Bound:VariList, Bind:VariList, P:Prem, (Row:ValList ; Rows:ValListList))
    => consRow(inScope(bindRow(Bound, Row) ~> P ~> readVaris(Bind)),
               premRows(Bound, Bind, P, Rows))

  //   rule Eval_prem/dbg
  rule debugPr(V:Val) => logDebug(V)

endmodule
