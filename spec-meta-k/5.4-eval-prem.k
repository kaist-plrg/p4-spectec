// Premise evaluation.
//
// Mirrors spec-meta/5.5-eval-prem.watsup: Eval_prem, Eval_prems. A premise
// is a statement: one that holds rewrites to `.K`, one that does not
// backtracks.

requires "5.3-eval-exp.k"

module PREM
  imports EVAL-EXP

  syntax KItem ::= callRel(Id, ValList)

  // ==========================================================================
  // Eval_prems: ctx |- prem* : res<ctx>
  // ==========================================================================

  rule <k> (P:Prem, Ps:PremList) => evalPrem(P) ~> Ps ... </k>
  rule <k> .PremList => .K ... </k>

  // ==========================================================================
  // Eval_prem: ctx |- prem : res<ctx>
  // ==========================================================================

  syntax KItem ::= evalPrem(Prem)

  //   rule Eval_prem/relpr:
  //     C |- REL id ':' exp* '->' exp_out* : OK C'
  //     -- (Eval_exp: C |- exp : OK val)*
  //     -- Call_rel: C |- id val* : OK val_out*
  //     -- Assign_exps : C |- exp_out* := val_out* : C'
  syntax KItem ::= relPrAwaitArgsK(Id, ExpList)
  rule <k> evalPrem(relPr(X:Id, Es:ExpList, Eouts:ExpList))
        => evalExps(Es) ~> relPrAwaitArgsK(X, Eouts)
       ...
       </k>
  rule <k> Vs:ValList ~> relPrAwaitArgsK(X:Id, Eouts:ExpList)
        => callRel(X, Vs) ~> relPrAwaitOutputsK(Eouts)
       ...
       </k>

  syntax KItem ::= relPrAwaitOutputsK(ExpList)
  rule <k> Vouts:ValList ~> relPrAwaitOutputsK(Eouts:ExpList) => assignExps(Eouts, Vouts) ... </k>

  //   rulegroup Eval_prem/ifpr
  syntax KItem ::= "ifPrAwaitConditionK" "(" ")"
  rule <k> evalPrem(ifPr(E:Exp)) => evalExp(E) ~> ifPrAwaitConditionK() ... </k>
  rule <k> boolV(true) ~> ifPrAwaitConditionK() => .K ... </k>
  rule <k> _:Val ~> ifPrAwaitConditionK() => backtrack ... </k> [owise]

  //   rulegroup Eval_prem/ifholdpr
  //
  //   rule Eval_prem/hold:    IFHOLD ... needs Call_rel to succeed with `OK eps`
  //   rule Eval_prem/nothold: IFNOTHOLD ... needs Call_rel to FAIL
  syntax KItem ::= holdPrAwaitArgsK(Id) | notHoldPrAwaitArgsK(Id)
  rule <k> evalPrem(ifHoldPr(X:Id, Es:ExpList)) => evalExps(Es) ~> holdPrAwaitArgsK(X) ... </k>
  rule <k> Vs:ValList ~> holdPrAwaitArgsK(X:Id) => callRel(X, Vs) ~> holdPrAwaitOutputsK() ... </k>

  syntax KItem ::= "holdPrAwaitOutputsK" "(" ")"
  rule <k> .ValList ~> holdPrAwaitOutputsK() => .K ... </k>
  rule <k> (_:Val, _:ValList) ~> holdPrAwaitOutputsK() => backtrack ... </k>

  // The relation call is one alternative: the premise holds iff it fails.
  rule <k> evalPrem(ifNotHoldPr(X:Id, Es:ExpList)) => evalExps(Es) ~> notHoldPrAwaitArgsK(X) ... </k>
  rule <k> Vs:ValList ~> notHoldPrAwaitArgsK(X:Id) => alt(callRel(X, Vs)) ~> notHoldPrAwaitOutputsK() ... </k>

  syntax KItem ::= "notHoldPrAwaitOutputsK" "(" ")"
  rule <k> FAIL ~> notHoldPrAwaitOutputsK() => .K ... </k>
  rule <k> ok(_:ValList) ~> notHoldPrAwaitOutputsK() => backtrack ... </k>

  //   rule Eval_prem/letpr:
  //     C |- LET exp_l '=' exp_r : OK C'
  //     -- Eval_exp: C |- exp_r : OK val_r
  //     -- Assign_exp: C |- exp_l := val_r : C'
  syntax KItem ::= letPrAwaitValueK(Exp)
  rule <k> evalPrem(letPr(El:Exp, Er:Exp)) => evalExp(Er) ~> letPrAwaitValueK(El) ... </k>
  rule <k> V:Val ~> letPrAwaitValueK(El:Exp) => assignExp(El, V) ... </k>

  //   rulegroup Eval_prem/iterpr-opt
  syntax KItem ::= evalIterPrOpt(VariList, VariList, Prem)
  rule <k> evalPrem(iterPr(P:Prem, iterPrem(quest(), Bound:VariList, Bind:VariList)))
        => subRow(Bound, quest()) ~> evalIterPrOpt(Bound, Bind, P)
       ...
       </k>

  //   rule Eval_prem/none: every bound variable is `OPT eps`
  rule <k> Vs:ValList ~> evalIterPrOpt(_:VariList, Bind:VariList, _:Prem) => .K ... </k>
       <lvenv> M:Map => addsMap(M, iterVaris(Bind, quest()),
                                repeatVal(optV(noVal()), lengthVaris(Bind))) </lvenv>
    requires allNoneOpts(Vs)

  //   rule Eval_prem/some: the premise under the sub-context, then its
  //   bindings as `OPT val`
  rule <k> Vs:ValList ~> evalIterPrOpt(Bound:VariList, Bind:VariList, P:Prem)
        => inScope(bindRow(Bound, unwrapSomeOpts(Vs)) ~> evalPrem(P) ~> readVaris(Bind))
           ~> bindOpts(Bind)
       ...
       </k>
    requires allSomeOpts(Vs)

  syntax KItem ::= bindOpts(VariList)
  rule <k> Vs:ValList ~> bindOpts(Bind:VariList) => .K ... </k>
       <lvenv> M:Map => addsMap(M, iterVaris(Bind, quest()), someVals(Vs)) </lvenv>

  //   rulegroup Eval_prem/iterpr-list
  syntax KItem ::= evalIterPrList(VariList, VariList, Prem)
  rule <k> evalPrem(iterPr(P:Prem, iterPrem(star(), Bound:VariList, Bind:VariList)))
        => subRow(Bound, star()) ~> evalIterPrList(Bound, Bind, P)
       ...
       </k>

  //   rule Eval_prem/empty, /list: the premise under each sub-context in
  //   turn, then the rows of bindings transposed into lists
  rule <k> Vs:ValList ~> evalIterPrList(Bound:VariList, Bind:VariList, P:Prem)
        => premRows(Bound, Bind, P, rows(Vs), .List)
       ...
       </k>

  syntax KItem ::= premRows(VariList, VariList, Prem, ValListList, List)
  rule <k> premRows(_:VariList, Bind:VariList, _:Prem, .ValListList, Acc:List)
        => bindLists(Bind, Acc)
       ...
       </k>
  rule <k> premRows(Bound:VariList, Bind:VariList, P:Prem,
                    (Row:ValList ; Rows:ValListList), Acc:List)
        => inScope(bindRow(Bound, Row) ~> evalPrem(P) ~> readVaris(Bind))
           ~> collectPremRow(Bound, Bind, P, Rows, Acc)
       ...
       </k>

  syntax KItem ::= collectPremRow(VariList, VariList, Prem, ValListList, List)
  rule <k> Vs:ValList ~> collectPremRow(Bound:VariList, Bind:VariList, P:Prem,
                                        Rows:ValListList, Acc:List)
        => premRows(Bound, Bind, P, Rows, Acc ListItem(Vs))
       ...
       </k>

  //   rule Eval_prem/dbg:
  //     C |- DEBUG exp : OK C
  //     -- Eval_exp: C |- exp : OK val
  //     -- debug val
  syntax KItem ::= "dbgPrAwaitValueK" "(" ")"
  rule <k> evalPrem(debugPr(E:Exp)) => evalExp(E) ~> dbgPrAwaitValueK() ... </k>
  rule <k> V:Val ~> dbgPrAwaitValueK() => logDebug(V) ... </k>

endmodule
