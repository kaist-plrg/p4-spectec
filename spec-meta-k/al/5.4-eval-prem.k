// Premise evaluation.
//
// Mirrors spec-meta/al/5.5-eval-prem.watsup: Eval_prem, Eval_prems.

requires "5.3-eval-exp.k"

module AL-PREM
  imports AL-EVAL-EXP

  syntax KItem ::= premOK()                 [symbol(premOK)]

  syntax KItem ::= callRel(Id, ValList)

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
  rule <k> FAIL ~> relPrAwaitArgsK(_:Id, _:ExpList) => FAIL ... </k>

  syntax KItem ::= relPrAwaitOutputsK(ExpList)
  rule <k> Vouts:ValList ~> relPrAwaitOutputsK(Eouts:ExpList)
        => assignExps(Eouts, Vouts) ~> premAwaitAssignmentK()
       ...
       </k>
  rule <k> FAIL ~> relPrAwaitOutputsK(_:ExpList) => FAIL ... </k>

  syntax KItem ::= "premAwaitAssignmentK" "(" ")"
  rule <k> assignOK() ~> premAwaitAssignmentK() => premOK() ... </k>
  rule <k> FAIL ~> premAwaitAssignmentK() => FAIL ... </k>

  //   rulegroup Eval_prem/ifpr
  syntax KItem ::= "ifPrAwaitConditionK" "(" ")"
  rule <k> evalPrem(ifPr(E:Exp)) => evalExp(E) ~> ifPrAwaitConditionK() ... </k>
  rule <k> boolV(true) ~> ifPrAwaitConditionK() => premOK() ... </k>
  rule <k> boolV(false) ~> ifPrAwaitConditionK() => FAIL ... </k>
  rule <k> FAIL ~> ifPrAwaitConditionK() => FAIL ... </k>

  //   rulegroup Eval_prem/ifholdpr
  //
  //   rule Eval_prem/hold:    IFHOLD ... needs Call_rel to succeed with `OK eps`
  //   rule Eval_prem/nothold: IFNOTHOLD ... needs Call_rel to FAIL
  syntax KItem ::= holdPrAwaitArgsK(Id) | notHoldPrAwaitArgsK(Id)
  rule <k> evalPrem(ifHoldPr(X:Id, Es:ExpList)) => evalExps(Es) ~> holdPrAwaitArgsK(X) ... </k>
  rule <k> Vs:ValList ~> holdPrAwaitArgsK(X:Id) => callRel(X, Vs) ~> holdPrAwaitOutputsK() ... </k>
  rule <k> FAIL ~> holdPrAwaitArgsK(_:Id) => FAIL ... </k>

  syntax KItem ::= "holdPrAwaitOutputsK" "(" ")"
  rule <k> .ValList ~> holdPrAwaitOutputsK() => premOK() ... </k>
  rule <k> FAIL ~> holdPrAwaitOutputsK() => FAIL ... </k>
  rule <k> (_:Val, _:ValList) ~> holdPrAwaitOutputsK() => FAIL ... </k>

  rule <k> evalPrem(ifNotHoldPr(X:Id, Es:ExpList)) => evalExps(Es) ~> notHoldPrAwaitArgsK(X) ... </k>
  rule <k> Vs:ValList ~> notHoldPrAwaitArgsK(X:Id) => callRel(X, Vs) ~> notHoldPrAwaitOutputsK() ... </k>
  rule <k> FAIL ~> notHoldPrAwaitArgsK(_:Id) => FAIL ... </k>

  syntax KItem ::= "notHoldPrAwaitOutputsK" "(" ")"
  rule <k> FAIL ~> notHoldPrAwaitOutputsK() => premOK() ... </k>
  rule <k> _:ValList ~> notHoldPrAwaitOutputsK() => FAIL ... </k>

  //   rule Eval_prem/letpr:
  //     C |- LET exp_l '=' exp_r : OK C'
  //     -- Eval_exp: C |- exp_r : OK val_r
  //     -- Assign_exp: C |- exp_l := val_r : C'
  syntax KItem ::= letPrAwaitValueK(Exp)
  rule <k> evalPrem(letPr(El:Exp, Er:Exp)) => evalExp(Er) ~> letPrAwaitValueK(El) ... </k>
  rule <k> V:Val ~> letPrAwaitValueK(El:Exp) => assignExp(El, V) ~> premAwaitAssignmentK() ... </k>
  rule <k> FAIL ~> letPrAwaitValueK(_:Exp) => FAIL ... </k>

  //   rulegroup Eval_prem/iterpr-opt
  syntax KItem ::= evalIterPrOpt(VariList, VariList, Prem)
  rule <k> evalPrem(iterPr(P:Prem, iterPrem(quest(), Bound:VariList, Bind:VariList)))
        => subOptRow(Bound) ~> evalIterPrOpt(Bound, Bind, P)
       ...
       </k>

  //   rule Eval_prem/none:
  //     -- if eps = $sub_opt(C, vari_bound*)
  //     -- if val_bind* = $repeat_<val>(OPT eps, |vari_bind*|)
  //     -- if (vari_bind_iter = $iter_vari(vari_bind, QUEST))*
  //     -- if C_res = $add_varis(C, vari_bind_iter*, val_bind*)
  rule <k> noRowOpt() ~> evalIterPrOpt(_:VariList, Bind:VariList, _:Prem) => premOK() ... </k>
       <lvenv> M:Map => addsMap(M, iterVaris(Bind, quest()),
                                repeatVal(optV(noVal()), lengthVaris(Bind))) </lvenv>

  //   rule Eval_prem/some:
  //     -- if C_sub = $sub_opt(C, vari_bound*)
  //     -- if (vari_bind_iter = $iter_vari(vari_bind, QUEST))*
  //     -- Eval_prem: C_sub |- prem : OK C_sub_res
  //     -- if (val_bind = $find_vari(C_sub_res, vari_bind))*
  //     -- if C_res = $add_varis(C, vari_bind_iter*, (OPT val_bind)*)
  rule <k> someRowOpt(Row:ValList) ~> evalIterPrOpt(Bound:VariList, Bind:VariList, P:Prem)
        => pushLocalSave() ~> bindRow(Bound, Row) ~> evalPrem(P)
           ~> collectIterPrOpt(Bind)
       ...
       </k>

  syntax KItem ::= collectIterPrOpt(VariList)
  rule <k> premOK() ~> collectIterPrOpt(Bind:VariList)
        => popLocalSave() ~> bindOptResults(Bind, findVaris(Bind, M))
       ...
       </k>
       <lvenv> M:Map </lvenv>
  rule <k> FAIL ~> collectIterPrOpt(_:VariList) => popLocalSave() ~> FAIL ... </k>

  syntax KItem ::= bindOptResults(VariList, ValList)
  rule <k> bindOptResults(Bind:VariList, Vs:ValList) => premOK() ... </k>
       <lvenv> M:Map => addsMap(M, iterVaris(Bind, quest()), someVals(Vs)) </lvenv>

  //   rule Eval_prem/empty, /list
  syntax KItem ::= evalIterPrList(VariList, VariList, Prem)
  rule <k> evalPrem(iterPr(P:Prem, iterPrem(star(), Bound:VariList, Bind:VariList)))
        => subListRows(Bound) ~> evalIterPrList(Bound, Bind, P)
       ...
       </k>

  //   rule Eval_prem/empty
  rule <k> .ValListList ~> evalIterPrList(_:VariList, Bind:VariList, _:Prem) => premOK() ... </k>
       <lvenv> M:Map => addsMap(M, iterVaris(Bind, star()),
                                repeatVal(listV(.ValList), lengthVaris(Bind))) </lvenv>

  //   rule Eval_prem/list:
  //     -- if C_sub* = $sub_list(C, vari_bound*)
  //     -- (Eval_prem: C_sub |- prem : OK C_sub_res)*
  //     -- if (val_sub_bind* = $find_varis(C_sub_res, vari_bind*))*
  //     -- if val_bind** = $transpose_<val>(val_sub_bind**)
  //     -- if (val_res_bind = LIST val_bind*)*
  //     -- if C_res = $add_varis(C, vari_bind_iter*, val_res_bind*)
  rule <k> Rows:ValListList ~> evalIterPrList(Bound:VariList, Bind:VariList, P:Prem)
        => pushLocalSave() ~> evalPremEachRow(Bound, Bind, P, Rows, .List)
       ...
       </k>
    requires Rows =/=K .ValListList

  syntax KItem ::= evalPremEachRow(VariList, VariList, Prem, ValListList, List)
  rule <k> evalPremEachRow(_:VariList, Bind:VariList, _:Prem, .ValListList, Acc:List)
        => popLocalSave() ~> finishIterPrList(Bind, Acc)
       ...
       </k>
  rule <k> evalPremEachRow(Bound:VariList, Bind:VariList, P:Prem,
                          (Row:ValList ; Rows:ValListList), Acc:List)
        => peekLocalSave() ~> bindRow(Bound, Row) ~> evalPrem(P)
           ~> collectPremRow(Bound, Bind, P, Rows, Acc)
       ...
       </k>

  syntax KItem ::= collectPremRow(VariList, VariList, Prem, ValListList, List)
  rule <k> premOK() ~> collectPremRow(Bound:VariList, Bind:VariList, P:Prem,
                                      Rows:ValListList, Acc:List)
        => evalPremEachRow(Bound, Bind, P, Rows, Acc ListItem(findVaris(Bind, M)))
       ...
       </k>
       <lvenv> M:Map </lvenv>
  rule <k> FAIL ~> collectPremRow(_:VariList, _:VariList, _:Prem, _:ValListList, _:List)
        => popLocalSave() ~> FAIL
       ...
       </k>

  // Once every row succeeded, transpose the accumulated per-row bindings into
  // one `LIST`-valued binding per `vari` in `Bind`, added onto the base <lvenv>.
  syntax KItem ::= finishIterPrList(VariList, List)
  rule <k> finishIterPrList(Bind:VariList, Acc:List) => premOK() ... </k>
       <lvenv> M:Map => addsMap(M, iterVaris(Bind, star()), transposeToLists(Bind, Acc)) </lvenv>

  //   rule Eval_prem/dbg:
  //     C |- DEBUG exp : OK C
  //     -- Eval_exp: C |- exp : OK val
  //     -- debug val
  syntax KItem ::= "dbgPrAwaitValueK" "(" ")"
  rule <k> evalPrem(debugPr(E:Exp)) => evalExp(E) ~> dbgPrAwaitValueK() ... </k>
  rule <k> V:Val ~> dbgPrAwaitValueK() => logDebug(V) ~> premOK() ... </k>
  rule <k> FAIL ~> dbgPrAwaitValueK() => FAIL ... </k>

  //   rule Eval_prem/fail
  rule <k> evalPrem(_:Prem) => FAIL ... </k> [owise]

  // ==========================================================================
  // Eval_prems: ctx |- prem* : res<ctx>
  // ==========================================================================

  syntax KItem ::= evalPrems(PremList)

  //   rule Eval_prems/empty
  rule <k> evalPrems(.PremList) => premOK() ... </k>

  //   rule Eval_prems/head-fail, /head-succ
  syntax KItem ::= premsAwaitHeadK(PremList)
  rule <k> evalPrems((P:Prem, Ps:PremList)) => evalPrem(P) ~> premsAwaitHeadK(Ps) ... </k>
  rule <k> premOK() ~> premsAwaitHeadK(Ps:PremList) => evalPrems(Ps) ... </k>
  rule <k> FAIL ~> premsAwaitHeadK(_:PremList) => FAIL ... </k>

endmodule
