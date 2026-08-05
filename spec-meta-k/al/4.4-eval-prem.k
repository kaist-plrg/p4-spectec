// Premise evaluation.
//
// Mirrors spec-meta/al/5.5-eval-prem.watsup: Eval_prem, Eval_prems.

requires "4.3-eval-exp.k"

module AL-PREM
  imports AL-EVAL-EXP

  syntax KItem ::= premOK()                 [symbol(premOK)]

  // Forward declaration; rules added in 6-call.k.
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
  syntax KItem ::= relPrK1(Id, ExpList)
  rule <k> evalPrem(relPr(X:Id, Es:ExpList, Eouts:ExpList))
        => evalExps(Es) ~> relPrK1(X, Eouts)
       ...
       </k>
  rule <k> Vs:ValList ~> relPrK1(X:Id, Eouts:ExpList) => callRel(X, Vs) ~> relPrK2(Eouts) ... </k>
  rule <k> FAIL ~> relPrK1(_:Id, _:ExpList) => FAIL ... </k>

  syntax KItem ::= relPrK2(ExpList)
  rule <k> Vouts:ValList ~> relPrK2(Eouts:ExpList) => assignExps(Eouts, Vouts) ~> outcomeToPrem() ... </k>
  rule <k> FAIL ~> relPrK2(_:ExpList) => FAIL ... </k>

  syntax KItem ::= "outcomeToPrem" "(" ")"
  rule <k> assignOK() ~> outcomeToPrem() => premOK() ... </k>
  rule <k> FAIL ~> outcomeToPrem() => FAIL ... </k>

  //   rulegroup Eval_prem/ifpr
  syntax KItem ::= "ifPrK" "(" ")"
  rule <k> evalPrem(ifPr(E:Exp)) => evalExp(E) ~> ifPrK() ... </k>
  rule <k> boolV(true) ~> ifPrK() => premOK() ... </k>
  rule <k> boolV(false) ~> ifPrK() => FAIL ... </k>
  rule <k> FAIL ~> ifPrK() => FAIL ... </k>

  //   rulegroup Eval_prem/ifholdpr
  //
  //   rule Eval_prem/hold:    IFHOLD ... needs Call_rel to succeed with `OK eps`
  //   rule Eval_prem/nothold: IFNOTHOLD ... needs Call_rel to FAIL
  syntax KItem ::= holdPrK1(Id) | notHoldPrK1(Id)
  rule <k> evalPrem(ifHoldPr(X:Id, Es:ExpList)) => evalExps(Es) ~> holdPrK1(X) ... </k>
  rule <k> Vs:ValList ~> holdPrK1(X:Id) => callRel(X, Vs) ~> holdPrK2() ... </k>
  rule <k> FAIL ~> holdPrK1(_:Id) => FAIL ... </k>

  syntax KItem ::= "holdPrK2" "(" ")"
  rule <k> .ValList ~> holdPrK2() => premOK() ... </k>
  rule <k> FAIL ~> holdPrK2() => FAIL ... </k>
  rule <k> (_:Val, _:ValList) ~> holdPrK2() => FAIL ... </k>

  rule <k> evalPrem(ifNotHoldPr(X:Id, Es:ExpList)) => evalExps(Es) ~> notHoldPrK1(X) ... </k>
  rule <k> Vs:ValList ~> notHoldPrK1(X:Id) => callRel(X, Vs) ~> notHoldPrK2() ... </k>
  rule <k> FAIL ~> notHoldPrK1(_:Id) => FAIL ... </k>

  syntax KItem ::= "notHoldPrK2" "(" ")"
  rule <k> FAIL ~> notHoldPrK2() => premOK() ... </k>
  rule <k> _:ValList ~> notHoldPrK2() => FAIL ... </k>

  //   rule Eval_prem/letpr:
  //     C |- LET exp_l '=' exp_r : OK C'
  //     -- Eval_exp: C |- exp_r : OK val_r
  //     -- Assign_exp: C |- exp_l := val_r : C'
  syntax KItem ::= letPrK(Exp)
  rule <k> evalPrem(letPr(El:Exp, Er:Exp)) => evalExp(Er) ~> letPrK(El) ... </k>
  rule <k> V:Val ~> letPrK(El:Exp) => assignExp(El, V) ~> outcomeToPrem() ... </k>
  rule <k> FAIL ~> letPrK(_:Exp) => FAIL ... </k>

  //   rule Eval_prem/dbg:
  //     C |- DEBUG exp : OK C
  //     -- Eval_exp: C |- exp : OK val
  //     -- debug val
  syntax KItem ::= "dbgPrK" "(" ")"
  rule <k> evalPrem(debugPr(E:Exp)) => evalExp(E) ~> dbgPrK() ... </k>
  rule <k> V:Val ~> dbgPrK() => premOK() ... </k>
       <log> L:List => L ListItem(V) </log>
  rule <k> FAIL ~> dbgPrK() => FAIL ... </k>

  // ==========================================================================
  // Iteration premises
  //
  //   rulegroup Eval_prem/iterpr-opt
  //   rulegroup Eval_prem/iterpr-list
  //
  // Uses the same row machinery as Eval_exp/opt and /list
  // (subOptRow/subListRows, pushSave/peekRestore/popSaveRestore), since
  // `$sub_opt`/`$sub_list` are the same helpers there.
  // ==========================================================================

  //   rule Eval_prem/none, /some
  syntax KItem ::= evalIterPrOpt(VariList, VariList, Prem)
  rule <k> evalPrem(iterPr(P:Prem, iterPrem(quest(), Bound:VariList, Bind:VariList)))
        => subOptRow(Bound) ~> evalIterPrOpt(Bound, Bind, P)
       ...
       </k>

  //   rule Eval_prem/none:
  //     -- if eps = $sub_opt(C, vari_bound*)
  //     -- if val_bind* = $repeat_<val>(OPT eps, |vari_bind*|)
  //     -- if C_res = $add_varis(C, vari_bind_iter*, val_bind*)
  rule <k> noRowOpt() ~> evalIterPrOpt(_:VariList, Bind:VariList, _:Prem) => premOK() ... </k>
       <lvenv> M:Map => addsMap(M, iterVaris(Bind, quest()),
                                repeatVal(optV(noVal()), lengthVaris(Bind))) </lvenv>

  //   rule Eval_prem/some:
  //     -- if C_sub = $sub_opt(C, vari_bound*)
  //     -- Eval_prem: C_sub |- prem : OK C_sub_res
  //     -- if (val_bind = $find_vari(C_sub_res, vari_bind))*
  //     -- if C_res = $add_varis(C, vari_bind_iter*, (OPT val_bind)*)
  rule <k> someRowOpt(Row:ValList) ~> evalIterPrOpt(Bound:VariList, Bind:VariList, P:Prem)
        => pushSave() ~> bindRow(Bound, Row) ~> evalPrem(P)
           ~> collectIterPrOpt(Bind)
       ...
       </k>

  // `findVarisM(Bind, M)` is computed here, while `<lvenv>` still holds the
  // sub-context result (`C_sub_res`); the extracted `ValList` is then carried
  // as a plain value past `popSaveRestore()`, which switches `<lvenv>` back to
  // the base — matching watsup's `$add_varis(C, ...)`, added to the
  // *original* `C`, not to `C_sub_res`.
  syntax KItem ::= collectIterPrOpt(VariList)
  rule <k> premOK() ~> collectIterPrOpt(Bind:VariList)
        => popSaveRestore() ~> bindOptResults(Bind, findVarisM(Bind, M))
       ...
       </k>
       <lvenv> M:Map </lvenv>
  rule <k> FAIL ~> collectIterPrOpt(_:VariList) => popSaveRestore() ~> FAIL ... </k>

  syntax KItem ::= bindOptResults(VariList, ValList)
  rule <k> bindOptResults(Bind:VariList, Vs:ValList) => premOK() ... </k>
       <lvenv> M:Map => addsMap(M, iterVaris(Bind, quest()), someVals(Vs)) </lvenv>

  // `$find_vari` against an explicit `Map`, used once the sub-context values
  // are wanted while `<lvenv>` still holds them (before restoring).
  syntax ValList ::= findVarisM(VariList, Map)
                                            [function, symbol(findVarisM)]
  rule findVarisM(.VariList, _:Map) => .ValList
  rule findVarisM((V:Vari, Vs:VariList), M:Map) => {M[varrOfVari(V)]}:>Val, findVarisM(Vs, M)
    requires varrOfVari(V) in_keys(M)

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
        => pushSave() ~> evalPremEachRow(Bound, Bind, P, Rows, .List)
       ...
       </k>
    requires Rows =/=K .ValListList

  // Runs `P` once per row (as Eval_exp/list does, restoring to the pushed
  // base before each row, never clearing it — `$sub_list` adds onto `C`).
  // Accumulates each row's `vari_bind*` values (a `ValList`, one per bound
  // variable) into `Acc`, in row order, so they can be transposed once every
  // row is done.
  syntax KItem ::= evalPremEachRow(VariList, VariList, Prem, ValListList, List)
  rule <k> evalPremEachRow(_:VariList, Bind:VariList, _:Prem, .ValListList, Acc:List)
        => popSaveRestore() ~> finishIterPrList(Bind, Acc)
       ...
       </k>
  rule <k> evalPremEachRow(Bound:VariList, Bind:VariList, P:Prem,
                          (Row:ValList ; Rows:ValListList), Acc:List)
        => peekRestore() ~> bindRow(Bound, Row) ~> evalPrem(P)
           ~> collectPremRow(Bound, Bind, P, Rows, Acc)
       ...
       </k>

  syntax KItem ::= collectPremRow(VariList, VariList, Prem, ValListList, List)
  rule <k> premOK() ~> collectPremRow(Bound:VariList, Bind:VariList, P:Prem,
                                      Rows:ValListList, Acc:List)
        => evalPremEachRow(Bound, Bind, P, Rows, Acc ListItem(findVarisM(Bind, M)))
       ...
       </k>
       <lvenv> M:Map </lvenv>
  rule <k> FAIL ~> collectPremRow(_:VariList, _:VariList, _:Prem, _:ValListList, _:List)
        => popSaveRestore() ~> FAIL
       ...
       </k>

  // Once every row succeeded, transpose the accumulated per-row bindings into
  // one `LIST`-valued binding per `vari` in `Bind`, added onto the (already
  // restored) base <lvenv>.
  syntax KItem ::= finishIterPrList(VariList, List)
  rule <k> finishIterPrList(Bind:VariList, Acc:List) => premOK() ... </k>
       <lvenv> M:Map => addsMap(M, iterVaris(Bind, star()), transposeToListsP(Bind, Acc)) </lvenv>

  syntax ValList ::= transposeToListsP(VariList, List)
                                            [function, symbol(transposeToListsP)]
  rule transposeToListsP(Vs:VariList, Acc:List) => transposeToListsP0(Vs, 0, Acc)

  syntax ValList ::= transposeToListsP0(VariList, Int, List)
                                            [function, total, symbol(transposeToListsP0)]
  rule transposeToListsP0(.VariList, _:Int, _:List) => .ValList
  rule transposeToListsP0((_:Vari, Vs:VariList), I:Int, Acc:List)
    => listV(columnAtP(I, Acc)), transposeToListsP0(Vs, I +Int 1, Acc)

  syntax ValList ::= columnAtP(Int, List)   [function, symbol(columnAtP)]
  rule columnAtP(_:Int, .List) => .ValList
  rule columnAtP(I:Int, ListItem(Vs:ValList) Rest:List) => nthVal(Vs, I), columnAtP(I, Rest)

  //   rule Eval_prem/fail: no rule above matched -- otherwise FAIL. The
  //   catch-all below fires only for a `Prem` shape none of the specific
  //   rules matched (there is none among AL's constructors; kept for parity
  //   with watsup's explicit `-- otherwise` rule).
  rule <k> evalPrem(_:Prem) => FAIL ... </k> [owise]

  // ==========================================================================
  // Eval_prems: ctx |- prem* : res<ctx>
  // ==========================================================================

  syntax KItem ::= evalPrems(PremList)

  //   rule Eval_prems/empty
  rule <k> evalPrems(.PremList) => premOK() ... </k>

  //   rule Eval_prems/head-fail, /head-succ
  syntax KItem ::= premsK(PremList)
  rule <k> evalPrems((P:Prem, Ps:PremList)) => evalPrem(P) ~> premsK(Ps) ... </k>
  rule <k> premOK() ~> premsK(Ps:PremList) => evalPrems(Ps) ... </k>
  rule <k> FAIL ~> premsK(_:PremList) => FAIL ... </k>

endmodule
