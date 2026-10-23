// Meta-function invocation: clauses, table rows, dispatch, and Call_func.
//
// Mirrors spec-meta/3-execution/3.11-eval-call-func.watsup. Every clause of
// a defined function is one alternative and exactly one must succeed (`det`
// below, until Step 7 of the port); table rows are tried in order.

requires "3.11-eval-prem.k"

module CALL-FUNC
  imports PREM

  // ==========================================================================
  // Tries and the determinism check over the outcomes of the alternatives
  //
  // A `Try` is a try, or its outcome once evaluated; a list of tries
  // evaluates in order to a list of outcomes. `det(Tries, Else)` is the
  // single success; `Else` when none succeeded; `error` when several did.
  // ==========================================================================

  syntax Try ::= tryAlt(K) | Res
  rule <k> tryAlt(K:K) => try(K) ... </k>

  syntax Tries ::= List{Try, ";"}           [symbol(tries), terminator-symbol(.tries), seqstrict, overload(tries)]
  syntax Ress ::= List{Res, ";"}            [symbol(ress), terminator-symbol(.ress), overload(tries)]
  syntax Tries ::= Ress
  syntax KResult ::= Ress

  syntax KItem ::= det(Tries, KItem)        [strict(1)]
  rule <k> det(Rs:Ress, Else:KItem) => pick(oks(Rs), Else) ... </k>

  syntax KItem ::= pick(List, KItem)
  rule <k> pick(ListItem(R:KItem), _:KItem) => R ... </k>
  rule <k> pick(.List, Else:KItem) => Else ... </k>
  rule <k> pick(ListItem(_) ListItem(_) _:List, _:KItem) => error ... </k>

  // The `OK` outcomes, in order.
  syntax List ::= oks(Ress)                 [function, total, symbol(oks)]
  rule oks(.Ress) => .List
  rule oks(ok(R:KItem) ; Rs:Ress) => ListItem(R) oks(Rs)
  rule oks(_:Failure ; Rs:Ress) => oks(Rs)

  // The first success of alternatives tried in order.
  syntax KItem ::= firstOf(Try, KItem)      [strict(1)]
  rule <k> firstOf(ok(R:KItem), _:KItem) => R ... </k>
  rule <k> firstOf(_:Failure, Next:KItem) => Next ... </k>

  // ==========================================================================
  // Eval_clause / Eval_clauses
  // ==========================================================================

  //   rule Eval_clause/succ: the arguments assigned, the premises, the result
  rule <k> evalClause(clause(As:ArgList, Eout:Exp, Ps:PremList), Vs:ValList)
        => assignArgs(As, Vs) ~> Ps ~> Eout
       ...
       </k>

  //   rule Eval_clauses: every clause is one try (`det` picks the result)
  syntax Tries ::= clauseTries(ClauseList, ValList)
                                            [function, total, symbol(clauseTries)]
  rule clauseTries(.ClauseList, _:ValList) => .Tries
  rule clauseTries((Cl:Clause, Cls:ClauseList), Vs:ValList)
    => tryAlt(evalClause(Cl, Vs)) ; clauseTries(Cls, Vs)

  // ==========================================================================
  // Eval_tblrow / Eval_tblrows
  // ==========================================================================

  //   rule Eval_tblrow/succ: like a clause, in a frame of its own
  rule <k> evalTblRow(tblRow(As:ArgList, Eout:Exp, Ps:PremList), Vs:ValList)
        => enterFrame(.Map) ~> assignArgs(As, Vs) ~> Ps ~> Eout ~> endFrame
       ...
       </k>

  //   rulegroup Eval_tblrows: in order, the first success wins
  rule <k> evalTblRows(.TblRowList, _:ValList) => unmatch ... </k>
  rule <k> evalTblRows((R:TblRow, Rs:TblRowList), Vs:ValList)
        => firstOf(tryAlt(evalTblRow(R, Vs)), evalTblRows(Rs, Vs))
       ...
       </k>

  // ==========================================================================
  // Call_defined_func/succ, /fail: the clauses in a frame whose tdenv binds
  // the type parameters. The `otherwise` clause is outside the determinism
  // check: it runs only when no ordinary clause matched, in the same frame.
  // ==========================================================================

  rule <k> callDefinedFunc(funcD(_, Ps:TParamList, _, _, Cls:ClauseList, EC:ElsClauseOpt), Ts:TArgList, Vs:ValList)
        => enterFrame(calleeTdenv(Ps, Ts))
           ~> det(clauseTries(Cls, Vs), elsClauseK(EC, Vs))
           ~> endFrame
       ...
       </k>

  // `{ (tparam ':' (TYP tparam eps '=' (ALIAS typ)))* }`
  syntax Map ::= calleeTdenv(TParamList, TArgList)
                                            [function, symbol(calleeTdenv)]
  rule calleeTdenv(.TParamList, .TArgList) => .Map
  rule calleeTdenv((X:Id, Xs:TParamList), (T:Typ, Ts:TArgList))
    => (X |-> typD(X, .TParamList, aliasDT(T))) calleeTdenv(Xs, Ts)

  syntax KItem ::= elsClauseK(ElsClauseOpt, ValList)
                                            [function, total, symbol(elsClauseK)]
  rule elsClauseK(noElsClause(), _:ValList) => unmatch
  rule elsClauseK(someElsClause(Cl:Clause), Vs:ValList) => evalClause(Cl, Vs)

  // ==========================================================================
  // Call_func_dispatch/extern, /builtin, /table, /defined
  // ==========================================================================

  syntax KItem ::= callFuncDispatch(FuncDefn, TArgList, ValList)

  rule <k> callFuncDispatch(extFuncD(X:Id, _, _, _), Ts:TArgList, Vs:ValList)
        => callExternFunc(X, typsOfTargs(Ts), Vs)
       ...
       </k>
  rule <k> callFuncDispatch(builtinFuncD(X:Id, _, _, _), Ts:TArgList, Vs:ValList)
        => callBuiltinFunc(X, typsOfTargs(Ts), Vs)
       ...
       </k>
  rule <k> callFuncDispatch(tableFuncD(_, _, _, Rs:TblRowList), .TArgList, Vs:ValList)
        => evalTblRows(Rs, Vs)
       ...
       </k>
  rule <k> callFuncDispatch(funcD(_, _, _, _, _, _) #as D:DefinedFuncDefn, Ts:TArgList, Vs:ValList)
        => callDefinedFunc(D, Ts, Vs)
       ...
       </k>
  rule <k> callFuncDispatch(_:FuncDefn, _:TArgList, _:ValList) => error ... </k> [owise]

  // ==========================================================================
  // Call_func: the name resolved through the local function parameters, then
  // `$find_func` in the global environment
  // ==========================================================================

  rule <k> callFunc(X:Id, Ts:TArgList, Vs:ValList) => callFuncNamed(resolveFuncParam(X, LF), Ts, Vs) ... </k>
       <lfenv> LF:Map </lfenv>

  syntax KItem ::= callFuncNamed(Id, TArgList, ValList)
  rule <k> callFuncNamed(X:Id, Ts:TArgList, Vs:ValList) => callFuncDispatch(D, Ts, Vs) ... </k>
       <gfenv> ... X |-> D:FuncDefn ... </gfenv>
  rule <k> callFuncNamed(_:Id, _:TArgList, _:ValList) => error ... </k> [owise]

endmodule
