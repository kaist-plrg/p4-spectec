// Meta-function invocation.
//
// Mirrors spec-meta/3-execution/3.10-eval-call-func.watsup, except that
// clauses are checked for determinism and table rows are first-match.

requires "3.11-eval-prem.k"

module CALL-FUNC
  imports PREM

  // ==========================================================================
  // Eval_clause: ctx |- clause `( val* `) : funcRes
  // ==========================================================================

  //   rule Eval_clause/succ
  rule <k> evalClause(clause(As:ArgList, Eout:Exp, Ps:PremList), Vs:ValList)
        => assignArgs(As, Vs) ~> Ps ~> Eout
       ...
       </k>

  // ==========================================================================
  // Eval_clauses: ctx |- clause* `( val* `) : funcRes
  // ==========================================================================

  //   rule Eval_clauses/nil
  rule <k> evalClauses(.ClauseList, _:ValList) => UNMATCH ... </k>
  rule <k> evalClauses((Cl:Clause, Cls:ClauseList), Vs:ValList)
        => evalClause(Cl, Vs) ~> afterClause(Cls, Vs, F, V)
       ...
       </k>
       <lfenv> F:Map </lfenv>
       <lvenv> V:Map </lvenv>

  syntax UnmatchBoundary ::= afterClause(ClauseList, ValList, Map, Map)
                                            // lfenv, lvenv

  //   rule Eval_clauses/unmatch
  rule <k> UNMATCH ~> afterClause(Cls:ClauseList, Vs:ValList, F:Map, V:Map) => evalClauses(Cls, Vs) ... </k>
       <lfenv> _ => F </lfenv>
       <lvenv> _ => V </lvenv>
  //   rule Eval_clauses/succ
  // rule <k> R:Val ~> afterClause(_:ClauseList, _:ValList, F:Map, V:Map) => R ... </k>
  //      <lfenv> _ => F </lfenv>
  //      <lvenv> _ => V </lvenv>

  // Determinism check (Eval_clauses_on_ok): replaces the rule above.
  //
  //   rule Eval_clauses/succ: the remaining clauses must all mismatch
  rule <k> R:Val ~> afterClause(Cls:ClauseList, Vs:ValList, F:Map, V:Map)
        => evalClausesOnOk(Cls, Vs) ~> R
       ...
       </k>
       <lfenv> _ => F </lfenv>
       <lvenv> _ => V </lvenv>
  //
  syntax KItem ::= evalClausesOnOk(ClauseList, ValList)
  //
  //   rule Eval_clauses_on_ok/nil
  rule <k> evalClausesOnOk(.ClauseList, _:ValList) => .K ... </k>
  rule <k> evalClausesOnOk((Cl:Clause, Cls:ClauseList), Vs:ValList)
        => evalClause(Cl, Vs) ~> afterClauseOnOk(Cls, Vs, F, V)
       ...
       </k>
       <lfenv> F:Map </lfenv>
       <lvenv> V:Map </lvenv>
  //
  syntax UnmatchBoundary ::= afterClauseOnOk(ClauseList, ValList, Map, Map)
                                            // lfenv, lvenv
  //
  //   rule Eval_clauses_on_ok/unmatch
  rule <k> UNMATCH ~> afterClauseOnOk(Cls:ClauseList, Vs:ValList, F:Map, V:Map) => evalClausesOnOk(Cls, Vs) ... </k>
       <lfenv> _ => F </lfenv>
       <lvenv> _ => V </lvenv>
  //   rule Eval_clauses_on_ok/error: a second success
  rule <k> _:Val ~> afterClauseOnOk(_:ClauseList, _:ValList, _:Map, _:Map) => ERROR ... </k>

  // ==========================================================================
  // Eval_tblrow: ctx |- tblRow `( val* `) : funcRes
  // Eval_tblrows: ctx |- tblRow* `( val* `) : funcRes
  // Call_table_func: ctx |- tableFuncDefn `( val* `) : funcRes
  // ==========================================================================

  //   rule Eval_tblrow/succ
  rule <k> evalTblRow(tblRow(As:ArgList, Eout:Exp, Ps:PremList), Vs:ValList)
        => enterFrame(.Map) ~> assignArgs(As, Vs) ~> Ps ~> Eout ~> endFrame
       ...
       </k>

  //   rulegroup Eval_tblrows
  rule <k> evalTblRows(.TblRowList, _:ValList) => UNMATCH ... </k>
  rule <k> evalTblRows((R:TblRow, Rs:TblRowList), Vs:ValList)
        => evalTblRow(R, Vs) ~> afterTblRow(Rs, Vs)
       ...
       </k>

  syntax UnmatchBoundary ::= afterTblRow(TblRowList, ValList)
  rule <k> UNMATCH ~> afterTblRow(Rs:TblRowList, Vs:ValList) => evalTblRows(Rs, Vs) ... </k>
  rule <k> V:Val ~> afterTblRow(_:TblRowList, _:ValList) => V ... </k>

  //   rule Call_table_func
  rule <k> callTableFunc(tableFuncD(_, _, _, Rs:TblRowList), Vs:ValList) => evalTblRows(Rs, Vs) ... </k>

  // ==========================================================================
  // Call_defined_func: ctx |- definedFuncDefn `< typ* `> `( val* `) : funcRes
  // ==========================================================================

  //   rule Call_defined_func/result, /unmatch: without an otherwise clause
  rule <k> callDefinedFunc(funcD(_, Ps:TParamList, _, _, Cls:ClauseList, noElsClause()), Ts:TArgList, Vs:ValList)
        => enterFrame(calleeTdenv(Ps, Ts))
           ~> evalClauses(Cls, Vs)
           ~> endFrame
       ...
       </k>
    requires sameArity(Ps, Ts)
  //   rule Call_defined_func/result, /unmatch: with an otherwise clause
  rule <k> callDefinedFunc(funcD(_, Ps:TParamList, _, _, Cls:ClauseList, someElsClause(Cl:Clause)), Ts:TArgList, Vs:ValList)
        => enterFrame(calleeTdenv(Ps, Ts))
           ~> evalClauses(Cls, Vs)
           ~> afterClauses(Cl, Vs)
           ~> endFrame
       ...
       </k>
    requires sameArity(Ps, Ts)
  //   rule Call_defined_func/error
  rule <k> callDefinedFunc(_:DefinedFuncDefn, _:TArgList, _:ValList) => ERROR ... </k> [owise]

  // `{ (tparam ':' (TYP tparam eps '=' (ALIAS typ)))* }`
  syntax Map ::= calleeTdenv(TParamList, TArgList)
                                            [function, symbol(calleeTdenv)]
  rule calleeTdenv(.TParamList, .TArgList) => .Map
  rule calleeTdenv((X:Id, Xs:TParamList), (T:Typ, Ts:TArgList))
    => (X |-> typD(X, .TParamList, aliasDT(T))) calleeTdenv(Xs, Ts)

  // The otherwise clause
  syntax UnmatchBoundary ::= afterClauses(Clause, ValList)

  //   rule Call_defined_func/result
  rule <k> R:Val ~> afterClauses(_:Clause, _:ValList) => R ... </k>
  //   rule Call_defined_func/unmatch
  rule <k> UNMATCH ~> afterClauses(Cl:Clause, Vs:ValList) => evalClause(Cl, Vs) ... </k>

  // ==========================================================================
  // Call_func: ctx |- id `< targ* `> `( val* `) : funcRes
  // ==========================================================================

  rule <k> callFunc(X:Id, Ts:TArgList, Vs:ValList)
        => callGlobalFunc(resolveFuncParam(X, LF), Ts, Vs)
       ...
       </k>
       <lfenv> LF:Map </lfenv>

  syntax KItem ::= callGlobalFunc(Id, TArgList, ValList)
  rule <k> callGlobalFunc(X:Id, Ts:TArgList, Vs:ValList) => callFuncDefn(D, Ts, Vs) ... </k>
       <gfenv> ... X |-> D:FuncDefn ... </gfenv>
  //   rule Call_func/error
  rule <k> callGlobalFunc(_:Id, _:TArgList, _:ValList) => ERROR ... </k> [owise]

  syntax KItem ::= callFuncDefn(FuncDefn, TArgList, ValList)

  //   rule Call_func/extern
  rule <k> callFuncDefn(extFuncD(X:Id, _, _, _), Ts:TArgList, Vs:ValList)
        => callExternFunc(X, typsOfTargs(Ts), Vs)
       ...
       </k>
  //   rule Call_func/builtin
  rule <k> callFuncDefn(builtinFuncD(X:Id, _, _, _), Ts:TArgList, Vs:ValList)
        => callBuiltinFunc(X, typsOfTargs(Ts), Vs)
       ...
       </k>
  //   rule Call_func/table
  rule <k> callFuncDefn(tableFuncD(_, _, _, _) #as D:TableFuncDefn, .TArgList, Vs:ValList)
        => callTableFunc(D, Vs)
       ...
       </k>
  //   rule Call_func/defined
  rule <k> callFuncDefn(funcD(_, _, _, _, _, _) #as D:DefinedFuncDefn, Ts:TArgList, Vs:ValList)
        => callDefinedFunc(D, Ts, Vs)
       ...
       </k>
  //   rule Call_func/error
  rule <k> callFuncDefn(_:FuncDefn, _:TArgList, _:ValList) => ERROR ... </k> [owise]

endmodule
