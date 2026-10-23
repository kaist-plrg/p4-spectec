// Meta-function invocation: clauses, table rows, defined functions, and
// Call_func.
//
// Mirrors spec-meta/3-execution/3.11-eval-call-func.watsup. The clauses of
// a defined function run in order: after the first success every later
// clause must mismatch, so a later error or second success is an error,
// even with equal results. Table rows are first-match.

requires "3.11-eval-prem.k"

module CALL-FUNC
  imports PREM

  // ==========================================================================
  // Eval_clause: ctx |- clause `( val* `) : funcRes
  // ==========================================================================

  //   rule Eval_clause/succ: the arguments assigned, the premises, the result
  rule <k> evalClause(clause(As:ArgList, Eout:Exp, Ps:PremList), Vs:ValList)
        => assignArgs(As, Vs) ~> Ps ~> Eout
       ...
       </k>

  // ==========================================================================
  // Eval_clauses: ctx |- clause* `( val* `) : funcRes
  // Eval_clauses_on_ok: ctx |- clause* `( val* `) : unitRes
  //
  // Each clause is one try, and its outcome selects the next step. The
  // outcome of `evalClauses` is a `Res`, which its caller puts back in
  // flight; `evalClausesOnOk` ends in `.K` (`OK`) or `ERROR`.
  // ==========================================================================

  //   rule Eval_clauses/nil
  rule <k> evalClauses(.ClauseList, _:ValList) => UNMATCH ... </k>
  rule <k> evalClauses((Cl:Clause, Cls:ClauseList), Vs:ValList)
        => try(evalClause(Cl, Vs)) ~> afterClause(Cls, Vs)
       ...
       </k>

  syntax KItem ::= afterClause(ClauseList, ValList)

  //   rule Eval_clauses/unmatch
  rule <k> UNMATCH ~> afterClause(Cls:ClauseList, Vs:ValList) => evalClauses(Cls, Vs) ... </k>
  //   rule Eval_clauses/succ: the remaining clauses must all mismatch
  rule <k> ok(R:KItem) ~> afterClause(Cls:ClauseList, Vs:ValList)
        => evalClausesOnOk(Cls, Vs) ~> onOk(ok(R))
       ...
       </k>
  //   rule Eval_clauses/error
  rule <k> ERROR ~> afterClause(_:ClauseList, _:ValList) => ERROR ... </k>

  // The outcome once the remaining alternatives are all unmatched.
  syntax KItem ::= onOk(Res)
  rule <k> onOk(R:Res) => R ... </k>
  rule <k> ERROR ~> onOk(_:Res) => ERROR ... </k>

  //   rule Eval_clauses_on_ok/nil
  rule <k> evalClausesOnOk(.ClauseList, _:ValList) => .K ... </k>
  rule <k> evalClausesOnOk((Cl:Clause, Cls:ClauseList), Vs:ValList)
        => try(evalClause(Cl, Vs)) ~> afterClauseOnOk(Cls, Vs)
       ...
       </k>

  syntax KItem ::= afterClauseOnOk(ClauseList, ValList)

  //   rule Eval_clauses_on_ok/unmatch
  rule <k> UNMATCH ~> afterClauseOnOk(Cls:ClauseList, Vs:ValList) => evalClausesOnOk(Cls, Vs) ... </k>
  //   rule Eval_clauses_on_ok/error: a second success or an error
  rule <k> ok(_) ~> afterClauseOnOk(_:ClauseList, _:ValList) => ERROR ... </k>
  rule <k> ERROR ~> afterClauseOnOk(_:ClauseList, _:ValList) => ERROR ... </k>

  // ==========================================================================
  // Eval_tblrow: ctx |- tblRow `( val* `) : funcRes
  // Eval_tblrows: ctx |- tblRow* `( val* `) : funcRes
  // Call_table_func: ctx |- tableFuncDefn `( val* `) : funcRes
  // ==========================================================================

  //   rule Eval_tblrow/succ: like a clause, in a frame of its own
  rule <k> evalTblRow(tblRow(As:ArgList, Eout:Exp, Ps:PremList), Vs:ValList)
        => enterFrame(.Map) ~> assignArgs(As, Vs) ~> Ps ~> Eout ~> endFrame
       ...
       </k>

  //   rulegroup Eval_tblrows: in order; the first row that does not mismatch
  //   gives the result
  rule <k> evalTblRows(.TblRowList, _:ValList) => unmatch ... </k>
  rule <k> evalTblRows((R:TblRow, Rs:TblRowList), Vs:ValList)
        => try(evalTblRow(R, Vs)) ~> afterTblRow(Rs, Vs)
       ...
       </k>

  syntax KItem ::= afterTblRow(TblRowList, ValList)
  rule <k> UNMATCH ~> afterTblRow(Rs:TblRowList, Vs:ValList) => evalTblRows(Rs, Vs) ... </k>
  rule <k> ok(V:KItem) ~> afterTblRow(_:TblRowList, _:ValList) => V ... </k>
  rule <k> ERROR ~> afterTblRow(_:TblRowList, _:ValList) => error ... </k>

  //   rule Call_table_func
  rule <k> callTableFunc(tableFuncD(_, _, _, Rs:TblRowList), Vs:ValList) => evalTblRows(Rs, Vs) ... </k>

  // ==========================================================================
  // Call_defined_func: ctx |- definedFuncDefn `< typ* `> `( val* `) : funcRes
  //
  // The clauses run in a frame whose tdenv binds the type parameters; the
  // otherwise clause runs there too, only when every clause mismatched.
  // ==========================================================================

  rule <k> callDefinedFunc(funcD(_, Ps:TParamList, _, _, Cls:ClauseList, EC:ElsClauseOpt), Ts:TArgList, Vs:ValList)
        => enterFrame(calleeTdenv(Ps, Ts))
           ~> evalClauses(Cls, Vs)
           ~> afterClauses(EC, Vs)
           ~> endFrame
       ...
       </k>
    requires sameArity(Ps, Ts)
  //   rule Call_defined_func/error: a wrong number of type arguments
  rule <k> callDefinedFunc(_:DefinedFuncDefn, _:TArgList, _:ValList) => error ... </k> [owise]

  // `{ (tparam ':' (TYP tparam eps '=' (ALIAS typ)))* }`
  syntax Map ::= calleeTdenv(TParamList, TArgList)
                                            [function, symbol(calleeTdenv)]
  rule calleeTdenv(.TParamList, .TArgList) => .Map
  rule calleeTdenv((X:Id, Xs:TParamList), (T:Typ, Ts:TArgList))
    => (X |-> typD(X, .TParamList, aliasDT(T))) calleeTdenv(Xs, Ts)

  syntax KItem ::= afterClauses(ElsClauseOpt, ValList)

  //   rule Call_defined_func/result
  rule <k> ok(R:KItem) ~> afterClauses(_:ElsClauseOpt, _:ValList) => R ... </k>
  rule <k> ERROR ~> afterClauses(_:ElsClauseOpt, _:ValList) => error ... </k>
  //   rule Call_defined_func/unmatch
  rule <k> UNMATCH ~> afterClauses(noElsClause(), _:ValList) => unmatch ... </k>
  rule <k> UNMATCH ~> afterClauses(someElsClause(Cl:Clause), Vs:ValList) => evalClause(Cl, Vs) ... </k>

  // ==========================================================================
  // Call_func: ctx |- id `< targ* `> `( val* `) : funcRes
  //
  // The name is resolved through the local function parameters, its
  // definition found in the global environment, and the call made by the
  // kind of the definition.
  // ==========================================================================

  rule <k> callFunc(X:Id, Ts:TArgList, Vs:ValList)
        => callGlobalFunc(resolveFuncParam(X, LF), Ts, Vs)
       ...
       </k>
       <lfenv> LF:Map </lfenv>

  syntax KItem ::= callGlobalFunc(Id, TArgList, ValList)
  rule <k> callGlobalFunc(X:Id, Ts:TArgList, Vs:ValList) => callFuncDefn(D, Ts, Vs) ... </k>
       <gfenv> ... X |-> D:FuncDefn ... </gfenv>
  //   rule Call_func/error: no such function
  rule <k> callGlobalFunc(_:Id, _:TArgList, _:ValList) => error ... </k> [owise]

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
  //   rule Call_func/table: without type arguments
  rule <k> callFuncDefn(tableFuncD(_, _, _, _) #as D:TableFuncDefn, .TArgList, Vs:ValList)
        => callTableFunc(D, Vs)
       ...
       </k>
  //   rule Call_func/defined
  rule <k> callFuncDefn(funcD(_, _, _, _, _, _) #as D:DefinedFuncDefn, Ts:TArgList, Vs:ValList)
        => callDefinedFunc(D, Ts, Vs)
       ...
       </k>
  //   rule Call_func/error: a table function with type arguments
  rule <k> callFuncDefn(_:FuncDefn, _:TArgList, _:ValList) => error ... </k> [owise]

endmodule
