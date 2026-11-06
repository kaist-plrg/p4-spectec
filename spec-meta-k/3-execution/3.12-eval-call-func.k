// Meta-function invocation: clauses, table rows, defined functions, and
// Call_func.
//
// Mirrors spec-meta/4-execution/4.10-eval-call-func.watsup, except that the
// clauses of a defined function run in order and the first that does not
// mismatch gives the result, without spec-meta's determinism check on the
// later clauses. Table rows are first-match too.

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
  //
  // The clauses in order, each under the context from before it; the first
  // that does not mismatch gives the outcome. `ERROR` passes the catch
  // frames (Eval_clauses/error).
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
  //   rule Eval_clauses/succ: the first success, the later clauses untried
  rule <k> R:Val ~> afterClause(_:ClauseList, _:ValList, F:Map, V:Map) => R ... </k>
       <lfenv> _ => F </lfenv>
       <lvenv> _ => V </lvenv>

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
  //   gives the result, and `ERROR` passes the catch frames. Each row's
  //   frame restores the local layer, so the catch frame holds none
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
  //
  // The clauses run in a frame whose tdenv binds the type parameters; the
  // otherwise clause runs there too, only when every clause mismatched.
  // Only a function with an otherwise clause catches the clauses' `UNMATCH`.
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
  //   rule Call_defined_func/error: a wrong number of type arguments
  rule <k> callDefinedFunc(_:DefinedFuncDefn, _:TArgList, _:ValList) => ERROR ... </k> [owise]

  // `{ (tparam ':' (TYP tparam eps '=' (ALIAS typ)))* }`
  syntax Map ::= calleeTdenv(TParamList, TArgList)
                                            [function, symbol(calleeTdenv)]
  rule calleeTdenv(.TParamList, .TArgList) => .Map
  rule calleeTdenv((X:Id, Xs:TParamList), (T:Typ, Ts:TArgList))
    => (X |-> typD(X, .TParamList, aliasDT(T))) calleeTdenv(Xs, Ts)

  // The otherwise clause, under the context the clauses started from
  syntax UnmatchBoundary ::= afterClauses(Clause, ValList)

  //   rule Call_defined_func/result: `ERROR` passes the catch frame
  rule <k> R:Val ~> afterClauses(_:Clause, _:ValList) => R ... </k>
  //   rule Call_defined_func/unmatch
  rule <k> UNMATCH ~> afterClauses(Cl:Clause, Vs:ValList) => evalClause(Cl, Vs) ... </k>

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
  rule <k> callFuncDefn(_:FuncDefn, _:TArgList, _:ValList) => ERROR ... </k> [owise]

endmodule
