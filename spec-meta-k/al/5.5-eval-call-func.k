// Meta-function invocation: clauses, table rows, dispatch, and Call_func.
//
// Mirrors spec-meta/al/5.6-eval-call-func.watsup.

requires "5.4-eval-prem.k"
requires "4.2-extern-ffi.k"

module AL-CALL-FUNC
  imports AL-PREM
  imports AL-EXTERN-FFI

  // ==========================================================================
  // Eval_clause / Eval_clauses
  //
  //   relation Eval_clause: ctx '/' ctx |- clause '@' `( val* `) : res<val>
  //   relation Eval_clauses: ctx '/' ctx |- clause* '@' `( val* `) : res<val>
  // ==========================================================================

  syntax KItem ::= evalClause(Clause, ValList)
                 | evalClauses(ClauseList, ValList)

  syntax KItem ::= clauseAwaitPremisesK(Exp)
                 | clauseAwaitAssignmentK(Exp, PremList)
                 | clausesAwaitResultK(ClauseList, ValList)

  rule <k> evalClause(clause(As:ArgList, Eout:Exp, Ps:PremList), Vs:ValList)
        => assignArgs(As, Vs) ~> clauseAwaitAssignmentK(Eout, Ps)
       ...
       </k>

  rule <k> assignOK() ~> clauseAwaitAssignmentK(Eout:Exp, Ps:PremList)
        => evalPrems(Ps) ~> clauseAwaitPremisesK(Eout)
       ...
       </k>
  rule <k> FAIL ~> clauseAwaitAssignmentK(_:Exp, _:PremList) => FAIL ... </k>

  rule <k> premOK() ~> clauseAwaitPremisesK(Eout:Exp) => evalExp(Eout) ... </k>
  rule <k> FAIL ~> clauseAwaitPremisesK(_:Exp) => FAIL ... </k>

  rule <k> evalClauses(.ClauseList, _:ValList) => FAIL ... </k>
  rule <k> evalClauses((Cl:Clause, Cls:ClauseList), Vs:ValList)
        => pushLocalSave() ~> evalClause(Cl, Vs) ~> clausesAwaitResultK(Cls, Vs)
       ...
       </k>

  rule <k> V:Val ~> clausesAwaitResultK(_:ClauseList, _:ValList) => popLocalSave() ~> V ... </k>
  rule <k> FAIL ~> clausesAwaitResultK(Cls:ClauseList, Vs:ValList)
        => popLocalSave() ~> evalClauses(Cls, Vs)
       ...
       </k>

  // ==========================================================================
  // Eval_tblrow / Eval_tblrows
  // ==========================================================================

  syntax KItem ::= evalTblRow(TblRow, ValList)
                 | evalTblRows(TblRowList, ValList)

  syntax KItem ::= "callFrameAwaitResultK" "(" ")"

  //   rule Eval_tblrow/succ:
  //     -- if C_callee = C[ .LOCAL = $empty_layer ]
  //     -- Assign_args: C_callee '/' C |- arg* := val* : C_callee'
  //     -- Eval_prems: C_callee' |- prem* : OK C_callee''
  //     -- Eval_exp: C_callee'' |- exp_out : OK val_out

  rule <k> evalTblRow(tblRow(As:ArgList, Eout:Exp, Ps:PremList), Vs:ValList)
        => snapshotCaller() ~> pushCallFrame(.Map)
           ~> assignArgs(As, Vs) ~> clauseAwaitAssignmentK(Eout, Ps)
           ~> callFrameAwaitResultK()
       ...
       </k>

  rule <k> V:Val ~> callFrameAwaitResultK() => popCallFrame() ~> V ... </k>
  rule <k> FAIL ~> callFrameAwaitResultK() => popCallFrame() ~> FAIL ... </k>

  //   rulegroup Eval_tblrows
  rule <k> evalTblRows(.TblRowList, _:ValList) => FAIL ... </k>
  rule <k> evalTblRows((R:TblRow, Rs:TblRowList), Vs:ValList)
        => evalTblRow(R, Vs) ~> tblRowsAwaitResultK(Rs, Vs)
       ...
       </k>

  syntax KItem ::= tblRowsAwaitResultK(TblRowList, ValList)
  rule <k> V:Val ~> tblRowsAwaitResultK(_:TblRowList, _:ValList) => V ... </k>
  rule <k> FAIL ~> tblRowsAwaitResultK(Rs:TblRowList, Vs:ValList) => evalTblRows(Rs, Vs) ... </k>

  // ==========================================================================
  // Call_defined_func
  //
  //   rule Call_defined_func:
  //     C |- definedFuncDef '@' `< typ* `> `( val* `) : valres
  //     -- if DEF tparam* '->' clause* elsclause? = definedFuncDef
  //     -- if tdenv = `{ (tparam ':' (DEF eps '->' (ALIAS typ)))* `}
  //     -- if C_callee = C[ .LOCAL = $empty_layer[ .TYP = tdenv ] ]
  //     -- if clause_else* = $opt_as_seq_<clause>(elsclause?)
  //     -- if clause_all* = clause* ++ clause_else*
  //     -- Eval_clauses: C_callee '/' C |- clause_all* '@' `( val* `) : valres
  // ==========================================================================

  syntax KItem ::= callDefinedFunc(TParamList, ClauseList, ElsClauseOpt, TypList, ValList)

  rule <k> callDefinedFunc(Ps:TParamList, Cls:ClauseList, EC:ElsClauseOpt, Ts:TypList, Vs:ValList)
        => snapshotCaller() ~> pushCallFrame(tdenvOfTargs(Ps, Ts))
           ~> evalClauses(appendClauses(Cls, optAsSeqClause(EC)), Vs)
           ~> callFrameAwaitResultK()
       ...
       </k>

  syntax Map ::= tdenvOfTargs(TParamList, TypList)
                                            [function, symbol(tdenvOfTargs)]
  rule tdenvOfTargs(.TParamList, .TypList) => .Map
  rule tdenvOfTargs((X:Id, Xs:TParamList), (T:Typ, Ts:TypList))
    => tdenvOfTargs(Xs, Ts) [X <- defTD(.TParamList, aliasDT(T))]

  syntax ClauseList ::= optAsSeqClause(ElsClauseOpt)
                                            [function, total, symbol(optAsSeqClause)]
                      | appendClauses(ClauseList, ClauseList)
                                            [function, total, symbol(appendClauses)]
  rule optAsSeqClause(noElsClause()) => .ClauseList
  rule optAsSeqClause(someElsClause(Cl:Clause)) => Cl, .ClauseList
  rule appendClauses(.ClauseList, Cs:ClauseList) => Cs
  rule appendClauses((C:Clause, Cs:ClauseList), Ds:ClauseList) => C, appendClauses(Cs, Ds)

  // ==========================================================================
  // Call_func_dispatch
  //
  //   rulegroup Call_func_dispatch {
  //     Call_func_dispatch/extern:  Call_extern_func
  //     Call_func_dispatch/builtin: Call_builtin_func
  //     Call_func_dispatch/table:   Call_table_func
  //     Call_func_dispatch/defined: Call_defined_func
  //   }
  // ==========================================================================

  syntax KItem ::= callFuncDispatch(FuncDef, TypList, ValList)
                 | callExternFunc(Id, TypList, ValList)
                 | callBuiltinFunc(Id, TypList, ValList)

  rule <k> callFuncDispatch(extFunc(X:Id), Ts:TypList, Vs:ValList)
        => callExternFunc(X, Ts, Vs)
       ...
       </k>
  rule <k> callFuncDispatch(builtinFunc(X:Id, _:TParamList, _:ParamList), Ts:TypList, Vs:ValList)
        => callBuiltinFunc(X, Ts, Vs)
       ...
       </k>
  rule <k> callFuncDispatch(tableFunc(_:ParamList, Rs:TblRowList), .TypList, Vs:ValList)
        => evalTblRows(Rs, Vs)
       ...
       </k>
  rule <k> callFuncDispatch(defFunc(Ps:TParamList, Cls:ClauseList, EC:ElsClauseOpt),
                            Ts:TypList, Vs:ValList)
        => callDefinedFunc(Ps, Cls, EC, Ts, Vs)
       ...
       </k>

  // ==========================================================================
  // Call_func: look the function up, then dispatch
  //
  //   rule Call_func: C |- id typ* val* : valres
  //     -- if funcdef = $find_func(C, id)
  //     -- Call_func_dispatch: C |- funcdef typ* val* : valres
  // ==========================================================================

  syntax KItem ::= findFunc(Id)
                 | callFuncAwaitFuncK(TypList, ValList)

  rule <k> callFunc(X:Id, Ts:TypList, Vs:ValList)
        => findFunc(X) ~> callFuncAwaitFuncK(Ts, Vs)
       ...
       </k>

  rule <k> F:FuncDef ~> callFuncAwaitFuncK(Ts:TypList, Vs:ValList)
        => callFuncDispatch(F, Ts, Vs)
       ...
       </k>
  rule <k> FAIL ~> callFuncAwaitFuncK(_:TypList, _:ValList) => FAIL ... </k>

  rule <k> findFunc(X:Id) => {LF[X]}:>FuncDef ... </k>
       <lfenv> LF:Map </lfenv>
    requires X in_keys(LF)

  rule <k> findFunc(X:Id) => {GF[X]}:>FuncDef ... </k>
       <lfenv> LF:Map </lfenv>
       <gfenv> GF:Map </gfenv>
    requires notBool (X in_keys(LF)) andBool X in_keys(GF)
  rule <k> findFunc(X:Id) => FAIL ... </k>
       <lfenv> LF:Map </lfenv>
       <gfenv> GF:Map </gfenv>
    requires notBool (X in_keys(LF)) andBool notBool (X in_keys(GF))

  // ==========================================================================
  // Call_builtin_func / Call_extern_func
  // ==========================================================================

  rule <k> callBuiltinFunc(X:Id, Ts:TypList, Vs:ValList)
        => externReq(JSON2String(jsonOfRequest(X, Ts, Vs)), builtinK())
       ...
       </k>

  rule <k> callExternFunc(X:Id, Ts:TypList, Vs:ValList)
        => externReq(JSON2String(jsonOfExternFuncRequest(X, Ts, Vs)),
                     externFuncK())
       ...
       </k>

endmodule
