// Meta-function invocation: clauses, table rows, dispatch, and Call_func.
//
// Mirrors spec-meta/al/5.6-eval-call-func.watsup: Eval_clause, Eval_clauses,
// Eval_tblrow, Eval_tblrows, Call_table_func, Call_defined_func,
// Call_func_dispatch, Call_func.

requires "4.4-eval-prem.k"

module AL-CALL-FUNC
  imports AL-PREM

  // ==========================================================================
  // Call-frame stack
  // ==========================================================================

  syntax KItem ::= pushCallFrame(Map)       // new local tdenv for the callee
                 | "popCallFrame" "(" ")"

  //   C_callee = C[ .LOCAL = $empty_layer[ .TYP = tdenv ] ]
  rule <k> pushCallFrame(T:Map) => .K ... </k>
       <ltdenv> T':Map => T </ltdenv>
       <lfenv> F:Map => .Map </lfenv>
       <lvenv> V:Map => .Map </lvenv>
       <callstack> .List => ListItem(localSave(T', F, V)) ... </callstack>

  rule <k> popCallFrame() => .K ... </k>
       <ltdenv> _ => T </ltdenv>
       <lfenv> _ => F </lfenv>
       <lvenv> _ => V </lvenv>
       <callstack> ListItem(localSave(T:Map, F:Map, V:Map)) => .List ... </callstack>

  // Snapshot the *caller's* local fenv into <caller>, for Assign_arg/fun.
  syntax KItem ::= "snapshotCaller" "(" ")"
  rule <k> snapshotCaller() => .K ... </k>
       <lfenv> F:Map </lfenv>
       <cfenv> _ => F </cfenv>

  // ==========================================================================
  // Eval_clause / Eval_clauses
  //
  //   relation Eval_clause: ctx '/' ctx |- clause '@' `( val* `) : res<val>
  //   relation Eval_clauses: ctx '/' ctx |- clause* '@' `( val* `) : res<val>
  //
  // `Eval_clause`'s two contexts are already `<local>` (the callee, being
  // built) and `<caller>` (the caller, read-only) by the time this runs — the
  // caller of `Eval_clause(s)` is responsible for `snapshotCaller()` and
  // `pushCallFrame(...)` beforehand (`Call_defined_func`/`Eval_tblrow` do
  // this). A clause that FAILs must not leave its argument bindings behind
  // for the next clause, so each attempt is wrapped in `<saves>` push/restore
  // — the clause list shares one callee call-frame throughout, only its
  // *bindings* within that frame are rolled back between attempts.
  // ==========================================================================

  syntax KItem ::= evalClause(Clause, ValList)
  syntax KItem ::= evalClauses(ClauseList, ValList)

  //   rule Eval_clause/succ, /fail
  syntax KItem ::= clauseBodyK(Exp, PremList)
  rule <k> evalClause(clause(As:ArgList, Eout:Exp, Ps:PremList), Vs:ValList)
        => assignArgs(As, Vs) ~> clauseAssignedK(Eout, Ps)
       ...
       </k>

  syntax KItem ::= clauseAssignedK(Exp, PremList)
  rule <k> assignOK() ~> clauseAssignedK(Eout:Exp, Ps:PremList)
        => evalPrems(Ps) ~> clauseBodyK(Eout, Ps)
       ...
       </k>
  rule <k> FAIL ~> clauseAssignedK(_:Exp, _:PremList) => FAIL ... </k>

  rule <k> premOK() ~> clauseBodyK(Eout:Exp, _:PremList) => evalExp(Eout) ... </k>
  rule <k> FAIL ~> clauseBodyK(_:Exp, _:PremList) => FAIL ... </k>

  //   rulegroup Eval_clauses
  rule <k> evalClauses(.ClauseList, _:ValList) => FAIL ... </k>
  rule <k> evalClauses((Cl:Clause, Cls:ClauseList), Vs:ValList)
        => pushSave() ~> evalClause(Cl, Vs) ~> tryNextClause(Cls, Vs)
       ...
       </k>

  syntax KItem ::= tryNextClause(ClauseList, ValList)
  rule <k> V:Val ~> tryNextClause(_:ClauseList, _:ValList) => popSaveRestore() ~> V ... </k>
  rule <k> FAIL ~> tryNextClause(Cls:ClauseList, Vs:ValList)
        => popSaveRestore() ~> evalClauses(Cls, Vs)
       ...
       </k>

  // ==========================================================================
  // Eval_tblrow / Eval_tblrows / Call_table_func
  // ==========================================================================

  syntax KItem ::= evalTblRow(TblRow, ValList)
  syntax KItem ::= evalTblRows(TblRowList, ValList)

  //   rule Eval_tblrow/succ:
  //     -- if C_callee = C[ .LOCAL = $empty_layer ]
  //     -- Assign_args: C_callee '/' C |- arg* := val* : C_callee'
  //     -- Eval_prems: C_callee' |- prem* : OK C_callee''
  //     -- Eval_exp: C_callee'' |- exp_out : OK val_out
  //
  // Unlike `Call_defined_func`, a table row's callee layer has no type
  // parameters, so `pushCallFrame(.Map)`.
  rule <k> evalTblRow(tblRow(As:ArgList, Eout:Exp, Ps:PremList), Vs:ValList)
        => snapshotCaller() ~> pushCallFrame(.Map)
           ~> assignArgs(As, Vs) ~> clauseAssignedK(Eout, Ps)
           ~> tblRowDone()
       ...
       </k>

  syntax KItem ::= "tblRowDone" "(" ")"
  rule <k> V:Val ~> tblRowDone() => popCallFrame() ~> V ... </k>
  rule <k> FAIL ~> tblRowDone() => popCallFrame() ~> FAIL ... </k>

  //   rulegroup Eval_tblrows
  rule <k> evalTblRows(.TblRowList, _:ValList) => FAIL ... </k>
  rule <k> evalTblRows((R:TblRow, Rs:TblRowList), Vs:ValList)
        => evalTblRow(R, Vs) ~> tryNextTblRow(Rs, Vs)
       ...
       </k>

  syntax KItem ::= tryNextTblRow(TblRowList, ValList)
  rule <k> V:Val ~> tryNextTblRow(_:TblRowList, _:ValList) => V ... </k>
  rule <k> FAIL ~> tryNextTblRow(Rs:TblRowList, Vs:ValList) => evalTblRows(Rs, Vs) ... </k>

  //   rule Call_table_func: C |- tableFuncDef '@' `( val* `) : valres
  //     -- if TABLE _ '->' tblrow* = tableFuncDef
  //     -- Eval_tblrows: C |- tblrow* '@' `( val* `) : valres
  // (inlined into Call_func_dispatch/table below, since `tableFuncDef` is
  // already destructured there)

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
           ~> tblRowDone()
       ...
       </k>

  // ``{ (tparam ':' (DEF eps '->' (ALIAS typ)))* `}``
  syntax Map ::= tdenvOfTargs(TParamList, TypList)
                                            [function, symbol(tdenvOfTargs)]
  rule tdenvOfTargs(.TParamList, .TypList) => .Map
  rule tdenvOfTargs((X:Id, Xs:TParamList), (T:Typ, Ts:TypList))
    => tdenvOfTargs(Xs, Ts) [X <- defTD(.TParamList, aliasDT(T))]

  //   $opt_as_seq_<clause>(elsclause?)
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
  //
  // `Call_extern_func`/`Call_builtin_func` are `extern relation`s: left
  // undefined on purpose (see 4.5-eval-call-func.k's sibling 4.6-eval-call-rel.k for the
  // matching note on `Call_extern_rel`), so a spec that reaches one gets a
  // stuck term rather than a silently wrong answer.
  // ==========================================================================

  syntax KItem ::= callFuncDispatch(FuncDef, TypList, ValList)
  syntax KItem ::= callExternFunc(Id, TypList, ValList)
  syntax KItem ::= callBuiltinFunc(Id, TypList, ValList)

  rule <k> callFuncDispatch(extFunc(X:Id), Ts:TypList, Vs:ValList)
        => callExternFunc(X, Ts, Vs)
       ...
       </k>
  rule <k> callFuncDispatch(builtinFunc(X:Id, _:TParamList, _:ParamList), Ts:TypList, Vs:ValList)
        => callBuiltinFunc(X, Ts, Vs)
       ...
       </k>
  //   rule Call_func_dispatch/table: only applies with no type arguments
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
  // Call_func: look the function up (local, then global), then dispatch
  //
  //   rule Call_func: C |- id typ* val* : valres
  //     -- if funcdef = $find_func(C, id)
  //     -- Call_func_dispatch: C |- funcdef typ* val* : valres
  //
  // `callFunc`'s task shape is declared in 4.3-eval-exp.k (used by
  // Eval_exp/call, which precedes this module in dependency order); its rules
  // are added here.
  // ==========================================================================

  rule <k> callFunc(X:Id, Ts:TypList, Vs:ValList) => findFuncLocal(X, Ts, Vs) ... </k>
       <lfenv> LF:Map </lfenv>
    requires X in_keys(LF)
  rule <k> callFunc(X:Id, Ts:TypList, Vs:ValList) => findFuncGlobal(X, Ts, Vs) ... </k>
       <lfenv> LF:Map </lfenv>
    requires notBool (X in_keys(LF))

  syntax KItem ::= findFuncLocal(Id, TypList, ValList)
                 | findFuncGlobal(Id, TypList, ValList)

  rule <k> findFuncLocal(X:Id, Ts:TypList, Vs:ValList) => callFuncDispatch({LF[X]}:>FuncDef, Ts, Vs) ... </k>
       <lfenv> LF:Map </lfenv>

  rule <k> findFuncGlobal(X:Id, Ts:TypList, Vs:ValList) => callFuncDispatch({GF[X]}:>FuncDef, Ts, Vs) ... </k>
       <gfenv> GF:Map </gfenv>
    requires X in_keys(GF)
  //   Call_func's `$find_func` returns `eps` if the id is bound in neither
  //   local nor global fenv; there is no watsup clause for a missing funcdef,
  //   which downstream leaves the call stuck. To keep the port total and let
  //   FAIL propagate visibly instead, an unresolved call is treated as FAIL.
  rule <k> findFuncGlobal(X:Id, _:TypList, _:ValList) => FAIL ... </k>
       <gfenv> GF:Map </gfenv>
    requires notBool (X in_keys(GF))

endmodule
