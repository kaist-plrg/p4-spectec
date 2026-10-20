// Meta-function invocation: clauses, table rows, dispatch, and Call_func.
//
// Mirrors spec-meta/5.6-eval-call-func.watsup. Every clause of a defined
// function is evaluated and exactly one must succeed (`det` in 3-context.k);
// table rows are still tried in order.

requires "5.4-eval-prem.k"
requires "4.2-extern-ffi.k"

module CALL-FUNC
  imports PREM
  imports EXTERN-FFI

  // ==========================================================================
  // Eval_clause / Eval_clauses
  //
  //   relation Eval_clause: ctx '/' ctx |- clause '@' `( val* `) : res<val>
  //   relation Eval_clauses: ctx '/' ctx |- clause* '@' `( val* `) : res<val>
  // ==========================================================================

  syntax KItem ::= evalClause(Clause, ValList)
                 | evalClauses(ClauseList, ValList, KItem)

  syntax KItem ::= clauseAwaitPremisesK(Exp)
                 | clauseAwaitAssignmentK(Exp, PremList)
                 | clausesAwaitResultK(ClauseList, ValList, KItem, List)

  rule <k> evalClause(clause(As:ArgList, Eout:Exp, Ps:PremList), Vs:ValList)
        => assignArgs(As, Vs) ~> clauseAwaitAssignmentK(Eout, Ps)
       ...
       </k>

  rule <k> assignOK() ~> clauseAwaitAssignmentK(Eout:Exp, Ps:PremList)
        => evalPrems(Ps) ~> clauseAwaitPremisesK(Eout)
       ...
       </k>

  rule <k> premOK() ~> clauseAwaitPremisesK(Eout:Exp) => evalExp(Eout) ... </k>

  //   rule Eval_clauses:
  //     -- (Eval_clause: C_callee '/' C |- clause '@' `( val* `) : valres)*
  //     -- if valres' = $det_valres(valres*)
  //
  // Every clause is one alternative under the callee context as it was at
  // the call; the outcomes are collected for `det`, and the KItem runs when
  // no clause matched.
  rule <k> evalClauses(Cls:ClauseList, Vs:ValList, Else:KItem)
        => clausesAwaitResultK(Cls, Vs, Else, .List)
       ...
       </k>

  rule <k> clausesAwaitResultK((Cl:Clause, Cls:ClauseList), Vs:ValList, Else:KItem, L:List)
        => alt(evalClause(Cl, Vs)) ~> clausesAwaitResultK(Cls, Vs, Else, L)
       ...
       </k>
  rule <k> clausesAwaitResultK(.ClauseList, _:ValList, Else:KItem, L:List) => det(L, Else) ... </k>

  rule <k> R:Res ~> clausesAwaitResultK(Cls:ClauseList, Vs:ValList, Else:KItem, L:List)
        => clausesAwaitResultK(Cls, Vs, Else, L ListItem(R))
       ...
       </k>

  // ==========================================================================
  // Eval_tblrow / Eval_tblrows
  // ==========================================================================

  syntax KItem ::= evalTblRow(TblRow, ValList)
                 | evalTblRows(TblRowList, ValList)

  //   rule Eval_tblrow/succ:
  //     -- if C_callee = C[ .LOCAL = $empty_layer ]
  //     -- Assign_args: C_callee '/' C |- arg* := val* : C_callee'
  //     -- Eval_prems: C_callee' |- prem* : OK C_callee''
  //     -- Eval_exp: C_callee'' |- exp_out : OK val_out

  rule <k> evalTblRow(tblRow(As:ArgList, Eout:Exp, Ps:PremList), Vs:ValList)
        => enterFrame(.Map)
           ~> assignArgs(As, Vs) ~> clauseAwaitAssignmentK(Eout, Ps)
           ~> endFrame
       ...
       </k>

  //   rulegroup Eval_tblrows: in order, the first success wins
  rule <k> evalTblRows(.TblRowList, _:ValList) => backtrack ... </k>
  rule <k> evalTblRows((R:TblRow, Rs:TblRowList), Vs:ValList)
        => alt(evalTblRow(R, Vs)) ~> tblRowsAwaitResultK(Rs, Vs)
       ...
       </k>

  syntax KItem ::= tblRowsAwaitResultK(TblRowList, ValList)
  rule <k> ok(V:Val) ~> tblRowsAwaitResultK(_:TblRowList, _:ValList) => V ... </k>
  rule <k> FAIL ~> tblRowsAwaitResultK(Rs:TblRowList, Vs:ValList) => evalTblRows(Rs, Vs) ... </k>

  // ==========================================================================
  // Call_defined_func
  //
  //   rule Call_defined_func/succ, /fail:
  //     C |- definedFuncDef '@' `< typ* `> `( val* `) : valres
  //     -- if DEF tparam* '->' clause* elsclause? = definedFuncDef
  //     -- if tdenv = `{ (tparam ':' (DEF eps '->' (ALIAS typ)))* `}
  //     -- if C_callee = C[ .LOCAL = $empty_layer[ .TYP = tdenv ] ]
  //     -- Eval_clauses: C_callee '/' C |- clause* '@' `( val* `) : valres
  //     (/fail: valres = FAIL, then Eval_clause on the `otherwise` clause)
  //
  // The `otherwise` clause is outside the determinism check: it runs only
  // when no ordinary clause matched, in the callee's frame.
  // ==========================================================================

  syntax KItem ::= callDefinedFunc(TParamList, ClauseList, ElsClauseOpt, TypList, ValList)

  rule <k> callDefinedFunc(Ps:TParamList, Cls:ClauseList, EC:ElsClauseOpt, Ts:TypList, Vs:ValList)
        => enterFrame(tdenvOfTargs(Ps, Ts))
           ~> evalClauses(Cls, Vs, elsClauseK(EC, Vs))
           ~> endFrame
       ...
       </k>

  syntax KItem ::= elsClauseK(ElsClauseOpt, ValList)
                                            [function, total, symbol(elsClauseK)]
  rule elsClauseK(noElsClause(), _:ValList) => backtrack
  rule elsClauseK(someElsClause(Cl:Clause), Vs:ValList) => evalClause(Cl, Vs)

  syntax Map ::= tdenvOfTargs(TParamList, TypList)
                                            [function, symbol(tdenvOfTargs)]
  rule tdenvOfTargs(.TParamList, .TypList) => .Map
  rule tdenvOfTargs((X:TParam, Xs:TParamList), (T:Typ, Ts:TypList))
    => tdenvOfTargs(Xs, Ts) [{X}:>Id <- defTD(.TParamList, aliasDT(T))]

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

  syntax KItem ::= callFuncDispatch(FuncDef, TArgList, ValList)
                 | callExternFunc(Id, TypList, ValList)
                 | callBuiltinFunc(Id, TypList, ValList)

  rule <k> callFuncDispatch(extFunc(X:Id), Ts:TArgList, Vs:ValList)
        => callExternFunc(X, typsOfTargs(Ts), Vs)
       ...
       </k>
  rule <k> callFuncDispatch(builtinFunc(X:Id, _:TParamList, _:ParamList), Ts:TArgList, Vs:ValList)
        => callBuiltinFunc(X, typsOfTargs(Ts), Vs)
       ...
       </k>
  rule <k> callFuncDispatch(tableFunc(_:ParamList, Rs:TblRowList), .TArgList, Vs:ValList)
        => evalTblRows(Rs, Vs)
       ...
       </k>
  rule <k> callFuncDispatch(defFunc(Ps:TParamList, Cls:ClauseList, EC:ElsClauseOpt),
                            Ts:TArgList, Vs:ValList)
        => callDefinedFunc(Ps, Cls, EC, typsOfTargs(Ts), Vs)
       ...
       </k>

  // ==========================================================================
  // Call_func: look the function up, then dispatch
  //
  //   rule Call_func: C |- id targ* val* : valres
  //     -- if funcdef = $find_func(C, id)
  //     -- Call_func_dispatch: C |- funcdef targ* val* : valres
  // ==========================================================================

  syntax KItem ::= findFunc(Id)
                 | callFuncAwaitFuncK(TArgList, ValList)

  rule <k> callFunc(X:Id, Ts:TArgList, Vs:ValList)
        => findFunc(X) ~> callFuncAwaitFuncK(Ts, Vs)
       ...
       </k>

  rule <k> F:FuncDef ~> callFuncAwaitFuncK(Ts:TArgList, Vs:ValList)
        => callFuncDispatch(F, Ts, Vs)
       ...
       </k>

  rule <k> findFunc(X:Id) => {LF[X]}:>FuncDef ... </k>
       <lfenv> LF:Map </lfenv>
    requires X in_keys(LF)

  rule <k> findFunc(X:Id) => {GF[X]}:>FuncDef ... </k>
       <lfenv> LF:Map </lfenv>
       <gfenv> GF:Map </gfenv>
    requires notBool (X in_keys(LF)) andBool X in_keys(GF)
  rule <k> findFunc(X:Id) => backtrack ... </k>
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
    requires notBool nativeBuiltin(X)

  rule <k> callExternFunc(X:Id, Ts:TypList, Vs:ValList)
        => externReq(JSON2String(jsonOfExternFuncRequest(X, Ts, Vs)),
                     externFuncK())
       ...
       </k>

  // ==========================================================================
  // Native builtins
  // ==========================================================================

  syntax Bool ::= nativeBuiltin(Id)         [function, total, symbol(nativeBuiltin)]
  rule nativeBuiltin("find_map") => true
  rule nativeBuiltin("find_maps") => true
  rule nativeBuiltin("add_map") => true
  rule nativeBuiltin("adds_map") => true
  rule nativeBuiltin("update_map") => true
  rule nativeBuiltin("assoc_") => true
  rule nativeBuiltin(_:Id) => false [owise]

  rule <k> callBuiltinFunc("find_map", _:TypList, (M:Val, K:Val, .ValList))
        => optV(findMap(K, pairsOfMap(M)))
       ...
       </k>
  rule <k> callBuiltinFunc("find_maps", _:TypList, (listV(Ms:ValList), K:Val, .ValList))
        => optV(findMaps(K, Ms))
       ...
       </k>
  rule <k> callBuiltinFunc("add_map", _:TypList, (M:Val, K:Val, V:Val, .ValList))
        => mapOfPairs(updatePairs(pairsOfMap(M), K, V))
       ...
       </k>
  rule <k> callBuiltinFunc("update_map", _:TypList, (M:Val, K:Val, V:Val, .ValList))
        => mapOfPairs(updatePairs(pairsOfMap(M), K, V))
       ...
       </k>
  rule <k> callBuiltinFunc("adds_map", _:TypList,
                           (M:Val, listV(Ks:ValList), listV(Vs:ValList), .ValList))
        => mapOfPairs(updatePairsMany(pairsOfMap(M), Ks, Vs))
       ...
       </k>
  rule <k> callBuiltinFunc("assoc_", _:TypList, (K:Val, listV(Ts:ValList), .ValList))
        => optV(assocTup(K, Ts))
       ...
       </k>

  syntax MixOp ::= "mapMixOp" "(" ")"       [function, total]
                 | "pairMixOp" "(" ")"      [function, total]
  rule mapMixOp() => ("`{", .AtomList) ; ("`}", .AtomList) ; .MixOp
  rule pairMixOp() => .AtomList ; ("':'", .AtomList) ; .AtomList ; .MixOp

  syntax ValList ::= pairsOfMap(Val)        [function, symbol(pairsOfMap)]
  rule pairsOfMap(injV(valCase(_:MixOp, (listV(Ps:ValList), .ValList)))) => Ps

  syntax Val ::= mapOfPairs(ValList)        [function, total, symbol(mapOfPairs)]
               | mkPair(Val, Val)           [function, total, symbol(mkPair)]
  rule mapOfPairs(Ps:ValList) => injV(valCase(mapMixOp(), (listV(Ps), .ValList)))
  rule mkPair(K:Val, V:Val) => injV(valCase(pairMixOp(), (K, V, .ValList)))

  // dec $find_map<K, V>(map<K, V>, K) : V?
  syntax ValOpt ::= findMap(Val, ValList)   [function, total, symbol(findMap)]
  rule findMap(_:Val, .ValList) => noVal()
  rule findMap(K:Val, (injV(valCase(_:MixOp, (K':Val, V:Val, .ValList))), Ps:ValList))
    => #if K ==K K' #then someVal(V) #else findMap(K, Ps) #fi
  rule findMap(K:Val, (_:Val, Ps:ValList)) => findMap(K, Ps) [owise]

  // dec $find_maps<K, V>(map<K, V>*, K) : V?
  syntax ValOpt ::= findMaps(Val, ValList)  [function, symbol(findMaps)]
                  | findMapsElse(ValOpt, Val, ValList)
                                            [function, symbol(findMapsElse)]
  rule findMaps(_:Val, .ValList) => noVal()
  rule findMaps(K:Val, (M:Val, Ms:ValList)) => findMapsElse(findMap(K, pairsOfMap(M)), K, Ms)
  rule findMapsElse(someVal(V:Val), _:Val, _:ValList) => someVal(V)
  rule findMapsElse(noVal(), K:Val, Ms:ValList) => findMaps(K, Ms)

  // dec $add_map<K, V>(map<K, V>, K, V) : map<K, V>
  // dec $update_map<K, V>(map<K, V>, K, V) : map<K, V>
  syntax ValList ::= updatePairs(ValList, Val, Val)
                                            [function, total, symbol(updatePairs)]
  rule updatePairs(.ValList, K:Val, V:Val) => mkPair(K, V), .ValList
  rule updatePairs((injV(valCase(_:MixOp, (K':Val, _:Val, .ValList))), Ps:ValList), K:Val, V:Val)
    => mkPair(K, V), Ps
    requires K ==K K'
  rule updatePairs((P:Val, Ps:ValList), K:Val, V:Val) => P, updatePairs(Ps, K, V) [owise]

  // dec $adds_map<K, V>(map<K, V>, K*, V*) : map<K, V>
  syntax ValList ::= updatePairsMany(ValList, ValList, ValList)
                                            [function, symbol(updatePairsMany)]
  rule updatePairsMany(Ps:ValList, .ValList, .ValList) => Ps
  rule updatePairsMany(Ps:ValList, (K:Val, Ks:ValList), (V:Val, Vs:ValList))
    => updatePairsMany(updatePairs(Ps, K, V), Ks, Vs)

  // dec $assoc_<X, Y>(X, (X, Y)*) : Y?
  syntax ValOpt ::= assocTup(Val, ValList)  [function, total, symbol(assocTup)]
  rule assocTup(_:Val, .ValList) => noVal()
  rule assocTup(K:Val, (tupV(K':Val, V:Val, .ValList), Ts:ValList))
    => #if K ==K K' #then someVal(V) #else assocTup(K, Ts) #fi
  rule assocTup(K:Val, (_:Val, Ts:ValList)) => assocTup(K, Ts) [owise]

endmodule
