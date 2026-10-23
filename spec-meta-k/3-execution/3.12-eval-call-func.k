// Meta-function invocation: clauses, table rows, dispatch, and Call_func.
//
// Mirrors spec-meta/3-execution/3.11-eval-call-func.watsup. Every clause of
// a defined function is one alternative and exactly one must succeed (`det`
// below, until Step 7 of the port); table rows are tried in order.

requires "3.11-eval-prem.k"
requires "3.04.2-extern-ffi.k"

module CALL-FUNC
  imports PREM
  imports EXTERN-FFI

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
  syntax KItem ::= evalClause(Clause, ValList)
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

  syntax KItem ::= evalTblRow(TblRow, ValList)
                 | evalTblRows(TblRowList, ValList)

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

  syntax KItem ::= callDefinedFunc(TParamList, ClauseList, ElsClauseOpt, TArgList, ValList)

  rule <k> callDefinedFunc(Ps:TParamList, Cls:ClauseList, EC:ElsClauseOpt, Ts:TArgList, Vs:ValList)
        => enterFrame(tdenvOf(Ps, Ts))
           ~> det(clauseTries(Cls, Vs), elsClauseK(EC, Vs))
           ~> endFrame
       ...
       </k>

  syntax KItem ::= elsClauseK(ElsClauseOpt, ValList)
                                            [function, total, symbol(elsClauseK)]
  rule elsClauseK(noElsClause(), _:ValList) => unmatch
  rule elsClauseK(someElsClause(Cl:Clause), Vs:ValList) => evalClause(Cl, Vs)

  // ==========================================================================
  // Call_func_dispatch/extern, /builtin, /table, /defined
  // ==========================================================================

  syntax KItem ::= callFuncDispatch(FuncDefn, TArgList, ValList)
                 | callExternFunc(Id, TypList, ValList)
                 | callBuiltinFunc(Id, TypList, ValList)

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
  rule <k> callFuncDispatch(funcD(_, Ps:TParamList, _, _, Cls:ClauseList, EC:ElsClauseOpt),
                            Ts:TArgList, Vs:ValList)
        => callDefinedFunc(Ps, Cls, EC, Ts, Vs)
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
