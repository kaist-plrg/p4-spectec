// Relations: their declarations, the extern relations, and the native
// builtins.
//
// Mirrors spec-meta/3-execution/3.03-relation.watsup. A relation of the
// spec is a `<k>` computation here; this module declares the computations
// and the modules that follow give their rules. The context `ctx` is the
// configuration, and a `failure` outcome is a signal in flight
// (3.00-result.k).

requires "3.04.2-extern-ffi.k"

module RELATION
  imports EXTERN-FFI

  // ==========================================================================
  // Extern meta-function and relation invocation
  //
  //   extern relation Call_extern_func:  |- id `< typ* `> `( val* `) : funcRes
  //   extern relation Call_builtin_func: |- id `< typ* `> `( val* `) : funcRes
  //   extern relation Call_extern_rel:   |- id `( val* `) : relRes
  // ==========================================================================

  syntax KItem ::= callExternFunc(Id, TypList, ValList)
                 | callBuiltinFunc(Id, TypList, ValList)
                 | callExternRel(Id, ValList)

  // ==========================================================================
  // Evaluation relations
  //
  //   relation Eval_exp:       ctx |- exp : expRes
  //   relation Eval_exps:      ctx |- exp* : expsRes
  //   relation Eval_exp_maybe: ctx? |- exp : expOptRes
  //   relation Eval_exp_each:  ctx* |- exp : expsRes
  //
  // An `Exp` or `ExpList` at the head of `<k>` evaluates in place by the
  // strictness of 1.0-syntax.k (3.08-eval-exp.k); the optional and list
  // contexts are `optSub`/`listSub` there.
  //
  //   relation Eval_path_acc: ctx |- val path : pathRes
  //   relation Eval_path_upd: ctx |- val path := val : pathRes
  // ==========================================================================

  syntax Exp ::= path(Val, Path)
               | upd(Val, Path, Exp)        [strict(3)]

  //   relation Eval_targs: ctx |- targ* : typsRes    -- within `callE` (3.10)
  //   relation Eval_arg:   ctx |- arg : argRes       -- an `Arg` at the head of <k>
  //   relation Eval_args:  ctx |- arg* : argsRes     -- an `ArgList`
  //
  //   relation Eval_prem:       ctx |- prem : premRes    -- a `Prem` at the head of <k>
  //   relation Eval_prems:      ctx |- prem* : premRes   -- a `PremList`
  //   relation Eval_prem_maybe: ctx? |- prem : premOptRes   -- `iterPrOpt` (3.11)
  //   relation Eval_prem_each:  ctx* |- prem : premIterRes -- `premRows` (3.11)

  // ==========================================================================
  // Assignment relations (3.06)
  //
  //   relation Assign_exp:  ctx |- exp := val : assignRes
  //   relation Assign_exps: ctx |- exp* := val* : assignRes
  //   relation Assign_arg:  ctx |- arg := val : assignRes
  //   relation Assign_args: ctx |- arg* := val* : assignRes
  //
  // A statement: `.K` with the bindings added to `<local>`, or `error`. The
  // values of `assignExps` may still be a computation (a relation's outputs).
  // ==========================================================================

  syntax KItem ::= assignExp(Exp, Val)
                 | assignExps(ExpList, KItem) [strict(2)]
                 | assignArg(Arg, Val)
                 | assignArgs(ArgList, ValList)

  // ==========================================================================
  // Call relations (3.11, 3.12)
  //
  //   relation Eval_clause:        ctx |- clause `( val* `) : funcRes
  //   relation Eval_clauses:       ctx |- clause* `( val* `) : funcRes
  //   relation Eval_clauses_on_ok: ctx |- clause* `( val* `) : unitRes
  //   relation Eval_tblrow:        ctx |- tblRow `( val* `) : funcRes
  //   relation Eval_tblrows:       ctx |- tblRow* `( val* `) : funcRes
  //   relation Call_table_func:    ctx |- tableFuncDefn `( val* `) : funcRes
  //   relation Call_defined_func:  ctx |- definedFuncDefn `< typ* `> `( val* `) : funcRes
  //   relation Call_func:          ctx |- id `< targ* `> `( val* `) : funcRes
  //
  //   syntax rul = rulMatch -> rulPath
  //   relation Eval_rul:          ctx |- rul `( val* `) : relRes
  //   relation Eval_ruls:         ctx |- rul* `( val* `) : relRes
  //   relation Eval_ruls_on_ok:   ctx |- rul* `( val* `) : unitRes
  //   relation Call_defined_rel:  ctx |- definedRelDefn `( val* `) : relRes
  //   relation Call_rel:          ctx |- id `( val* `) : relRes
  // ==========================================================================

  syntax KItem ::= evalClause(Clause, ValList)
                 | evalClauses(ClauseList, ValList)
                 | evalClausesOnOk(ClauseList, ValList)
                 | evalTblRow(TblRow, ValList)
                 | evalTblRows(TblRowList, ValList)
                 | callTableFunc(TableFuncDefn, ValList)
                 | callDefinedFunc(DefinedFuncDefn, TArgList, ValList)
                 | callFunc(Id, TArgList, ValList)

  syntax Rul ::= rul(RulMatch, RulPath)     [symbol(rul)]
  syntax RulList ::= List{Rul, ","}         [symbol(rulList), terminator-symbol(.rulList)]

  syntax KItem ::= evalRul(Rul, ValList)
                 | evalRuls(RulList, ValList)
                 | evalRulsOnOk(RulList, ValList)
                 | callDefinedRel(DefinedRelDefn, ValList)
                 | callRel(Id, ValList)

  // ==========================================================================
  // Call_extern_func / Call_builtin_func / Call_extern_rel: to the host
  // ==========================================================================

  rule <k> callExternFunc(X:Id, Ts:TypList, Vs:ValList)
        => externReq(JSON2String(jsonOfExternFuncRequest(X, Ts, Vs)),
                     externFuncK())
       ...
       </k>

  rule <k> callBuiltinFunc(X:Id, Ts:TypList, Vs:ValList)
        => externReq(JSON2String(jsonOfRequest(X, Ts, Vs)), builtinK())
       ...
       </k>
    requires notBool nativeBuiltin(X)

  rule <k> callExternRel(X:Id, Vs:ValList)
        => externReq(JSON2String(jsonOfExternRelRequest(X, Vs)),
                     externRelK())
       ...
       </k>

  // ==========================================================================
  // Native builtins: the map and association builtins of the object level
  // (spec/), answered here rather than by the host
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
