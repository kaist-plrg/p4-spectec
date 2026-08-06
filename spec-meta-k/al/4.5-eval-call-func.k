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
  // `Call_extern_func` is an `extern relation`: left undefined on purpose (see
  // 4.5-eval-call-func.k's sibling 4.6-eval-call-rel.k for the matching note on
  // `Call_extern_rel`), so a spec that reaches one gets a stuck term rather
  // than a silently wrong answer.
  //
  // `Call_builtin_func` is also an `extern relation` in watsup, but for a
  // different reason: its meaning lives in the host interpreter
  // (`p4spec/lib/interface/builtin/`) rather than in the meta-language.  The
  // stdlib builtins are implemented here in `callBuiltinFunc` below, so that a
  // spec using them runs under K too.
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

  // ==========================================================================
  // Call_builtin_func
  //
  // The builtins declared in spec-meta/common/0-stdlib.watsup:
  //
  //   builtin dec $rev_<X>(X*) : X*
  //   builtin dec $assoc_<X, Y>(X, (X, Y)*) : Y?
  //   builtin dec $transpose_<X>(X**) : X**
  //   builtin dec $find_map<K, V>(map<K, V>, K) : V?
  //   builtin dec $find_maps<K, V>(map<K, V>*, K) : V?
  //   builtin dec $add_map<K, V>(map<K, V>, K, V) : map<K, V>
  //   builtin dec $adds_map<K, V>(map<K, V>, K*, V*) : map<K, V>
  //
  // These mirror `p4spec/lib/interface/builtin/{lists,maps}.ml`, which is the
  // authority on their meaning.  Type arguments carry no runtime content here:
  // the OCaml implementations use them only to build the `note` type of the
  // result value, and K values are untyped, so `TypList` is ignored throughout.
  //
  // Keys are compared with `==K`, structural equality on `Val`, matching
  // `Value.eq` in the OCaml (and the `eqOp` case of `cmpopPoly`).
  //
  // A builtin is total: it yields a `Val`, never FAIL.  `$find_map` reports a
  // missing key as `optV(noVal())`, which is the `V?` in its signature.
  // ==========================================================================

  //   `map<K, V>` is `set<pair<K, V>>`, i.e. `` `{ pair* `} ``, and `pair` is
  //   `K ':' V`.  Both are variant types, so a map value is an `injV` whose
  //   mixop is the one the type declaration gives.  These two mixops are
  //   fixed by the stdlib and are built here rather than read off the term.
  //
  //   `` `{ `} `` is `["`{"] ; ["`}"]`; `` ':' `` is `[] ; [":"] ; []` — one
  //   atom group per notation position, empty where an argument goes.
  syntax MixOp ::= "mixopMap" "(" ")"         [function, total, symbol(mixopMap)]
                 | "mixopPair" "(" ")"        [function, total, symbol(mixopPair)]

  rule mixopMap() => ("`{", .AtomList) ; ("`}", .AtomList) ; .MixOp
  rule mixopPair() => .AtomList ; ("':'", .AtomList) ; .AtomList ; .MixOp

  //   The pair list inside a map value.  Partial on purpose: a value that is
  //   not a map leaves the term stuck rather than silently reading as empty,
  //   matching the `error "expected a map"` of `map_of_value`.
  syntax ValList ::= mapPairs(Val)            [function, symbol(mapPairs)]
  rule mapPairs(injV(valCase(M:MixOp, listV(Ps:ValList), .ValList))) => Ps
    requires eqMixOp(M, mixopMap())

  syntax Val ::= mkMap(ValList)               [function, total, symbol(mkMap)]
                | mkPair(Val, Val)            [function, total, symbol(mkPair)]
  rule mkMap(Ps:ValList) => injV(valCase(mixopMap(), listV(Ps), .ValList))
  rule mkPair(K:Val, V:Val) => injV(valCase(mixopPair(), (K, V, .ValList)))

  // --------------------------------------------------------------------------
  //   $find_map<K, V>(map<K, V>, K) : V?
  //
  //   `map_find_opt`: the first pair whose key is equal, or `eps`.
  // --------------------------------------------------------------------------

  syntax ValOpt ::= findPairs(ValList, Val)   [function, total, symbol(findPairs)]
  rule findPairs(.ValList, _:Val) => noVal()
  rule findPairs((injV(valCase(M:MixOp, (K':Val, V:Val, .ValList))), Ps:ValList), K:Val)
    => #if eqMixOp(M, mixopPair()) andBool K' ==K K
       #then someVal(V)
       #else findPairs(Ps, K)
       #fi
  //   A non-pair element is skipped, as in `map_find_opt`'s `| _ ->` branch.
  rule findPairs((_:Val, Ps:ValList), K:Val) => findPairs(Ps, K) [owise]

  rule <k> callBuiltinFunc("find_map", _:TypList, (Vmap:Val, K:Val, .ValList))
        => optV(findPairs(mapPairs(Vmap), K))
       ...
       </k>

  // --------------------------------------------------------------------------
  //   $find_maps<K, V>(map<K, V>*, K) : V?
  //
  //   The first map that has the key wins; `eps` if none does.
  // --------------------------------------------------------------------------

  syntax ValOpt ::= findMaps(ValList, Val)    [function, symbol(findMaps)]
  rule findMaps(.ValList, _:Val) => noVal()
  rule findMaps((Vmap:Val, Vmaps:ValList), K:Val)
    => #if findPairs(mapPairs(Vmap), K) ==K noVal()
       #then findMaps(Vmaps, K)
       #else findPairs(mapPairs(Vmap), K)
       #fi

  rule <k> callBuiltinFunc("find_maps", _:TypList, (listV(Vmaps:ValList), K:Val, .ValList))
        => optV(findMaps(Vmaps, K))
       ...
       </k>

  // --------------------------------------------------------------------------
  //   $add_map<K, V>(map<K, V>, K, V) : map<K, V>
  //
  //   `map_update`: replace the first pair with an equal key *in place*, or
  //   append at the end when the key is absent.  Position matters — the map is
  //   a list, and `$find_map` reads it front-to-back — so rebinding an existing
  //   key must not move it.  This is what makes the fourth `$add_map` in
  //   examples/builtin-map.watsup overwrite "three" with 42 rather than
  //   shadowing it with a second entry.
  // --------------------------------------------------------------------------

  syntax ValList ::= updatePairs(ValList, Val, Val)
                                            [function, total, symbol(updatePairs)]
  rule updatePairs(.ValList, K:Val, V:Val) => mkPair(K, V), .ValList
  rule updatePairs((injV(valCase(M:MixOp, (K':Val, V':Val, .ValList))), Ps:ValList), K:Val, V:Val)
    => #if eqMixOp(M, mixopPair()) andBool K' ==K K
       #then mkPair(K, V), Ps
       #else injV(valCase(M, (K', V', .ValList))), updatePairs(Ps, K, V)
       #fi
  rule updatePairs((P:Val, Ps:ValList), K:Val, V:Val) => P, updatePairs(Ps, K, V) [owise]

  rule <k> callBuiltinFunc("add_map", _:TypList, (Vmap:Val, K:Val, V:Val, .ValList))
        => mkMap(updatePairs(mapPairs(Vmap), K, V))
       ...
       </k>

  //   `$update_map` is the same function in the OCaml; watsup's stdlib does not
  //   declare it, but the interface registers it, so it is accepted here too.
  rule <k> callBuiltinFunc("update_map", _:TypList, (Vmap:Val, K:Val, V:Val, .ValList))
        => mkMap(updatePairs(mapPairs(Vmap), K, V))
       ...
       </k>

  // --------------------------------------------------------------------------
  //   $adds_map<K, V>(map<K, V>, K*, V*) : map<K, V>
  //
  //   `List.fold_left2` over the two lists — so, as in the OCaml, it is
  //   undefined when they differ in length, and the term sticks there.
  // --------------------------------------------------------------------------

  syntax ValList ::= updatePairsAll(ValList, ValList, ValList)
                                            [function, symbol(updatePairsAll)]
  rule updatePairsAll(Ps:ValList, .ValList, .ValList) => Ps
  rule updatePairsAll(Ps:ValList, (K:Val, Ks:ValList), (V:Val, Vs:ValList))
    => updatePairsAll(updatePairs(Ps, K, V), Ks, Vs)

  rule <k> callBuiltinFunc("adds_map", _:TypList,
                           (Vmap:Val, listV(Ks:ValList), listV(Vs:ValList), .ValList))
        => mkMap(updatePairsAll(mapPairs(Vmap), Ks, Vs))
       ...
       </k>

  // --------------------------------------------------------------------------
  //   $rev_<X>(X*) : X*
  //   $transpose_<X>(X**) : X**
  //   $assoc_<X, Y>(X, (X, Y)*) : Y?
  //
  //   All three already exist as functions in common/0-stdlib.k, instantiated
  //   at `val`; these rules only route the builtin call to them.
  // --------------------------------------------------------------------------

  rule <k> callBuiltinFunc("rev_", _:TypList, (listV(Vs:ValList), .ValList))
        => listV(revVal(Vs))
       ...
       </k>

  //   `$transpose_` is `val**`, which arrives as a `listV` of `listV`s;
  //   `transposeVal` works on `ValListList`, so the two are converted either
  //   way around it.
  rule <k> callBuiltinFunc("transpose_", _:TypList, (listV(Vss:ValList), .ValList))
        => listV(valListsOfMatrix(transposeVal(matrixOfValLists(Vss))))
       ...
       </k>

  syntax ValListList ::= matrixOfValLists(ValList)
                                            [function, symbol(matrixOfValLists)]
  rule matrixOfValLists(.ValList) => .ValListList
  rule matrixOfValLists((listV(Vs:ValList), Rest:ValList))
    => Vs ; matrixOfValLists(Rest)

  syntax ValList ::= valListsOfMatrix(ValListList)
                                            [function, total, symbol(valListsOfMatrix)]
  rule valListsOfMatrix(.ValListList) => .ValList
  rule valListsOfMatrix(Vs:ValList ; Vss:ValListList)
    => listV(Vs), valListsOfMatrix(Vss)

  //   `$assoc_` takes pairs as *tuples*, not as the `pair` variant that maps
  //   use — `(X, Y)*` in the signature, `TupleV` in the OCaml.  `assocVal` in
  //   common/0-stdlib.k is instantiated at `atom` keys over `valfield*`, a
  //   different shape, so the tuple form is done here.
  syntax ValOpt ::= assocTup(Val, ValList)    [function, total, symbol(assocTup)]
  rule assocTup(_:Val, .ValList) => noVal()
  rule assocTup(K:Val, (tupV((K':Val, V:Val, .ValList)), Ps:ValList))
    => #if K' ==K K #then someVal(V) #else assocTup(K, Ps) #fi
  rule assocTup(K:Val, (_:Val, Ps:ValList)) => assocTup(K, Ps) [owise]

  rule <k> callBuiltinFunc("assoc_", _:TypList, (K:Val, listV(Ps:ValList), .ValList))
        => optV(assocTup(K, Ps))
       ...
       </k>

endmodule
