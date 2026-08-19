// Meta-function invocation: clauses, table rows, dispatch, and Call_func.
//
// Mirrors spec-meta/al/5.6-eval-call-func.watsup: Eval_clause, Eval_clauses,
// Eval_tblrow, Eval_tblrows, Call_table_func, Call_defined_func,
// Call_func_dispatch, Call_func.

requires "5.4-eval-prem.k"
requires "4-extern-json.k"

module AL-CALL-FUNC
  imports AL-PREM
  imports AL-EXTERN-JSON

  // ==========================================================================
  // Call-frame stack
  // ==========================================================================

  syntax KItem ::= pushCallFrame(Map)       // new local tdenv for the callee
                 | "popCallFrame" "(" ")"

  //   C_callee = C[ .LOCAL = $empty_layer[ .TYP = tdenv ] ]
  //
  // `<cfenv>` is saved and restored alongside `<local>`.  In watsup,
  // `Assign_arg/fun` reads `C_caller`, a context *passed as an argument* to
  // `Eval_clause` and therefore scoped to exactly one call; K models it with a
  // cell, which is global unless the frame stack restores it.  Without that,
  // any call made between binding a `FUN` parameter and using it overwrites the
  // single slot, so a `def` parameter that is re-passed through recursion --
  // `$find_overloadeds_unnamed` handing `def $get_parameterListIR` to its own
  // recursive call -- looks up a name in whatever frame happened to run last
  // (usually an empty one) and FAILs.
  rule <k> pushCallFrame(T:Map) => .K ... </k>
       <ltdenv> T':Map => T </ltdenv>
       <lfenv> F:Map => .Map </lfenv>
       <lvenv> V:Map => .Map </lvenv>
       <cfenv> CF:Map </cfenv>
       <callstack> .List => ListItem(callSave(localSave(T', F, V), CF)) ... </callstack>

  rule <k> popCallFrame() => .K ... </k>
       <ltdenv> _ => T </ltdenv>
       <lfenv> _ => F </lfenv>
       <lvenv> _ => V </lvenv>
       <cfenv> _ => CF </cfenv>
       <callstack> ListItem(callSave(localSave(T:Map, F:Map, V:Map), CF:Map)) => .List ... </callstack>

  // Snapshot the *caller's* local fenv into <caller>, for Assign_arg/fun.
  //
  // Runs *before* `pushCallFrame`, so `<lfenv>` is still the caller's; the
  // value it writes is what `pushCallFrame` then saves on the callee's behalf
  // and `popCallFrame` puts back on return.
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
  // `Call_extern_func` and `Call_builtin_func` are both `extern relation`s in
  // watsup: their meaning lives outside the meta-language, so K implements
  // neither and calls out over the external interface instead (the transport
  // is in 4-extern-json.k).  They differ only in who answers —
  // `Call_builtin_func` reaches the OCaml builtin registry
  // (`p4spec/lib/interface/builtin/`), while `Call_extern_func` reaches the
  // *lower spec* named by `externArgs()`, mirroring `Make_parametric`.
  //
  // Both are wired below; see 5.6-eval-call-rel.k for the matching
  // `Call_extern_rel`.  One semantic difference is worth keeping in view: a
  // builtin is total and always yields a `Val`, whereas an extern may FAIL.
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
  // `callFunc`'s task shape is declared in 5.3-eval-exp.k (used by
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
  // Call_builtin_func: dispatch to the host interpreter
  //
  // `Call_builtin_func` is an `extern relation` in watsup because a builtin's
  // meaning lives in the host interpreter (`p4spec/lib/interface/builtin/`),
  // not in the meta-language.  K therefore implements none of them: it
  // serializes the call to JSON, shells out to `spectec-boot extern`, and
  // reads the result back.  The OCaml registry is the single authority for
  // what a builtin computes, and all 44 of its entries are reachable from
  // here — not just the stdlib handful a target's `builtin dec` names.
  //
  // The OCaml oracle is the narrower of the two here: `spectec-boot run ...
  // -ali` resolves `Call_builtin_func` against the meta-spec's own functions,
  // so it only runs the eight that spec-meta's stdlib declares.  A target
  // calling, say, `$text_to_int` works under K and fails under the oracle.
  //
  // The codec is in 4-extern-json.k, which documents the wire format; its
  // OCaml counterpart is `p4spec/lib/interface/spectec/ali/extern.ml`.
  //
  // Unlike everywhere else in this port, type arguments are *not* ignored.  A
  // builtin derives the `note` type of the value it returns from its `targs`
  // (`lists.ml:14-16`, `maps.ml:29,67,72`), and the host runs a typed value
  // representation, so `TypList` goes out on the wire faithfully.
  //
  // RUNTIME DEPENDENCY: `./spectec-boot` must exist and runs must start from
  // the repo root.  K rules cannot read environment variables, so unlike
  // `kast-json.sh` there is no `$SPECTEC_BOOT` override available here.  The
  // request file lives under `./.tmp/` for the same reason — a path relative
  // to the working directory, kept inside the project rather than in `/tmp`.
  // ==========================================================================

  //   The transport itself — request file, `#system`, reply decoding — lives
  //   in 4-extern-json.k, next to the codec it serves and shared with the
  //   two extern kinds.  All that is left here is to say what to send and how
  //   the reply should be read.
  rule <k> callBuiltinFunc(X:Id, Ts:TypList, Vs:ValList)
        => externReq(JSON2String(jsonOfRequest(X, Ts, Vs)), builtinArgs(), builtinK())
       ...
       </k>

  // ==========================================================================
  // Call_extern_func: dispatch to the lower spec
  //
  // `Call_extern_func` is the hook by which a spec running under K reaches the
  // externs of the level *below* it.  In OCaml that is `Make_parametric`
  // (`p4spec/lib/backend-boot/spectec.ml:173-215`), which routes the call into
  // a lower runner's `eval_func`; here it goes over the same wire, and to the
  // same subcommand, as a builtin — `spectec-boot extern -spec SPEC`, which
  // loads SPEC and resolves the extern name against its ordinary `dec`s.
  //
  // Unlike a builtin, this can FAIL: the reply is `{"fail": null}` rather than
  // a value, which `externFuncResult` turns into `FAIL` so it backtracks
  // through `tryNextClause` exactly like a failed clause.
  // ==========================================================================

  //   `externArgs` reads `<p4prog>` because the two entries of 6-entry.k have
  //   different externs behind them; see the rules for it in 4-extern-json.k.
  rule <k> callExternFunc(X:Id, Ts:TypList, Vs:ValList)
        => externReq(JSON2String(jsonOfExternFuncRequest(X, Ts, Vs)),
                     externArgs(P4), externFuncK())
       ...
       </k>
       <p4prog> P4:P4Opt </p4prog>

endmodule
