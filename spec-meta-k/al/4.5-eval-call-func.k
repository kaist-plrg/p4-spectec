// Meta-function invocation: clauses, table rows, dispatch, and Call_func.
//
// Mirrors spec-meta/al/5.6-eval-call-func.watsup: Eval_clause, Eval_clauses,
// Eval_tblrow, Eval_tblrows, Call_table_func, Call_defined_func,
// Call_func_dispatch, Call_func.

requires "4.4-eval-prem.k"
requires "4.4.5-extern-json.k"

module AL-CALL-FUNC
  imports AL-PREM
  imports AL-EXTERN-JSON

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
  // (`p4spec/lib/interface/builtin/`) rather than in the meta-language.  Rather
  // than reimplement it, `callBuiltinFunc` below calls out to that interpreter
  // over an external interface, which keeps the OCaml authoritative and makes
  // every builtin it registers available under K.
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
  // Call_builtin_func: dispatch to the host interpreter
  //
  // `Call_builtin_func` is an `extern relation` in watsup because a builtin's
  // meaning lives in the host interpreter (`p4spec/lib/interface/builtin/`),
  // not in the meta-language.  K therefore implements none of them: it
  // serializes the call to JSON, shells out to `spectec-boot builtin`, and
  // reads the result back.  The OCaml registry is the single authority for
  // what a builtin computes, and all 44 of its entries are reachable from
  // here — not just the stdlib handful a target's `builtin dec` names.
  //
  // The OCaml oracle is the narrower of the two here: `spectec-boot run ...
  // -ali` resolves `Call_builtin_func` against the meta-spec's own functions,
  // so it only runs the eight that spec-meta's stdlib declares.  A target
  // calling, say, `$text_to_int` works under K and fails under the oracle.
  //
  // The codec is in 4.4.5-extern-json.k, which documents the wire format; its
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

  syntax KItem ::= builtinReq(String)
                 | builtinFile(IOFile, String)
                 | builtinWrite(IOInt, String, String)
                 | builtinRun(String)
                 | builtinResp(Int, String, String)

  //   The request goes through a *file*, never through argv: operator atoms
  //   render with quotes (`':'`), and `#system` passes its argument through a
  //   shell, which fails on those with an unterminated quoted string.  Only
  //   the `#mkstemp` path — `./.tmp/spectec-k-` plus six alphanumerics — is
  //   ever interpolated into the command line.
  //
  //   One file serves the whole run.  `#mkstemp` mints it on the first builtin
  //   call and `<builtinreq>` remembers the path; every later call reuses it,
  //   and K removes it when rewriting ends.  Calling `#mkstemp` per call would
  //   instead leave one file behind per builtin invocation.
  rule <k> callBuiltinFunc(X:Id, Ts:TypList, Vs:ValList)
        => builtinReq(JSON2String(jsonOfRequest(X, Ts, Vs)))
       ...
       </k>

  //   First call: no path yet, so make the file and write through the fd
  //   `#mkstemp` already opened.
  rule <k> builtinReq(Req:String) => builtinFile(#mkstemp(builtinTemplate()), Req) ... </k>
       <builtinreq> "" </builtinreq>

  rule <k> builtinFile(#tempFile(P:String, FD:Int), Req:String)
        => builtinWrite(FD, Req, P)
       ...
       </k>
       <builtinreq> _ => P </builtinreq>

  //   Later calls: reopen the same path in mode `"w"`, which truncates (C
  //   `fopen` semantics), so a short request can never read back the tail of a
  //   longer predecessor.
  rule <k> builtinReq(Req:String) => builtinWrite(#open(P, "w"), Req, P) ... </k>
       <builtinreq> P:String </builtinreq>
    requires P =/=String ""

  //   `#write`/`#close` have sort `K`, so they can only be sequenced directly
  //   in the `<k>` cell — a `strict(1)` wrapper around them does not kompile.
  rule <k> builtinWrite(FD:Int, Req:String, P:String)
        => #write(FD, Req) ~> #close(FD) ~> builtinRun(P)
       ...
       </k>

  rule <k> builtinRun(P:String)
        => #system(builtinCmd() +String " builtin -i '" +String P +String "'")
       ...
       </k>

  //   `#system` is a *function* hook, so its result lands in the `<k>` cell as
  //   a `#systemResult` that a continuation has to pick up.
  rule <k> #systemResult(C:Int, Out:String, Err:String)
        => builtinResp(C, Out, Err)
       ...
       </k>

  rule <k> builtinResp(0, Out:String, _:String)
        => valOfJson(respOk(String2JSON(Out)))
       ...
       </k>

  //   A non-zero exit deliberately has no rule.  A builtin is total in the
  //   meta-language — it yields a `Val`, never FAIL — so a failure response
  //   means a malformed request or a missing implementation, which is a defect
  //   rather than something the spec can recover from.  The stuck term carries
  //   the child's stderr, which is what you want to read.

  syntax String ::= "builtinCmd" "(" ")"      [function, total, symbol(builtinCmd)]
                  | "builtinTemplate" "(" ")" [function, total, symbol(builtinTemplate)]
  rule builtinCmd() => "./spectec-boot"
  //   `#mkstemp` fills the `XXXXXX` in, so concurrent runs in one working
  //   directory get distinct request files.  `./.tmp/` must exist — the
  //   `kast-json.sh` wrapper creates it, since K rules cannot mkdir.
  rule builtinTemplate() => "./.tmp/spectec-k-XXXXXX"

endmodule
