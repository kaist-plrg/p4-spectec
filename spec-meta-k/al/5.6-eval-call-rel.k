// Relation invocation: rule paths, rule groups, dispatch, and Call_rel.
//
// Mirrors spec-meta/al/5.7-eval-call-rel.watsup: Eval_rul, Eval_ruls,
// Eval_rulgroup, Eval_rulgroups, Call_defined_rel, Call_rel_dispatch,
// Call_rel.

requires "5.5-eval-call-func.k"

module AL-CALL-REL
  imports AL-CALL-FUNC

  // ==========================================================================
  // Eval_rul / Eval_ruls
  //
  //   relation Eval_rul: ctx |- rulmatch -> rulpath '@' `( val* `) : res<val*>
  //   relation Eval_ruls: ctx |- rulmatch -> rulpath* '@' `( val* `) : res<val*>
  // ==========================================================================

  syntax KItem ::= evalRul(RulMatch, RulPath, ValList)
  syntax KItem ::= evalRuls(RulMatch, RulPathList, ValList)

  //   rule Eval_rul/succ:
  //     -- if id '=' exp_out* '-' prem_path* = rulpath
  //     -- if exp* '-' prem_match* = rulmatch
  //     -- Assign_exps: C |- exp* := val* : C'
  //     -- if prem* = prem_match* ++ prem_path*
  //     -- Eval_prems: C' |- prem* : OK C''
  //     -- (Eval_exp: C'' |- exp_out : OK val_out)*
  syntax KItem ::= rulMatchedK(PremList, ExpList)
  rule <k> evalRul(rulMatch(Es:ExpList, PsMatch:PremList),
                   rulPath(_:Id, Eouts:ExpList, PsPath:PremList),
                   Vs:ValList)
        => assignExps(Es, Vs) ~> rulAssignedK(appendPrems(PsMatch, PsPath), Eouts)
       ...
       </k>

  syntax PremList ::= appendPrems(PremList, PremList)
                                            [function, total, symbol(appendPrems)]
  rule appendPrems(.PremList, Ps:PremList) => Ps
  rule appendPrems((P:Prem, Ps:PremList), Qs:PremList) => P, appendPrems(Ps, Qs)

  syntax KItem ::= rulAssignedK(PremList, ExpList)
  rule <k> assignOK() ~> rulAssignedK(Ps:PremList, Eouts:ExpList)
        => evalPrems(Ps) ~> rulBodyK(Eouts)
       ...
       </k>
  rule <k> FAIL ~> rulAssignedK(_:PremList, _:ExpList) => FAIL ... </k>

  syntax KItem ::= rulBodyK(ExpList)
  rule <k> premOK() ~> rulBodyK(Eouts:ExpList) => evalExps(Eouts) ... </k>
  rule <k> FAIL ~> rulBodyK(_:ExpList) => FAIL ... </k>

  //   rulegroup Eval_ruls
  rule <k> evalRuls(_:RulMatch, .RulPathList, _:ValList) => FAIL ... </k>
  rule <k> evalRuls(M:RulMatch, (P:RulPath, Ps:RulPathList), Vs:ValList)
        => pushSave() ~> evalRul(M, P, Vs) ~> tryNextRul(M, Ps, Vs)
       ...
       </k>

  syntax KItem ::= tryNextRul(RulMatch, RulPathList, ValList)
  rule <k> Vs:ValList ~> tryNextRul(_:RulMatch, _:RulPathList, _:ValList)
        => popSaveRestore() ~> Vs
       ...
       </k>
  rule <k> FAIL ~> tryNextRul(M:RulMatch, Ps:RulPathList, Vs:ValList)
        => popSaveRestore() ~> evalRuls(M, Ps, Vs)
       ...
       </k>

  // ==========================================================================
  // Eval_rulgroup / Eval_rulgroups
  //
  //   relation Eval_rulgroup: ctx |- rulgroup '@' `( val* `) : res<val*>
  //   relation Eval_rulgroups: ctx |- rulgroup* '@' `( val* `) : res<val*>
  // ==========================================================================

  syntax KItem ::= evalRulGroup(RulGroup, ValList)
  syntax KItem ::= evalRulGroups(RulGroupList, ValList)

  //   rule Eval_rulgroup/succ, /fail:
  //     -- if id ':' rulmatch '=' rulpath* = rulgroup
  //     -- Eval_ruls: C |- rulmatch -> rulpath* '@' `( val* `) : OK val_out*
  rule <k> evalRulGroup(rulGroup(_:Id, M:RulMatch, Ps:RulPathList), Vs:ValList)
        => evalRuls(M, Ps, Vs)
       ...
       </k>

  //   rulegroup Eval_rulgroups
  rule <k> evalRulGroups(.RulGroupList, _:ValList) => FAIL ... </k>
  rule <k> evalRulGroups((G:RulGroup, Gs:RulGroupList), Vs:ValList)
        => evalRulGroup(G, Vs) ~> tryNextRulGroup(Gs, Vs)
       ...
       </k>

  syntax KItem ::= tryNextRulGroup(RulGroupList, ValList)
  rule <k> Vs:ValList ~> tryNextRulGroup(_:RulGroupList, _:ValList) => Vs ... </k>
  rule <k> FAIL ~> tryNextRulGroup(Gs:RulGroupList, Vs:ValList) => evalRulGroups(Gs, Vs) ... </k>

  // ==========================================================================
  // Call_defined_rel
  //
  //   rule Call_defined_rel:
  //     C |- definedRelDef '@' `( val* `) : valsres
  //     -- if DEF rulgroup* elsgroup? = definedRelDef
  //     -- if (rulgroup_els = $elsgroup_as_rulgroup(elsgroup))?
  //     -- if rulgroup_all* = rulgroup* ++ $opt_as_seq_<rulgroup>(rulgroup_els?)
  //     -- if C_local = C[ .LOCAL = $empty_layer ]
  //     -- Eval_rulgroups: C_local |- rulgroup_all* '@' `( val* `) : valsres
  //
  // Unlike a function call, a relation call's fresh local layer has no type
  // parameters to seed (relations have no `tparam*`), and there is no caller
  // context to snapshot (no `FUN` arguments in a relation's `exp*` match), so
  // this only needs `pushCallFrame(.Map)`/`popCallFrame()`, not
  // `snapshotCaller()`.
  // ==========================================================================

  syntax KItem ::= callDefinedRel(RulGroupList, ElsGroupOpt, ValList)

  rule <k> callDefinedRel(Gs:RulGroupList, EG:ElsGroupOpt, Vs:ValList)
        => pushCallFrame(.Map)
           ~> evalRulGroups(appendRulGroups(Gs, optAsSeqRulGroup(EG)), Vs)
           ~> relCallDone()
       ...
       </k>

  // Like `tblRowDone()` (5.5-eval-call-func.k), but for a relation call's result
  // shape: a `ValList` on success (a relation's outputs are `val*`, not a
  // single `val`) rather than a bare `Val`.
  syntax KItem ::= "relCallDone" "(" ")"
  rule <k> Vs:ValList ~> relCallDone() => popCallFrame() ~> Vs ... </k>
  rule <k> FAIL ~> relCallDone() => popCallFrame() ~> FAIL ... </k>

  //   dec $elsgroup_as_rulgroup(elsgroup) : rulgroup
  syntax RulGroupList ::= optAsSeqRulGroup(ElsGroupOpt)
                                            [function, total, symbol(optAsSeqRulGroup)]
                        | appendRulGroups(RulGroupList, RulGroupList)
                                            [function, total, symbol(appendRulGroups)]
  rule optAsSeqRulGroup(noElsGroup()) => .RulGroupList
  rule optAsSeqRulGroup(someElsGroup(elsGroup(X:Id, M:RulMatch, P:RulPath)))
    => rulGroup(X, M, (P, .RulPathList)), .RulGroupList
  rule appendRulGroups(.RulGroupList, Gs:RulGroupList) => Gs
  rule appendRulGroups((G:RulGroup, Gs:RulGroupList), Hs:RulGroupList)
    => G, appendRulGroups(Gs, Hs)

  // ==========================================================================
  // Call_rel_dispatch
  //
  //   rulegroup Call_rel_dispatch {
  //     Call_rel_dispatch/defined: Call_defined_rel
  //     Call_rel_dispatch/ext:     Call_extern_rel
  //   }
  //
  // `Call_extern_rel` is an `extern relation`, dispatched over the external
  // interface like its sibling `Call_extern_func` (5.5-eval-call-func.k): the
  // call is serialized and answered against the target spec in `<specdir>`, on
  // the P4 interface.
  //
  // A relation yields `val*` rather than a single value, so the reply's `ok`
  // payload is a JSON array and `externRelResult` decodes it to a `ValList` —
  // exactly the shape `relCallDone()` already expects.  A `{"fail": null}`
  // reply becomes `FAIL`, which backtracks through `tryNextRul` like any
  // failed rule, so neither continuation needed changing.
  // ==========================================================================

  syntax KItem ::= callRelDispatch(RelDef, ValList)
  syntax KItem ::= callExternRel(Id, ValList)

  rule <k> callRelDispatch(defRel(Gs:RulGroupList, EG:ElsGroupOpt), Vs:ValList)
        => callDefinedRel(Gs, EG, Vs)
       ...
       </k>
  rule <k> callRelDispatch(extRel(X:Id), Vs:ValList) => callExternRel(X, Vs) ... </k>

  //   An extern relation takes only `id val*` — no `targs`
  //   (spec-meta/common/4-relation.watsup:26-30).
  rule <k> callExternRel(X:Id, Vs:ValList)
        => externReq(JSON2String(jsonOfExternRelRequest(X, Vs)),
                     externRelK())
       ...
       </k>

  // ==========================================================================
  // Call_rel: look the relation up (global only — see 3-context.watsup's
  // `$find_rel`, which never checks a local `renv`), then dispatch.
  //
  //   rule Call_rel: C |- id val* : valsres
  //     -- if reldef = $find_rel(C, id)
  //     -- Call_rel_dispatch: C |- reldef val* : valsres
  //
  // `callRel`'s task shape is declared in 5.4-eval-prem.k (used by the relation
  // premise and if-hold/if-not-hold premises, which precede this module in
  // dependency order); its rules are added here.
  // ==========================================================================

  rule <k> callRel(X:Id, Vs:ValList) => callRelDispatch({GR[X]}:>RelDef, Vs) ... </k>
       <grenv> GR:Map </grenv>
    requires X in_keys(GR)
  //   `$find_rel` has no fallback: a relation id absent from `GLOBAL.REL`
  //   matches no watsup clause. As with an unresolved function call, this is
  //   treated as FAIL to keep the port total.
  rule <k> callRel(X:Id, _:ValList) => FAIL ... </k>
       <grenv> GR:Map </grenv>
    requires notBool (X in_keys(GR))

endmodule
