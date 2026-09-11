// Relation invocation: rule paths, rule groups, dispatch, and Call_rel.
//
// Mirrors spec-meta/al/5.7-eval-call-rel.watsup.

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
                 | evalRuls(RulMatch, RulPathList, ValList)

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
        => assignExps(Es, Vs) ~> rulAwaitAssignmentK(appendPrems(PsMatch, PsPath), Eouts)
       ...
       </k>

  syntax PremList ::= appendPrems(PremList, PremList)
                                            [function, total, symbol(appendPrems)]
  rule appendPrems(.PremList, Ps:PremList) => Ps
  rule appendPrems((P:Prem, Ps:PremList), Qs:PremList) => P, appendPrems(Ps, Qs)

  syntax KItem ::= rulAwaitAssignmentK(PremList, ExpList)
  rule <k> assignOK() ~> rulAwaitAssignmentK(Ps:PremList, Eouts:ExpList)
        => evalPrems(Ps) ~> rulAwaitPremisesK(Eouts)
       ...
       </k>
  rule <k> FAIL ~> rulAwaitAssignmentK(_:PremList, _:ExpList) => FAIL ... </k>

  syntax KItem ::= rulAwaitPremisesK(ExpList)
  rule <k> premOK() ~> rulAwaitPremisesK(Eouts:ExpList) => evalExps(Eouts) ... </k>
  rule <k> FAIL ~> rulAwaitPremisesK(_:ExpList) => FAIL ... </k>

  //   rulegroup Eval_ruls
  rule <k> evalRuls(_:RulMatch, .RulPathList, _:ValList) => FAIL ... </k>
  rule <k> evalRuls(M:RulMatch, (P:RulPath, Ps:RulPathList), Vs:ValList)
        => pushLocalSave() ~> evalRul(M, P, Vs) ~> rulPathsAwaitResultK(M, Ps, Vs)
       ...
       </k>

  syntax KItem ::= rulPathsAwaitResultK(RulMatch, RulPathList, ValList)
  rule <k> Vs:ValList ~> rulPathsAwaitResultK(_:RulMatch, _:RulPathList, _:ValList)
        => popLocalSave() ~> Vs
       ...
       </k>
  rule <k> FAIL ~> rulPathsAwaitResultK(M:RulMatch, Ps:RulPathList, Vs:ValList)
        => popLocalSave() ~> evalRuls(M, Ps, Vs)
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
        => evalRulGroup(G, Vs) ~> rulGroupsAwaitResultK(Gs, Vs)
       ...
       </k>

  syntax KItem ::= rulGroupsAwaitResultK(RulGroupList, ValList)
  rule <k> Vs:ValList ~> rulGroupsAwaitResultK(_:RulGroupList, _:ValList) => Vs ... </k>
  rule <k> FAIL ~> rulGroupsAwaitResultK(Gs:RulGroupList, Vs:ValList) => evalRulGroups(Gs, Vs) ... </k>

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
  // ==========================================================================

  syntax KItem ::= callDefinedRel(RulGroupList, ElsGroupOpt, ValList)

  rule <k> callDefinedRel(Gs:RulGroupList, EG:ElsGroupOpt, Vs:ValList)
        => pushCallFrame(.Map)
           ~> evalRulGroups(appendRulGroups(Gs, optAsSeqRulGroup(EG)), Vs)
           ~> relCallAwaitResultK()
       ...
       </k>

  syntax KItem ::= "relCallAwaitResultK" "(" ")"
  rule <k> Vs:ValList ~> relCallAwaitResultK() => popCallFrame() ~> Vs ... </k>
  rule <k> FAIL ~> relCallAwaitResultK() => popCallFrame() ~> FAIL ... </k>

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
  // ==========================================================================

  syntax KItem ::= callRelDispatch(RelDef, ValList)
  syntax KItem ::= callExternRel(Id, ValList)

  rule <k> callRelDispatch(defRel(Gs:RulGroupList, EG:ElsGroupOpt), Vs:ValList)
        => callDefinedRel(Gs, EG, Vs)
       ...
       </k>
  rule <k> callRelDispatch(extRel(X:Id), Vs:ValList) => callExternRel(X, Vs) ... </k>

  rule <k> callExternRel(X:Id, Vs:ValList)
        => externReq(JSON2String(jsonOfExternRelRequest(X, Vs)),
                     externRelK())
       ...
       </k>

  // ==========================================================================
  // Call_rel: look the relation up, then dispatch.
  //
  //   rule Call_rel: C |- id val* : valsres
  //     -- if reldef = $find_rel(C, id)
  //     -- Call_rel_dispatch: C |- reldef val* : valsres
  // ==========================================================================

  syntax KItem ::= findRel(Id)
                 | callRelAwaitRelK(ValList)

  rule <k> callRel(X:Id, Vs:ValList)
        => findRel(X) ~> callRelAwaitRelK(Vs)
       ...
       </k>

  rule <k> R:RelDef ~> callRelAwaitRelK(Vs:ValList)
        => callRelDispatch(R, Vs)
       ...
       </k>
  rule <k> FAIL ~> callRelAwaitRelK(_:ValList) => FAIL ... </k>

  rule <k> findRel(X:Id) => {GR[X]}:>RelDef ... </k>
       <grenv> GR:Map </grenv>
    requires X in_keys(GR)
  rule <k> findRel(X:Id) => FAIL ... </k>
       <grenv> GR:Map </grenv>
    requires notBool (X in_keys(GR))

endmodule
