// Relation invocation: rule paths, dispatch, and Call_rel.
//
// Mirrors spec-meta/5.7-eval-call-rel.watsup, except that the rule paths of
// all groups of a relation are checked for determinism together (see
// Call_defined_rel below).

requires "5.5-eval-call-func.k"

module CALL-REL
  imports CALL-FUNC

  // ==========================================================================
  // Eval_rul
  //
  //   relation Eval_rul: ctx |- rulmatch -> rulpath '@' `( val* `) : res<val*>
  // ==========================================================================

  syntax KItem ::= evalRul(RulMatch, RulPath, ValList)

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

  syntax KItem ::= rulAwaitPremisesK(ExpList)
  rule <k> premOK() ~> rulAwaitPremisesK(Eouts:ExpList) => evalExps(Eouts) ... </k>

  // ==========================================================================
  // Eval_ruls: every rule path of the relation, flat across its groups
  //
  //   relation Eval_ruls: ctx |- rulmatch -> rulpath* '@' `( val* `) : res<val*>
  //   relation Eval_rulgroups: ctx |- rulgroup* '@' `( val* `) : res<val*>
  //
  // Each attempt pairs a group's rulmatch with one of its paths; each is one
  // alternative under the relation's local layer, and the outcomes are
  // collected for `det`; the KItem runs when no path matched.
  // ==========================================================================

  syntax RulAttempt ::= rulAttempt(RulMatch, RulPath)
  syntax RulAttemptList ::= List{RulAttempt, ","}

  syntax RulAttemptList ::= attemptsOfGroups(RulGroupList)
                                            [function, total, symbol(attemptsOfGroups)]
                          | attemptsOfPaths(RulMatch, RulPathList)
                                            [function, total, symbol(attemptsOfPaths)]
                          | appendAttempts(RulAttemptList, RulAttemptList)
                                            [function, total, symbol(appendAttempts)]
  rule attemptsOfGroups(.RulGroupList) => .RulAttemptList
  rule attemptsOfGroups((rulGroup(_:Id, M:RulMatch, Ps:RulPathList), Gs:RulGroupList))
    => appendAttempts(attemptsOfPaths(M, Ps), attemptsOfGroups(Gs))
  rule attemptsOfPaths(_:RulMatch, .RulPathList) => .RulAttemptList
  rule attemptsOfPaths(M:RulMatch, (P:RulPath, Ps:RulPathList))
    => rulAttempt(M, P), attemptsOfPaths(M, Ps)
  rule appendAttempts(.RulAttemptList, Bs:RulAttemptList) => Bs
  rule appendAttempts((A:RulAttempt, As:RulAttemptList), Bs:RulAttemptList)
    => A, appendAttempts(As, Bs)

  syntax KItem ::= evalRuls(RulAttemptList, ValList, KItem)
                 | rulsAwaitResultK(RulAttemptList, ValList, KItem, List)

  rule <k> evalRuls(As:RulAttemptList, Vs:ValList, Else:KItem)
        => rulsAwaitResultK(As, Vs, Else, .List)
       ...
       </k>

  rule <k> rulsAwaitResultK((rulAttempt(M:RulMatch, P:RulPath), As:RulAttemptList),
                            Vs:ValList, Else:KItem, L:List)
        => alt(evalRul(M, P, Vs)) ~> rulsAwaitResultK(As, Vs, Else, L)
       ...
       </k>
  rule <k> rulsAwaitResultK(.RulAttemptList, _:ValList, Else:KItem, L:List) => det(L, Else) ... </k>

  rule <k> R:Res ~> rulsAwaitResultK(As:RulAttemptList, Vs:ValList, Else:KItem, L:List)
        => rulsAwaitResultK(As, Vs, Else, L ListItem(R))
       ...
       </k>

  // ==========================================================================
  // Call_defined_rel
  //
  //   rule Call_defined_rel/succ, /fail:
  //     C |- definedRelDef '@' `( val* `) : valsres
  //     -- if DEF rulgroup* elsgroup? = definedRelDef
  //     -- if C_local = C[ .LOCAL = $empty_layer ]
  //     -- Eval_rulgroups: C_local |- rulgroup* '@' `( val* `) : valsres
  //     (/fail: valsres = FAIL, then Eval_rulgroup on the else group)
  //
  // Deviation from the spec: the spec checks determinism per group and then
  // across groups, so a group with two matching paths FAILs and a second
  // group's single match is accepted. Here all paths of all groups are
  // checked together, and the else group runs only when none matched, in
  // the relation's frame.
  // ==========================================================================

  syntax KItem ::= callDefinedRel(RulGroupList, ElsGroupOpt, ValList)

  rule <k> callDefinedRel(Gs:RulGroupList, EG:ElsGroupOpt, Vs:ValList)
        => enterFrame(.Map)
           ~> evalRuls(attemptsOfGroups(Gs), Vs, elsGroupK(EG, Vs))
           ~> endFrame
       ...
       </k>

  syntax KItem ::= elsGroupK(ElsGroupOpt, ValList)
                                            [function, total, symbol(elsGroupK)]
  rule elsGroupK(noElsGroup(), _:ValList) => backtrack
  rule elsGroupK(someElsGroup(elsGroup(_:Id, M:RulMatch, P:RulPath)), Vs:ValList)
    => evalRul(M, P, Vs)

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

  rule <k> findRel(X:Id) => {GR[X]}:>RelDef ... </k>
       <grenv> GR:Map </grenv>
    requires X in_keys(GR)
  rule <k> findRel(X:Id) => backtrack ... </k>
       <grenv> GR:Map </grenv>
    requires notBool (X in_keys(GR))

endmodule
