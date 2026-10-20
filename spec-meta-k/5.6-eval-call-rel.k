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
  // ==========================================================================

  syntax KItem ::= evalRul(RulMatch, RulPath, ValList)

  //   rule Eval_rul/succ: the inputs assigned, the premises of the match
  //   and of the path, the outputs
  rule <k> evalRul(rulMatch(Es:ExpList, PsMatch:PremList),
                   rulPath(_:Id, Eouts:ExpList, PsPath:PremList),
                   Vs:ValList)
        => assignExps(Es, Vs) ~> PsMatch ~> PsPath ~> Eouts
       ...
       </k>

  // ==========================================================================
  // Eval_ruls, $ruls_of_rulgroups: one try per (rulmatch, rulpath) pair of
  // every group
  // ==========================================================================

  syntax Tries ::= rulTries(RulGroupList, ValList)
                                            [function, total, symbol(rulTries)]
  rule rulTries(.RulGroupList, _:ValList) => .Tries
  rule rulTries((rulGroup(_:Id, _:RulMatch, .RulPathList), Gs:RulGroupList), Vs:ValList)
    => rulTries(Gs, Vs)
  rule rulTries((rulGroup(X:Id, M:RulMatch, (P:RulPath, Ps:RulPathList)), Gs:RulGroupList),
               Vs:ValList)
    => try(evalRul(M, P, Vs)) ; rulTries((rulGroup(X, M, Ps), Gs), Vs)

  // ==========================================================================
  // Call_defined_rel/succ, /fail: the rule paths in a frame of their own
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
           ~> det(rulTries(Gs, Vs), elsGroupK(EG, Vs))
           ~> endFrame
       ...
       </k>

  syntax KItem ::= elsGroupK(ElsGroupOpt, ValList)
                                            [function, total, symbol(elsGroupK)]
  rule elsGroupK(noElsGroup(), _:ValList) => backtrack
  rule elsGroupK(someElsGroup(elsGroup(_:Id, M:RulMatch, P:RulPath)), Vs:ValList)
    => evalRul(M, P, Vs)

  // ==========================================================================
  // Call_rel_dispatch/defined, /ext
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
  // Call_rel: `$find_rel` in the global environment
  // ==========================================================================

  rule <k> callRel(X:Id, Vs:ValList) => callRelDispatch(R, Vs) ... </k>
       <grenv> ... X |-> R:RelDef ... </grenv>
  rule <k> callRel(_:Id, _:ValList) => backtrack ... </k> [owise]

endmodule
