// Relation invocation: rule paths, defined relations, and Call_rel.
//
// Mirrors spec-meta/3-execution/3.12-eval-call-rel.watsup. The rule paths
// of all groups of a relation run in order, like the clauses of a function.

requires "3.12-eval-call-func.k"

module CALL-REL
  imports CALL-FUNC

  // ==========================================================================
  // Eval_rul: ctx |- rul `( val* `) : relRes
  // ==========================================================================

  //   rule Eval_rul/succ: the inputs assigned, the premises of the match
  //   and of the path, the outputs
  rule <k> evalRul(rul(rulMatch(Es:ExpList, PsMatch:PremList),
                       rulPath(_:Id, Eouts:ExpList, PsPath:PremList)),
                   Vs:ValList)
        => assignExps(Es, Vs) ~> PsMatch ~> PsPath ~> Eouts
       ...
       </k>

  // ==========================================================================
  // Eval_ruls: ctx |- rul* `( val* `) : relRes
  // Eval_ruls_on_ok: ctx |- rul* `( val* `) : unitRes
  //
  // As Eval_clauses / Eval_clauses_on_ok (3.12-eval-call-func.k).
  // ==========================================================================

  //   rule Eval_ruls/nil
  rule <k> evalRuls(.RulList, _:ValList) => UNMATCH ... </k>
  rule <k> evalRuls((R:Rul, Rs:RulList), Vs:ValList)
        => try(evalRul(R, Vs)) ~> afterRul(Rs, Vs)
       ...
       </k>

  syntax KItem ::= afterRul(RulList, ValList)

  //   rule Eval_ruls/unmatch
  rule <k> UNMATCH ~> afterRul(Rs:RulList, Vs:ValList) => evalRuls(Rs, Vs) ... </k>
  //   rule Eval_ruls/succ: the remaining rule paths must all mismatch
  rule <k> ok(Ws:KItem) ~> afterRul(Rs:RulList, Vs:ValList)
        => evalRulsOnOk(Rs, Vs) ~> onOk(ok(Ws))
       ...
       </k>
  //   rule Eval_ruls/error
  rule <k> ERROR ~> afterRul(_:RulList, _:ValList) => ERROR ... </k>

  //   rule Eval_ruls_on_ok/nil
  rule <k> evalRulsOnOk(.RulList, _:ValList) => .K ... </k>
  rule <k> evalRulsOnOk((R:Rul, Rs:RulList), Vs:ValList)
        => try(evalRul(R, Vs)) ~> afterRulOnOk(Rs, Vs)
       ...
       </k>

  syntax KItem ::= afterRulOnOk(RulList, ValList)

  //   rule Eval_ruls_on_ok/unmatch
  rule <k> UNMATCH ~> afterRulOnOk(Rs:RulList, Vs:ValList) => evalRulsOnOk(Rs, Vs) ... </k>
  //   rule Eval_ruls_on_ok/error: a second success or an error
  rule <k> ok(_) ~> afterRulOnOk(_:RulList, _:ValList) => ERROR ... </k>
  rule <k> ERROR ~> afterRulOnOk(_:RulList, _:ValList) => ERROR ... </k>

  // ==========================================================================
  // Call_defined_rel: ctx |- definedRelDefn `( val* `) : relRes
  //
  //   dec $ruls_of_rulgroup(rulGroup) : rul*
  //   dec $ruls_of_rulgroups(rulGroup*) : rul*
  //   dec $rul_of_elsgroup(elsGroup) : rul
  //
  // The rule paths run in a frame of their own; the otherwise group runs
  // there too, only when every path mismatched.
  // ==========================================================================

  syntax RulList ::= rulsOfRulGroups(RulGroupList)
                                            [function, total, symbol(rulsOfRulGroups)]
                   | rulsOfRulGroup(RulMatch, RulPathList)
                                            [function, total, symbol(rulsOfRulGroup)]
                   | appendRuls(RulList, RulList)
                                            [function, total, symbol(appendRuls)]
  rule rulsOfRulGroups(.RulGroupList) => .RulList
  rule rulsOfRulGroups(rulGroup(_:Id, M:RulMatch, Ps:RulPathList), Gs:RulGroupList)
    => appendRuls(rulsOfRulGroup(M, Ps), rulsOfRulGroups(Gs))
  rule rulsOfRulGroup(_:RulMatch, .RulPathList) => .RulList
  rule rulsOfRulGroup(M:RulMatch, (P:RulPath, Ps:RulPathList)) => rul(M, P), rulsOfRulGroup(M, Ps)
  rule appendRuls(.RulList, Rs:RulList) => Rs
  rule appendRuls((R:Rul, Rs:RulList), Rs':RulList) => R, appendRuls(Rs, Rs')

  syntax Rul ::= rulOfElsGroup(ElsGroup)    [function, total, symbol(rulOfElsGroup)]
  rule rulOfElsGroup(elsGroup(_:Id, M:RulMatch, P:RulPath)) => rul(M, P)

  rule <k> callDefinedRel(relD(_, _, _, Gs:RulGroupList, EG:ElsGroupOpt), Vs:ValList)
        => enterFrame(.Map)
           ~> evalRuls(rulsOfRulGroups(Gs), Vs)
           ~> afterRuls(EG, Vs)
           ~> endFrame
       ...
       </k>

  syntax KItem ::= afterRuls(ElsGroupOpt, ValList)

  //   rule Call_defined_rel/result
  rule <k> ok(Ws:KItem) ~> afterRuls(_:ElsGroupOpt, _:ValList) => Ws ... </k>
  rule <k> ERROR ~> afterRuls(_:ElsGroupOpt, _:ValList) => error ... </k>
  //   rule Call_defined_rel/unmatch
  rule <k> UNMATCH ~> afterRuls(noElsGroup(), _:ValList) => unmatch ... </k>
  rule <k> UNMATCH ~> afterRuls(someElsGroup(EG:ElsGroup), Vs:ValList)
        => evalRul(rulOfElsGroup(EG), Vs)
       ...
       </k>

  // ==========================================================================
  // Call_rel: ctx |- id `( val* `) : relRes
  // ==========================================================================

  rule <k> callRel(X:Id, Vs:ValList) => callRelDefn(D, Vs) ... </k>
       <grenv> ... X |-> D:RelDefn ... </grenv>
  //   rule Call_rel/error: no such relation
  rule <k> callRel(_:Id, _:ValList) => error ... </k> [owise]

  syntax KItem ::= callRelDefn(RelDefn, ValList)

  //   rule Call_rel/extern
  rule <k> callRelDefn(extRelD(X:Id, _, _), Vs:ValList) => callExternRel(X, Vs) ... </k>
  //   rule Call_rel/defined
  rule <k> callRelDefn(relD(_, _, _, _, _) #as D:DefinedRelDefn, Vs:ValList)
        => callDefinedRel(D, Vs)
       ...
       </k>

endmodule
