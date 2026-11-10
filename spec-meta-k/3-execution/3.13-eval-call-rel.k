// Relation invocation: rule paths, defined relations, and Call_rel.
//
// Mirrors spec-meta/3-execution/3.11-eval-call-rel.watsup.

requires "3.12-eval-call-func.k"

module CALL-REL
  imports CALL-FUNC

  // ==========================================================================
  // Eval_rul: ctx |- rul `( val* `) : relRes
  // ==========================================================================

  //   rule Eval_rul/succ
  rule <k> evalRul(rul(rulMatch(Es:ExpList, PsMatch:PremList),
                       rulPath(_:Id, Eouts:ExpList, PsPath:PremList)),
                   Vs:ValList)
        => assignExps(Es, Vs) ~> PsMatch ~> PsPath ~> Eouts
       ...
       </k>

  // ==========================================================================
  // Eval_ruls: ctx |- rul* `( val* `) : relRes
  //
  // `ERROR` passes the catch frames (Eval_ruls/error).
  // ==========================================================================

  //   rule Eval_ruls/nil
  rule <k> evalRuls(.RulList, _:ValList) => UNMATCH ... </k>
  rule <k> evalRuls((R:Rul, Rs:RulList), Vs:ValList)
        => evalRul(R, Vs) ~> afterRul(Rs, Vs, F, V)
       ...
       </k>
       <lfenv> F:Map </lfenv>
       <lvenv> V:Map </lvenv>

  syntax UnmatchBoundary ::= afterRul(RulList, ValList, Map, Map)
                                            // lfenv, lvenv

  //   rule Eval_ruls/unmatch
  rule <k> UNMATCH ~> afterRul(Rs:RulList, Vs:ValList, F:Map, V:Map) => evalRuls(Rs, Vs) ... </k>
       <lfenv> _ => F </lfenv>
       <lvenv> _ => V </lvenv>
  //   rule Eval_ruls/succ
  rule <k> Ws:ValList ~> afterRul(_:RulList, _:ValList, F:Map, V:Map) => Ws ... </k>
       <lfenv> _ => F </lfenv>
       <lvenv> _ => V </lvenv>

  // Determinism check (Eval_ruls_on_ok): replaces the rule above.
  //
  //   rule Eval_ruls/succ: the remaining paths must all mismatch
  // rule <k> Ws:ValList ~> afterRul(Rs:RulList, Vs:ValList, F:Map, V:Map)
  //       => evalRulsOnOk(Rs, Vs) ~> Ws
  //      ...
  //      </k>
  //      <lfenv> _ => F </lfenv>
  //      <lvenv> _ => V </lvenv>
  //
  // syntax KItem ::= evalRulsOnOk(RulList, ValList)
  //
  //   rule Eval_ruls_on_ok/nil
  // rule <k> evalRulsOnOk(.RulList, _:ValList) => .K ... </k>
  // rule <k> evalRulsOnOk((R:Rul, Rs:RulList), Vs:ValList)
  //       => evalRul(R, Vs) ~> afterRulOnOk(Rs, Vs, F, V)
  //      ...
  //      </k>
  //      <lfenv> F:Map </lfenv>
  //      <lvenv> V:Map </lvenv>
  //
  // syntax UnmatchBoundary ::= afterRulOnOk(RulList, ValList, Map, Map)
  //                                           // lfenv, lvenv
  //
  //   rule Eval_ruls_on_ok/unmatch
  // rule <k> UNMATCH ~> afterRulOnOk(Rs:RulList, Vs:ValList, F:Map, V:Map) => evalRulsOnOk(Rs, Vs) ... </k>
  //      <lfenv> _ => F </lfenv>
  //      <lvenv> _ => V </lvenv>
  //   rule Eval_ruls_on_ok/error: a second success
  // rule <k> _:ValList ~> afterRulOnOk(_:RulList, _:ValList, _:Map, _:Map) => ERROR ... </k>

  // ==========================================================================
  // Call_defined_rel: ctx |- definedRelDefn `( val* `) : relRes
  //
  //   dec $ruls_of_rulgroup(rulGroup) : rul*
  //   dec $ruls_of_rulgroups(rulGroup*) : rul*
  //   dec $rul_of_elsgroup(elsGroup) : rul
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

  //   rule Call_defined_rel/result, /unmatch
  rule <k> callDefinedRel(relD(_, _, _, Gs:RulGroupList, noElsGroup()), Vs:ValList)
        => enterFrame(.Map)
           ~> evalRuls(rulsOfRulGroups(Gs), Vs)
           ~> endFrame
       ...
       </k>
  //   rule Call_defined_rel/result, /unmatch
  rule <k> callDefinedRel(relD(_, _, _, Gs:RulGroupList, someElsGroup(EG:ElsGroup)), Vs:ValList)
        => enterFrame(.Map)
           ~> evalRuls(rulsOfRulGroups(Gs), Vs)
           ~> afterRuls(rulOfElsGroup(EG), Vs)
           ~> endFrame
       ...
       </k>

  // The otherwise group
  syntax UnmatchBoundary ::= afterRuls(Rul, ValList)

  //   rule Call_defined_rel/result
  rule <k> Ws:ValList ~> afterRuls(_:Rul, _:ValList) => Ws ... </k>
  //   rule Call_defined_rel/unmatch
  rule <k> UNMATCH ~> afterRuls(R:Rul, Vs:ValList) => evalRul(R, Vs) ... </k>

  // ==========================================================================
  // Call_rel: ctx |- id `( val* `) : relRes
  // ==========================================================================

  rule <k> callRel(X:Id, Vs:ValList) => callRelDefn(D, Vs) ... </k>
       <grenv> ... X |-> D:RelDefn ... </grenv>
  //   rule Call_rel/error: no such relation
  rule <k> callRel(_:Id, _:ValList) => ERROR ... </k> [owise]

  syntax KItem ::= callRelDefn(RelDefn, ValList)

  //   rule Call_rel/extern
  rule <k> callRelDefn(extRelD(X:Id, _, _), Vs:ValList) => callExternRel(X, Vs) ... </k>
  //   rule Call_rel/defined
  rule <k> callRelDefn(relD(_, _, _, _, _) #as D:DefinedRelDefn, Vs:ValList)
        => callDefinedRel(D, Vs)
       ...
       </k>

endmodule
