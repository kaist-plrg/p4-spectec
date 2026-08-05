// Loading context from a script.
//
// Mirrors `$load`, `$load_typdef`, `$load_reldef`, `$load_funcdef` from
// spec-meta/al/3-context.watsup.

requires "2-env.k"

module AL-CONTEXT
  imports AL-ENV

  // ==========================================================================
  // $load: consumes definitions off the head of the script into <global>
  // ==========================================================================

  rule <k> (extTypD(X:Id), Ds:Script) => Ds ... </k>
       <gtdenv> M:Map => M[X <- extTD()] </gtdenv>
  rule <k> (typD(X:Id, Ps:TParamList, DT:DefTyp), Ds:Script) => Ds ... </k>
       <gtdenv> M:Map => M[X <- defTD(Ps, DT)] </gtdenv>

  rule <k> (extRelD(X:Id, _:TypList, _:TypList), Ds:Script) => Ds ... </k>
       <grenv> M:Map => M[X <- extRel(X)] </grenv>
  rule <k> (relD(X:Id, _:TypList, _:TypList, Gs:RulGroupList, EG:ElsGroupOpt),
            Ds:Script)
        => Ds
       ...
       </k>
       <grenv> M:Map => M[X <- defRel(Gs, EG)] </grenv>

  rule <k> (extFuncD(X:Id, _:TParamList, _:ParamList, _:Typ), Ds:Script) => Ds ... </k>
       <gfenv> M:Map => M[X <- extFunc(X)] </gfenv>
  rule <k> (builtinFuncD(X:Id, Ps:TParamList, As:ParamList, _:Typ), Ds:Script)
        => Ds
       ...
       </k>
       <gfenv> M:Map => M[X <- builtinFunc(X, Ps, As)] </gfenv>
  rule <k> (tableFuncD(X:Id, As:ParamList, _:Typ, Rs:TblRowList), Ds:Script)
        => Ds
       ...
       </k>
       <gfenv> M:Map => M[X <- tableFunc(As, Rs)] </gfenv>
  rule <k> (funcD(X:Id, Ps:TParamList, _:ParamList, _:Typ, Cs:ClauseList, EC:ElsClauseOpt),
            Ds:Script)
        => Ds
       ...
       </k>
       <gfenv> M:Map => M[X <- defFunc(Ps, Cs, EC)] </gfenv>

  rule <k> .Script => .K ... </k>

endmodule
