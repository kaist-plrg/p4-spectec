// Context and control: loading, outcomes, backtracking, scopes, and frames.
//
// Mirrors spec-meta/3-context.watsup and the result sorts of
// spec-meta/4-relation.watsup.

requires "2-env.k"

module CONTEXT
  imports ENV

  // ==========================================================================
  // Outcomes
  //
  //   syntax res<X> = OK X | FAIL
  //
  // The outcome of one alternative (a clause, a rule path, a table row, a
  // cast). `FAIL` is only ever a recorded outcome, never in-flight failure:
  // a rule with no applicable case rewrites to `backtrack` instead.
  // ==========================================================================

  syntax Res ::= ok(KItem)                  [symbol(ok)]
               | "FAIL"                     [symbol(FAIL)]

  syntax KResult ::= Val | ValList | ValField | ValFieldList | ValCase
                   | ValListList | Res

  // ==========================================================================
  // Backtracking
  //
  // `backtrack` discards the pending work of the current alternative up to
  // the boundary that started it. A checkpoint turns it into the outcome
  // `FAIL`; a scope or frame boundary restores its state and lets it go on.
  // ==========================================================================

  syntax KItem ::= "backtrack"

  syntax Boundary ::= checkpoint(Map, Map)  // lfenv, lvenv
                    | endScope(Map)         // lvenv
                    | "endFrame"

  rule <k> backtrack ~> (K:KItem => .K) ... </k> requires notBool isBoundary(K)

  // ==========================================================================
  // Alternatives
  //
  // `alt(K)` runs `K` as one alternative: its bindings are undone afterwards
  // and its outcome is recorded. A list of alternatives evaluates in order
  // to a list of outcomes.
  // ==========================================================================

  syntax Alt ::= alt(K) | Res

  syntax Alts ::= List{Alt, ";"}            [symbol(alts), terminator-symbol(.alts), seqstrict, overload(alts)]
  syntax Ress ::= List{Res, ";"}            [symbol(ress), terminator-symbol(.ress), overload(alts)]
  syntax Alts ::= Ress
  syntax KResult ::= Ress

  rule <k> alt(K:K) => K ~> checkpoint(F, V) ... </k>
       <lfenv> F:Map </lfenv>
       <lvenv> V:Map </lvenv>
  rule <k> R:KResult ~> checkpoint(F:Map, V:Map) => ok(R) ... </k>
       <lfenv> _ => F </lfenv>
       <lvenv> _ => V </lvenv>
  rule <k> backtrack ~> checkpoint(F:Map, V:Map) => FAIL ... </k>
       <lfenv> _ => F </lfenv>
       <lvenv> _ => V </lvenv>

  // `unres`: the outcome of a cast, back in flight.
  syntax KItem ::= unres(Res)
  rule <k> unres(ok(V:Val)) => V ... </k>
  rule <k> unres(FAIL) => backtrack ... </k>

  // ==========================================================================
  // Determinism check over the outcomes of the alternatives of a call
  //
  //   def $det_valres(valres*) : valres
  //   def $det_valsres(valsres*) : valsres
  //
  // `det(Alts, Else)` is the single success; `Else` (the else clause or
  // group, or `backtrack`) when none succeeded; `backtrack` when several did.
  // ==========================================================================

  syntax KItem ::= det(Alts, KItem)         [strict(1)]
  rule <k> det(Rs:Ress, Else:KItem) => pick(oks(Rs), Else) ... </k>

  syntax KItem ::= pick(List, KItem)
  rule <k> pick(ListItem(R:KItem), _:KItem) => R ... </k>
  rule <k> pick(.List, Else:KItem) => Else ... </k>
  rule <k> pick(ListItem(_) ListItem(_) _:List, _:KItem) => backtrack ... </k>

  // The `OK` outcomes, in order.
  syntax List ::= oks(Ress)                 [function, total, symbol(oks)]
  rule oks(.Ress) => .List
  rule oks(ok(R:KItem) ; Rs:Ress) => ListItem(R) oks(Rs)
  rule oks(FAIL ; Rs:Ress) => oks(Rs)

  // The first success of alternatives tried in order.
  syntax KItem ::= firstOf(Alt, KItem)      [strict(1)]
  rule <k> firstOf(ok(R:KItem), _:KItem) => R ... </k>
  rule <k> firstOf(FAIL, Next:KItem) => Next ... </k>

  // ==========================================================================
  // Scopes: a sub-context whose value bindings end with it
  //
  //   $sub_opt / $sub_list bodies, Assign_exp/list
  // ==========================================================================

  syntax KItem ::= inScope(K)

  rule <k> inScope(K:K) => K ~> endScope(M) ... </k>
       <lvenv> M:Map </lvenv>
  rule <k> R:KResult ~> endScope(M:Map) => R ... </k>
       <lvenv> _ => M </lvenv>
  rule <k> backtrack ~> endScope(M:Map) => backtrack ... </k>
       <lvenv> _ => M </lvenv>

  syntax KItem ::= "clearLocalVenv" "(" ")"
  rule <k> clearLocalVenv() => .K ... </k>
       <lvenv> _ => .Map </lvenv>

  // ==========================================================================
  // Call frames
  //
  //   C_callee = C[ .LOCAL = $empty_layer[ .TYP = tdenv ] ]
  //
  // `enterFrame(T)` replaces the local layer and records the caller's local
  // function environment for `Assign_arg/fun`; `endFrame` restores all of it.
  // ==========================================================================

  syntax Frame ::= frame(Map, Map, Map, Map) [symbol(frame)]   // ltdenv, lfenv, lvenv, cfenv

  syntax KItem ::= enterFrame(Map)          // the callee's local tdenv

  rule <k> enterFrame(T:Map) => .K ... </k>
       <ltdenv> T':Map => T </ltdenv>
       <lfenv> F:Map => .Map </lfenv>
       <lvenv> V:Map => .Map </lvenv>
       <cfenv> CF:Map => F </cfenv>
       <callstack> .List => ListItem(frame(T', F, V, CF)) ... </callstack>

  rule <k> R:KResult ~> endFrame => R ... </k>
       <ltdenv> _ => T </ltdenv>
       <lfenv> _ => F </lfenv>
       <lvenv> _ => V </lvenv>
       <cfenv> _ => CF </cfenv>
       <callstack> ListItem(frame(T:Map, F:Map, V:Map, CF:Map)) => .List ... </callstack>
  rule <k> backtrack ~> endFrame => backtrack ... </k>
       <ltdenv> _ => T </ltdenv>
       <lfenv> _ => F </lfenv>
       <lvenv> _ => V </lvenv>
       <cfenv> _ => CF </cfenv>
       <callstack> ListItem(frame(T:Map, F:Map, V:Map, CF:Map)) => .List ... </callstack>

  // ==========================================================================
  // $load: definitions in order, each into <global>
  // ==========================================================================

  rule <k> (D:Defn, Ds:Script) => D ~> Ds ... </k>
  rule <k> .Script => .K ... </k>

  rule <k> extTypD(X:Id) => .K ... </k>
       <gtdenv> M:Map => M[X <- extTD()] </gtdenv>
  rule <k> typD(X:Id, Ps:TParamList, DT:DefTyp) => .K ... </k>
       <gtdenv> M:Map => M[X <- defTD(Ps, DT)] </gtdenv>

  rule <k> extRelD(X:Id, _:TypList, _:TypList) => .K ... </k>
       <grenv> M:Map => M[X <- extRel(X)] </grenv>
  rule <k> relD(X:Id, _:TypList, _:TypList, Gs:RulGroupList, EG:ElsGroupOpt) => .K ... </k>
       <grenv> M:Map => M[X <- defRel(Gs, EG)] </grenv>

  rule <k> extFuncD(X:Id, _:TParamList, _:ParamList, _:Typ) => .K ... </k>
       <gfenv> M:Map => M[X <- extFunc(X)] </gfenv>
  rule <k> builtinFuncD(X:Id, Ps:TParamList, As:ParamList, _:Typ) => .K ... </k>
       <gfenv> M:Map => M[X <- builtinFunc(X, Ps, As)] </gfenv>
  rule <k> tableFuncD(X:Id, As:ParamList, _:Typ, Rs:TblRowList) => .K ... </k>
       <gfenv> M:Map => M[X <- tableFunc(As, Rs)] </gfenv>
  rule <k> funcD(X:Id, Ps:TParamList, _:ParamList, _:Typ, Cs:ClauseList, EC:ElsClauseOpt)
        => .K
       ...
       </k>
       <gfenv> M:Map => M[X <- defFunc(Ps, Cs, EC)] </gfenv>

  // ==========================================================================
  // Sub-context construction for iteration
  //
  //   dec $sub_opt(ctx, vari*) : ctx?
  //   dec $sub_list(ctx, vari*) : ctx*
  //
  // `subRow(vari*, iter)` finds the bound variables at one more iteration;
  // the consumer splits on `allSomeOpts`/`allNoneOpts` or transposes the
  // lists into `rows`.
  // ==========================================================================

  syntax KItem ::= subRow(VariList, Iter)
  rule <k> subRow(Xs:VariList, I:Iter) => finds(iterVaris(Xs, I), M) ... </k>
       <lvenv> M:Map </lvenv>

  syntax Bool ::= allSomeOpts(ValList)      [function, total, symbol(allSomeOpts)]
                | allNoneOpts(ValList)      [function, total, symbol(allNoneOpts)]
  rule allSomeOpts(.ValList) => true
  rule allSomeOpts(optV(someVal(_:Val)), Vs:ValList) => allSomeOpts(Vs)
  rule allSomeOpts(_:Val, _:ValList) => false [owise]
  rule allNoneOpts(.ValList) => true
  rule allNoneOpts(optV(noVal()), Vs:ValList) => allNoneOpts(Vs)
  rule allNoneOpts(_:Val, _:ValList) => false [owise]

  syntax ValList ::= unwrapSomeOpts(ValList)
                                            [function, symbol(unwrapSomeOpts)]
  rule unwrapSomeOpts(.ValList) => .ValList
  rule unwrapSomeOpts(optV(someVal(V:Val)), Vs:ValList) => V, unwrapSomeOpts(Vs)

  // The rows of `$sub_list`: the bound lists transposed, one row per
  // element, so an empty list gives no rows.
  syntax ValListList ::= rows(ValList)      [function, symbol(rows)]
                       | lists(ValList)     [function, symbol(lists)]
  rule rows(Vs:ValList) => transposeVal(lists(Vs))
  rule lists(.ValList) => .ValListList
  rule lists(listV(Vs:ValList), Ws:ValList) => Vs ; lists(Ws)

  // Bind one row of a sub-context.
  syntax KItem ::= bindRow(VariList, ValList)
  rule <k> bindRow(.VariList, .ValList) => .K ... </k>
  rule <k> bindRow((V:Vari, Vs:VariList), (W:Val, Ws:ValList)) => bindRow(Vs, Ws) ... </k>
       <lvenv> M:Map => M[varrOfVari(V) <- W] </lvenv>

endmodule
