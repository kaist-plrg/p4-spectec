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
  //   $det_valres / $det_valsres take the outcomes of every alternative.
  //
  // `alt(K)` runs `K` as one alternative: its bindings are undone afterwards
  // and its outcome is recorded.
  // ==========================================================================

  syntax Alt ::= alt(K) | Res

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
  // `det(L, Else)` is the single success; `Else` (the else clause or group,
  // or `backtrack`) when none succeeded; and `backtrack` when several did.
  // ==========================================================================

  syntax KItem ::= det(List, KItem)

  syntax List ::= successes(List)           [function, symbol(successes)]
  rule successes(.List) => .List
  rule successes(ListItem(FAIL) L:List) => successes(L)
  rule successes(ListItem(ok(R:KItem)) L:List) => ListItem(R) successes(L)

  rule <k> det(L:List, _:KItem) => {successes(L)[0]}:>KItem ... </k>
    requires size(successes(L)) ==Int 1
  rule <k> det(L:List, Else:KItem) => Else ... </k>
    requires size(successes(L)) ==Int 0
  rule <k> det(L:List, _:KItem) => backtrack ... </k>
    requires size(successes(L)) >Int 1

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

  // ==========================================================================
  // $find_typ
  // ==========================================================================

  syntax TypDefOpt ::= noTypDefOpt()        [symbol(noTypDefOpt)]
                     | someTypDefOpt(TypDef)
                                            [symbol(someTypDefOpt)]

  syntax TypDefOpt ::= findTypIn(Map, Map, Id)
                                            [function, total, symbol(findTypIn)]
                                            // (ltdenv, gtdenv, id)

  rule findTypIn(L:Map, _:Map, X:Id) => someTypDefOpt({L[X]}:>TypDef)
    requires X in_keys(L)
  rule findTypIn(L:Map, G:Map, X:Id) => someTypDefOpt({G[X]}:>TypDef)
    requires notBool (X in_keys(L)) andBool (X in_keys(G))
  rule findTypIn(L:Map, G:Map, X:Id) => noTypDefOpt()
    requires notBool (X in_keys(L)) andBool notBool (X in_keys(G))

  // ==========================================================================
  // Sub-context construction for iteration
  //
  //   dec $sub_opt(ctx, vari*) : ctx?
  //   dec $sub_list(ctx, vari*) : ctx*
  // ==========================================================================

  syntax SubOptRow ::= noRowOpt()           [symbol(noRowOpt)]
                     | someRowOpt(ValList)  [symbol(someRowOpt)]

  syntax KItem ::= subOptRow(VariList)
  rule <k> subOptRow(Vs:VariList) => classifySubOpt(findOpts(iterVaris(Vs, quest()), M)) ... </k>
       <lvenv> M:Map </lvenv>

  syntax ValListOpt ::= noValListOpt()      [symbol(noValListOpt)]
                      | someValListOpt(ValList)
                                            [symbol(someValListOpt)]

  syntax ValListOpt ::= findOpts(VarrList, Map)
                                            [function, total, symbol(findOpts)]
  rule findOpts(.VarrList, _:Map) => someValListOpt(.ValList)
  rule findOpts((R:Varr, Rs:VarrList), M:Map)
    => consOptRow({M[R]}:>Val, findOpts(Rs, M))
    requires R in_keys(M)
  rule findOpts((R:Varr, _:VarrList), M:Map) => noValListOpt()
    requires notBool (R in_keys(M))

  syntax ValListOpt ::= consOptRow(Val, ValListOpt)
                                            [function, total, symbol(consOptRow)]
  rule consOptRow(V:Val, someValListOpt(Vs:ValList)) => someValListOpt(V, Vs)
  rule consOptRow(_:Val, noValListOpt()) => noValListOpt()

  syntax SubOptRow ::= classifySubOpt(ValListOpt)
                                            [function, symbol(classifySubOpt)]
  // every lookup was `OPT val`
  rule classifySubOpt(someValListOpt(Vs:ValList)) => someRowOpt(unwrapSomeOpts(Vs))
    requires allSomeOpts(Vs)
  // every lookup was `OPT eps`
  rule classifySubOpt(someValListOpt(Vs:ValList)) => noRowOpt()
    requires allNoneOpts(Vs)

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

  // `$sub_list`: the rows to bind, one per element, or `.ValListList` if no
  // rows apply (`$sub_list(C, vari*) = eps`, e.g. an empty `LIST`).
  syntax KItem ::= subListRows(VariList)
  rule <k> subListRows(Vs:VariList) => transposeVal(unwrapLists(findLists(iterVaris(Vs, star()), M))) ... </k>
       <lvenv> M:Map </lvenv>

  syntax ValList ::= findLists(VarrList, Map)
                                            [function, symbol(findLists)]
  rule findLists(.VarrList, _:Map) => .ValList
  rule findLists((R:Varr, Rs:VarrList), M:Map) => {M[R]}:>Val, findLists(Rs, M)
    requires R in_keys(M)

  syntax ValListList ::= unwrapLists(ValList)
                                            [function, symbol(unwrapLists)]
  rule unwrapLists(.ValList) => .ValListList
  rule unwrapLists(listV(Vs:ValList), Ws:ValList) => Vs ; unwrapLists(Ws)

  // `vari*` at one more `iter`.
  syntax VarrList ::= iterVaris(VariList, Iter)
                                            [function, total, symbol(iterVaris)]
  rule iterVaris(.VariList, _:Iter) => .VarrList
  rule iterVaris((V:Vari, Vs:VariList), I:Iter)
    => varrOfVari(iterVari(V, I)), iterVaris(Vs, I)

  // Bind one row of a sub-context.
  syntax KItem ::= bindRow(VariList, ValList)
  rule <k> bindRow(.VariList, .ValList) => .K ... </k>
  rule <k> bindRow((V:Vari, Vs:VariList), (W:Val, Ws:ValList)) => bindRow(Vs, Ws) ... </k>
       <lvenv> M:Map => M[varrOfVari(V) <- W] </lvenv>

endmodule
