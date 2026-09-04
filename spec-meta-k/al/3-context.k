// Loading context from a script.
//
// Mirrors `$load`, `$load_typdef`, `$load_reldef`, `$load_funcdef` from
// spec-meta/al/3-context.watsup.

requires "2-env.k"
requires "../common/3-relation.k"

module AL-CONTEXT
  imports AL-ENV
  imports COMMON-RELATION

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
  // Local environment saves
  // ==========================================================================

  syntax KItem ::= "pushLocalSave" "(" ")"
                 | "popLocalSave" "(" ")"
                 | popLocalSaveThen(KItem)
                 | "popLocalSaveList" "(" ")"
                 | "peekLocalSave" "(" ")"
                 | "clearLocalVenv" "(" ")"

  rule <k> pushLocalSave() => .K ... </k>
       <ltdenv> T:Map </ltdenv>
       <lfenv> F:Map </lfenv>
       <lvenv> V:Map </lvenv>
       <saves> .List => ListItem(localSave(T, F, V)) ... </saves>

  rule <k> popLocalSave() => .K ... </k>
       <ltdenv> _ => T </ltdenv>
       <lfenv> _ => F </lfenv>
       <lvenv> _ => V </lvenv>
       <saves> ListItem(localSave(T:Map, F:Map, V:Map)) => .List ... </saves>

  rule <k> V:Val ~> popLocalSaveThen(Next:KItem) => popLocalSave() ~> V ~> Next ... </k>
  rule <k> FAIL ~> popLocalSaveThen(_:KItem) => popLocalSave() ~> FAIL ... </k>

  rule <k> Vs:ValList ~> popLocalSaveList() => popLocalSave() ~> listV(Vs) ... </k>
  rule <k> FAIL ~> popLocalSaveList() => popLocalSave() ~> FAIL ... </k>

  rule <k> peekLocalSave() => .K ... </k>
       <ltdenv> _ => T </ltdenv>
       <lfenv> _ => F </lfenv>
       <lvenv> _ => V </lvenv>
       <saves> ListItem(localSave(T:Map, F:Map, V:Map)) ... </saves>

  rule <k> clearLocalVenv() => .K ... </k>
       <lvenv> _ => .Map </lvenv>

  // ==========================================================================
  // Call-frame stack
  // ==========================================================================

  syntax KItem ::= pushCallFrame(Map)       // new local tdenv for the callee
                 | "popCallFrame" "(" ")"
                 | "snapshotCaller" "(" ")"

  rule <k> pushCallFrame(T:Map) => .K ... </k>
       <ltdenv> T':Map => T </ltdenv>
       <lfenv> F:Map => .Map </lfenv>
       <lvenv> V:Map => .Map </lvenv>
       <cfenv> CF:Map </cfenv>
       <callstack> .List => ListItem(callSave(localSave(T', F, V), CF)) ... </callstack>

  rule <k> popCallFrame() => .K ... </k>
       <ltdenv> _ => T </ltdenv>
       <lfenv> _ => F </lfenv>
       <lvenv> _ => V </lvenv>
       <cfenv> _ => CF </cfenv>
       <callstack> ListItem(callSave(localSave(T:Map, F:Map, V:Map), CF:Map)) => .List ... </callstack>

  rule <k> snapshotCaller() => .K ... </k>
       <lfenv> F:Map </lfenv>
       <cfenv> _ => F </cfenv>

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

  // `$sub_opt`: `noRowOpt()` if `$sub_opt` is `eps` (skip the body);
  // `someRowOpt(vals)` if it applies, with `vals` the value to bind each
  // `vari` in `Vs` to, in order.
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

endmodule
