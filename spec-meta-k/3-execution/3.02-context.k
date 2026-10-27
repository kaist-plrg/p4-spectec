// Context: loading, function parameters, scopes, call frames, and
// sub-contexts for iteration.
//
// Mirrors spec-meta/4-execution/4.01-context.watsup. The context `ctx` is
// the `<global>` and `<local>` cells of the configuration.

requires "3.01-env.k"

module CONTEXT
  imports ENV

  // ==========================================================================
  // $load: definitions in order, each into <global> as written
  // ==========================================================================

  rule <k> (D:Defn, Ds:Spec) => D ~> Ds ... </k>
  rule <k> .Spec => .K ... </k>

  rule <k> (extTypD(X:Id) #as D:ExternTypDefn) => .K ... </k>
       <gtdenv> M:Map => M[X <- D] </gtdenv>
  rule <k> (typD(X:Id, _, _) #as D:DefinedTypDefn) => .K ... </k>
       <gtdenv> M:Map => M[X <- D] </gtdenv>

  rule <k> (extRelD(X:Id, _, _) #as D:ExternRelDefn) => .K ... </k>
       <grenv> M:Map => M[X <- D] </grenv>
  rule <k> (relD(X:Id, _, _, _, _) #as D:DefinedRelDefn) => .K ... </k>
       <grenv> M:Map => M[X <- D] </grenv>

  rule <k> (extFuncD(X:Id, _, _, _) #as D:ExternFuncDefn) => .K ... </k>
       <gfenv> M:Map => M[X <- D] </gfenv>
  rule <k> (builtinFuncD(X:Id, _, _, _) #as D:BuiltinFuncDefn) => .K ... </k>
       <gfenv> M:Map => M[X <- D] </gfenv>
  rule <k> (tableFuncD(X:Id, _, _, _) #as D:TableFuncDefn) => .K ... </k>
       <gfenv> M:Map => M[X <- D] </gfenv>
  rule <k> (funcD(X:Id, _, _, _, _, _) #as D:DefinedFuncDefn) => .K ... </k>
       <gfenv> M:Map => M[X <- D] </gfenv>

  // ==========================================================================
  // Type definitions
  //
  //   dec $find_typ(ctx, id) : typDef?
  //
  // The local type definitions, then the global ones.
  // ==========================================================================

  syntax TypDefOpt ::= findTyp(Id, Map, Map)
                                            [function, total, symbol(findTyp)]   // id, gtdenv, ltdenv
  rule findTyp(X:Id, _:Map, (X |-> D:TypDef) _:Map) => someTypDef(D)
  rule findTyp(X:Id, (X |-> D:TypDef) _:Map, L:Map) => someTypDef(D)
    requires notBool (X in_keys(L))
  rule findTyp(_:Id, _:Map, _:Map) => noTypDef() [owise]

  // ==========================================================================
  // Function parameters
  //
  //   dec $resolve_func_param(ctx, id) : id
  //
  // `<lfenv>` binds a function parameter to a global function name.
  // ==========================================================================

  syntax Id ::= resolveFuncParam(Id, Map)   [function, total, symbol(resolveFuncParam)]
  rule resolveFuncParam(X:Id, (X |-> Y:Id) _:Map) => Y
  rule resolveFuncParam(X:Id, _:Map) => X [owise]

  // ==========================================================================
  // Scopes: a sub-context whose value bindings end with it
  //
  //   $sub_opt / $sub_list bodies, Assign_exp/list
  // ==========================================================================

  syntax Exp ::= inScope(K)                // an `Exp`, so that it fits a strict hole

  rule <k> inScope(K:K) => K ~> endScope(M) ... </k>
       <lvenv> M:Map </lvenv>
  rule <k> R:KResult ~> endScope(M:Map) => R ... </k>
       <lvenv> _ => M </lvenv>
  rule <k> S:Signal ~> endScope(M:Map) => S ... </k>
       <lvenv> _ => M </lvenv>

  syntax KItem ::= "clearLocalVenv" "(" ")"
  rule <k> clearLocalVenv() => .K ... </k>
       <lvenv> _ => .Map </lvenv>

  // ==========================================================================
  // Call frames
  //
  //   C_callee = C[ .LOCAL = $empty_localLayer[ .TYP = tdenv ] ]
  //
  // `enterFrame(T)` replaces the local layer; `endFrame` restores it.
  // ==========================================================================

  syntax Frame ::= frame(Map, Map, Map)     [symbol(frame)]   // ltdenv, lfenv, lvenv

  syntax KItem ::= enterFrame(Map)          // the callee's local tdenv

  rule <k> enterFrame(T:Map) => .K ... </k>
       <ltdenv> T':Map => T </ltdenv>
       <lfenv> F:Map => .Map </lfenv>
       <lvenv> V:Map => .Map </lvenv>
       <callstack> .List => ListItem(frame(T', F, V)) ... </callstack>

  rule <k> R:KResult ~> endFrame => R ... </k>
       <ltdenv> _ => T </ltdenv>
       <lfenv> _ => F </lfenv>
       <lvenv> _ => V </lvenv>
       <callstack> ListItem(frame(T:Map, F:Map, V:Map)) => .List ... </callstack>
  rule <k> S:Signal ~> endFrame => S ... </k>
       <ltdenv> _ => T </ltdenv>
       <lfenv> _ => F </lfenv>
       <lvenv> _ => V </lvenv>
       <callstack> ListItem(frame(T:Map, F:Map, V:Map)) => .List ... </callstack>

  // ==========================================================================
  // Sub-contexts for iteration
  //
  //   dec $sub_opt(ctx, vari*) : subOptRes       syntax subOptRes = ok<ctx?> | error
  //   dec $sub_list(ctx, vari*) : subListRes     syntax subListRes = ok<ctx*> | error
  //
  // A sub-context is the current context with `vari*` bound: here the values
  // to bind to them (`SubOpt`), or one row of values per sub-context
  // (`ValListList`). The consumer runs each under `inScope(bindRow(..) ~> ..)`.
  // ==========================================================================

  syntax SubOpt ::= noSub()                 [symbol(noSub)]
                  | someSub(ValList)        [symbol(someSub)]

  syntax Res ::= subOpt(VariList, Map)      [function, total, symbol(subOpt)]
  rule subOpt(.VariList, _:Map) => ERROR
  rule subOpt(Xs:VariList, M:Map) => subOptOf(finds(iterVaris(Xs, quest()), M)) [owise]

  syntax Res ::= subOptOf(Res)              [function, total, symbol(subOptOf)]
  rule subOptOf(ok(Vs:ValList)) => ok(someSub(unwrapSomeOpts(Vs)))
    requires allSomeOpts(Vs)
  rule subOptOf(ok(Vs:ValList)) => ok(noSub())
    requires allNoneOpts(Vs)
  rule subOptOf(_:Res) => ERROR [owise]

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

  // The rows of `$sub_list`: the bound lists transposed, one row per element,
  // so an empty list gives no rows. Lists of different lengths are an error.
  syntax Res ::= subList(VariList, Map)     [function, total, symbol(subList)]
  rule subList(Xs:VariList, M:Map) => subListOf(finds(iterVaris(Xs, star()), M))

  syntax Res ::= subListOf(Res)             [function, total, symbol(subListOf)]
  rule subListOf(ok(Vs:ValList)) => ok(transposeVal(lists(Vs)))
    requires sameLengthLists(Vs)
  rule subListOf(_:Res) => ERROR [owise]

  syntax ValListList ::= lists(ValList)     [function, symbol(lists)]
  rule lists(.ValList) => .ValListList
  rule lists(listV(Vs:ValList), Ws:ValList) => Vs ; lists(Ws)

  // All `LIST`s of one length.
  syntax Bool ::= sameLengthLists(ValList)  [function, total, symbol(sameLengthLists)]
                | sameLengthLists(Int, ValList)
                                            [function, total, symbol(sameLengthListsAs)]
  rule sameLengthLists(.ValList) => true
  rule sameLengthLists(listV(Vs:ValList), Ws:ValList) => sameLengthLists(lengthVals(Vs), Ws)
  rule sameLengthLists(_:Val, _:ValList) => false [owise]
  rule sameLengthLists(_:Int, .ValList) => true
  rule sameLengthLists(N:Int, (listV(Vs:ValList), Ws:ValList))
    => lengthVals(Vs) ==Int N andBool sameLengthLists(N, Ws)
  rule sameLengthLists(_:Int, (_:Val, _:ValList)) => false [owise]

  // Bind one row of a sub-context.
  syntax KItem ::= bindRow(VariList, ValList)
  rule <k> bindRow(.VariList, .ValList) => .K ... </k>
  rule <k> bindRow((V:Vari, Vs:VariList), (W:Val, Ws:ValList)) => bindRow(Vs, Ws) ... </k>
       <lvenv> M:Map => M[varrOfVari(V) <- W] </lvenv>

  // The results of the rows, in order.
  syntax KItem ::= consRow(KItem, KItem)    [seqstrict]
  rule <k> consRow(Vs:ValList, Vss:ValListList) => Vs ; Vss ... </k>

endmodule
