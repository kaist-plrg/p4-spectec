// Type casts.
//
// Mirrors spec-meta/3-execution/3.05.4-typ-cast.watsup. The context `C` is
// the three maps a cast reads: `<gtdenv>`, `<ltdenv>`, and `<gfenv>`.
//
//   syntax castRes = ok<val> | error
//   syntax castsRes = ok<val*> | error

requires "3.06.3-typ-match.k"

module TYP-CAST
  imports TYP-MATCH

  // ==========================================================================
  // Upcasts
  //
  //   dec $upcast(ctx, typ, val) : castRes
  //   dec $upcasts(ctx, typ*, val*) : castsRes
  //
  // A value that does not fit the type is an error.
  // ==========================================================================

  syntax Res ::= upcast(Map, Map, Map, Typ, Val)
                                            [function, total, symbol(upcast)]
               | upcasts(Map, Map, Map, TypList, ValList)
                                            [function, total, symbol(upcasts)]

  rule upcast(_:Map, _:Map, _:Map, intT(), intN(I:Int)) => ok(intN(I))
  rule upcast(_:Map, _:Map, _:Map, intT(), natN(N:Int)) => ok(intN(N))
  rule upcast(G:Map, L:Map, FE:Map, varT(X:Id, Ts:TArgList), V:Val)
    => upcastVar(G, L, FE, findTyp(G, L, X), varT(X, Ts), V)
  rule upcast(G:Map, L:Map, FE:Map, tupT(Ts:TypList), tupV(Vs:ValList))
    => resTupV(upcasts(G, L, FE, Ts, Vs))
  rule upcast(_:Map, _:Map, _:Map, iterT(_:Typ, quest()), optV(noVal())) => ok(optV(noVal()))
  rule upcast(G:Map, L:Map, FE:Map, iterT(T:Typ, quest()), optV(someVal(V:Val)))
    => resOptV(upcast(G, L, FE, T, V))
  rule upcast(G:Map, L:Map, FE:Map, iterT(T:Typ, star()), listV(Vs:ValList))
    => resListV(upcasts(G, L, FE, repeatTyp(T, lengthVals(Vs)), Vs))
  rule upcast(_:Map, _:Map, _:Map, boolT(), boolV(B:Bool)) => ok(boolV(B))
  rule upcast(_:Map, _:Map, _:Map, textT(), textV(S:String)) => ok(textV(S))
  rule upcast(_:Map, _:Map, _:Map, natT(), natN(N:Int)) => ok(natN(N))
  rule upcast(G:Map, L:Map, FE:Map, funcT(Ps:TParamList, Ts:TypList, T:Typ), funcV(X:Id)) => ok(funcV(X))
    requires subtyp(FE, updateMap(G, L), funcT(Ps, Ts, T), funcV(X)) ==K ok(true)
  rule upcast(_:Map, _:Map, _:Map, _:Typ, _:Val) => ERROR [owise]

  // `VAR id targ*` by its definition (`$find_typ`): through an alias, as is
  // when a variant or struct type holds the value, as is for an extern value
  // of an extern type.
  syntax Res ::= upcastVar(Map, Map, Map, TypDefOpt, Typ, Val)
                                            [function, total, symbol(upcastVar)]
  rule upcastVar(G:Map, L:Map, FE:Map, someTypDef(typD(_, Ps:TParamList, aliasDT(T:Typ))), varT(_, Ts:TArgList), V:Val)
    => upcastRes(G, L, FE, substTyp(thetaOf(Ps, Ts), T), V)
    requires sameArity(Ps, Ts)
  rule upcastVar(G:Map, L:Map, FE:Map, someTypDef(typD(_, _, variantDT(_))), T:Typ, V:Val) => ok(V)
    requires subtyp(FE, updateMap(G, L), T, V) ==K ok(true)
  rule upcastVar(G:Map, L:Map, FE:Map, someTypDef(typD(_, _, structDT(_))), T:Typ, V:Val) => ok(V)
    requires subtyp(FE, updateMap(G, L), T, V) ==K ok(true)
  rule upcastVar(_:Map, _:Map, _:Map, someTypDef(extTypD(_)), varT(_, .TArgList), extV(J:JSON)) => ok(extV(J))
  rule upcastVar(_:Map, _:Map, _:Map, _:TypDefOpt, _:Typ, _:Val) => ERROR [owise]

  syntax Res ::= upcastRes(Map, Map, Map, Res, Val)
                                            [function, symbol(upcastRes)]
  rule upcastRes(G:Map, L:Map, FE:Map, ok(T:Typ), V:Val) => upcast(G, L, FE, T, V)
  rule upcastRes(_:Map, _:Map, _:Map, F:Failure, _:Val) => F

  rule upcasts(_:Map, _:Map, _:Map, .TypList, .ValList) => ok(.ValList)
  rule upcasts(G:Map, L:Map, FE:Map, (T:Typ, Ts:TypList), (V:Val, Vs:ValList))
    => consRes(upcast(G, L, FE, T, V), upcasts(G, L, FE, Ts, Vs))
  rule upcasts(_:Map, _:Map, _:Map, _:TypList, _:ValList) => ERROR [owise]

  // ==========================================================================
  // Downcasts
  //
  //   dec $downcast(ctx, typ, val) : castRes
  //   dec $downcasts(ctx, typ*, val*) : castsRes
  // ==========================================================================

  syntax Res ::= downcast(Map, Map, Map, Typ, Val)
                                            [function, total, symbol(downcast)]
               | downcasts(Map, Map, Map, TypList, ValList)
                                            [function, total, symbol(downcasts)]

  rule downcast(_:Map, _:Map, _:Map, natT(), natN(N:Int)) => ok(natN(N))
  rule downcast(_:Map, _:Map, _:Map, natT(), intN(I:Int)) => ok(natN(I))
    requires I >=Int 0
  rule downcast(G:Map, L:Map, FE:Map, varT(X:Id, Ts:TArgList), V:Val)
    => downcastVar(G, L, FE, findTyp(G, L, X), varT(X, Ts), V)
  rule downcast(G:Map, L:Map, FE:Map, tupT(Ts:TypList), tupV(Vs:ValList))
    => resTupV(downcasts(G, L, FE, Ts, Vs))
  rule downcast(_:Map, _:Map, _:Map, iterT(_:Typ, quest()), optV(noVal())) => ok(optV(noVal()))
  rule downcast(G:Map, L:Map, FE:Map, iterT(T:Typ, quest()), optV(someVal(V:Val)))
    => resOptV(downcast(G, L, FE, T, V))
  rule downcast(G:Map, L:Map, FE:Map, iterT(T:Typ, star()), listV(Vs:ValList))
    => resListV(downcasts(G, L, FE, repeatTyp(T, lengthVals(Vs)), Vs))
  rule downcast(_:Map, _:Map, _:Map, boolT(), boolV(B:Bool)) => ok(boolV(B))
  rule downcast(_:Map, _:Map, _:Map, textT(), textV(S:String)) => ok(textV(S))
  rule downcast(_:Map, _:Map, _:Map, intT(), N:Num) => ok(N)
  rule downcast(G:Map, L:Map, FE:Map, funcT(Ps:TParamList, Ts:TypList, T:Typ), funcV(X:Id)) => ok(funcV(X))
    requires subtyp(FE, updateMap(G, L), funcT(Ps, Ts, T), funcV(X)) ==K ok(true)
  rule downcast(_:Map, _:Map, _:Map, _:Typ, _:Val) => ERROR [owise]

  syntax Res ::= downcastVar(Map, Map, Map, TypDefOpt, Typ, Val)
                                            [function, total, symbol(downcastVar)]
  rule downcastVar(G:Map, L:Map, FE:Map, someTypDef(typD(_, Ps:TParamList, aliasDT(T:Typ))), varT(_, Ts:TArgList), V:Val)
    => downcastRes(G, L, FE, substTyp(thetaOf(Ps, Ts), T), V)
    requires sameArity(Ps, Ts)
  rule downcastVar(G:Map, L:Map, FE:Map, someTypDef(typD(_, _, variantDT(_))), T:Typ, V:Val) => ok(V)
    requires subtyp(FE, updateMap(G, L), T, V) ==K ok(true)
  rule downcastVar(G:Map, L:Map, FE:Map, someTypDef(typD(_, _, structDT(_))), T:Typ, V:Val) => ok(V)
    requires subtyp(FE, updateMap(G, L), T, V) ==K ok(true)
  rule downcastVar(_:Map, _:Map, _:Map, someTypDef(extTypD(_)), varT(_, .TArgList), extV(J:JSON)) => ok(extV(J))
  rule downcastVar(_:Map, _:Map, _:Map, _:TypDefOpt, _:Typ, _:Val) => ERROR [owise]

  syntax Res ::= downcastRes(Map, Map, Map, Res, Val)
                                            [function, symbol(downcastRes)]
  rule downcastRes(G:Map, L:Map, FE:Map, ok(T:Typ), V:Val) => downcast(G, L, FE, T, V)
  rule downcastRes(_:Map, _:Map, _:Map, F:Failure, _:Val) => F

  rule downcasts(_:Map, _:Map, _:Map, .TypList, .ValList) => ok(.ValList)
  rule downcasts(G:Map, L:Map, FE:Map, (T:Typ, Ts:TypList), (V:Val, Vs:ValList))
    => consRes(downcast(G, L, FE, T, V), downcasts(G, L, FE, Ts, Vs))
  rule downcasts(_:Map, _:Map, _:Map, _:TypList, _:ValList) => ERROR [owise]

  // ==========================================================================
  // Cast results
  // ==========================================================================

  // The cast values as a tuple, list, or option, or the failure.
  syntax Res ::= resTupV(Res)               [function, symbol(resTupV)]
               | resListV(Res)              [function, symbol(resListV)]
               | resOptV(Res)               [function, symbol(resOptV)]
  rule resTupV(ok(Vs:ValList)) => ok(tupV(Vs))
  rule resTupV(F:Failure) => F
  rule resListV(ok(Vs:ValList)) => ok(listV(Vs))
  rule resListV(F:Failure) => F
  rule resOptV(ok(V:Val)) => ok(optV(someVal(V)))
  rule resOptV(F:Failure) => F

endmodule
