// Type substitution, type casts, and value-type matching.
//
// Mirrors spec-meta/5.1-eval-typ.watsup.
//
//   dec $subst_typ(theta, typ) : typ
//   dec $upcast(ctx, typ, val) : res<val>
//   dec $downcast(ctx, typ, val) : res<val>
//   dec $subtyp(tdenv, typ, val) : bool
//   dec $subtyps(tdenv, typ*, val*) : bool

requires "3-context.k"
requires "5.0-eval-ops.k"

module TYP
  imports CONTEXT
  imports EVAL-OPS
  imports MAP
  imports BOOL
  imports K-EQUAL

  // Type substitution

  syntax Typ ::= substTyp(Map, Typ)             [function, total, symbol(substTyp)]
               | substTypInner(Map, Typ)        [function, total, symbol(substTypInner)]

  syntax TypList ::= substTypsMap(Map, TypList) [function, total, symbol(substTypsMap)]
  syntax TArgList ::= substTargsMap(Map, TArgList)
                                                [function, total, symbol(substTargsMap)]
                   | targsOfTyps(TypList)       [function, total, symbol(targsOfTyps)]
  syntax TypList ::= typsOfTargs(TArgList)      [function, total, symbol(typsOfTargs)]

  rule substTyp(.Map, T:Typ) => T
  rule substTyp(Th:Map, T:Typ) => substTypInner(Th, T) [owise]

  rule substTypInner(_:Map, natT()) => natT()
  rule substTypInner(_:Map, intT()) => intT()
  rule substTypInner(_:Map, textT()) => textT()
  rule substTypInner(_:Map, boolT()) => boolT()
  rule substTypInner(_:Map, funcT()) => funcT()

  rule substTypInner(Th:Map, varT(X:Id, .TArgList)) => {Th[X]}:>Typ
    requires X in_keys(Th)
  rule substTypInner(Th:Map, varT(X:Id, .TArgList)) => varT(X, .TArgList)
    requires notBool (X in_keys(Th))
  rule substTypInner(Th:Map, varT(X:Id, Ts:TArgList)) => varT(X, substTargsMap(Th, Ts))
    requires Ts =/=K .TArgList

  rule substTypInner(Th:Map, tupT(Ts:TypList)) => tupT(substTypsMap(Th, Ts))
  rule substTypInner(Th:Map, iterT(T:Typ, I:Iter)) => iterT(substTypInner(Th, T), I)

  rule substTypsMap(_:Map, .TypList) => .TypList
  rule substTypsMap(Th:Map, (T:Typ, Ts:TypList))
    => substTypInner(Th, T), substTypsMap(Th, Ts)

  rule substTargsMap(_:Map, .TArgList) => .TArgList
  rule substTargsMap(Th:Map, (T:TArg, Ts:TArgList))
    => substTypInner(Th, {T}:>Typ), substTargsMap(Th, Ts)

  rule typsOfTargs(.TArgList) => .TypList
  rule typsOfTargs((T:TArg, Ts:TArgList)) => {T}:>Typ, typsOfTargs(Ts)

  rule targsOfTyps(.TypList) => .TArgList
  rule targsOfTyps((T:Typ, Ts:TypList)) => T, targsOfTyps(Ts)

  // ==========================================================================
  // Mixop equality and association
  // ==========================================================================

  syntax Bool ::= eqMixOp(MixOp, MixOp)     [function, total, symbol(eqMixOp)]
                | eqAtoms(AtomList, AtomList)
                                            [function, total, symbol(eqAtoms)]

  rule eqMixOp(.MixOp, .MixOp) => true
  rule eqMixOp((A:AtomList ; As:MixOp), (B:AtomList ; Bs:MixOp))
    => eqAtoms(A, B) andBool eqMixOp(As, Bs)
  rule eqMixOp(_:MixOp, _:MixOp) => false [owise]

  rule eqAtoms(.AtomList, .AtomList) => true
  rule eqAtoms((A:Atom, As:AtomList), (B:Atom, Bs:AtomList))
    => A ==String B andBool eqAtoms(As, Bs)
  rule eqAtoms(_:AtomList, _:AtomList) => false [owise]

  syntax TypListOpt ::= noTypList()         [symbol(noTypList)]
                      | someTypList(TypList)
                                            [symbol(someTypList)]

  syntax TypListOpt ::= assocTypCase(MixOp, TypCaseList)
                                            [function, total, symbol(assocTypCase)]

  rule assocTypCase(_:MixOp, .TypCaseList) => noTypList()
  rule assocTypCase(M:MixOp, (typCase(M':MixOp, Ts:TypList), Cs:TypCaseList))
    => #if eqMixOp(M, M') #then someTypList(Ts) #else assocTypCase(M, Cs) #fi

  // ==========================================================================
  // Upcasts
  //
  //   def $upcast(C, INT, INT i) = OK (INT i)
  //   def $upcast(C, INT, NAT n) = OK (INT n)
  //   def $upcast(C, INT, val) = FAIL          -- if ~(val <: num)
  //   def $upcast(C, VAR id targ*, val) = $upcast(C, typ', val)
  //   def $upcast(C, TUP typ*, TUP val*) = OK (TUP val_upcast*)
  //   def $upcast(C, TUP typ*, val) = FAIL     -- if ~$is_tup(val)
  //   def $upcast(C, ITER typ QUEST, OPT val?) = ...
  //   def $upcast(C, ITER typ STAR, LIST val*) = ...
  //   def $upcast(C, typ, val) = OK val        -- otherwise
  // ==========================================================================

  syntax Res ::= upcast(Map, Map, Typ, Val) [function, symbol(upcast)]
                                            // (ltdenv, gtdenv, typ, val)

  rule upcast(_:Map, _:Map, intT(), intN(I:Int)) => ok(intN(I))
  rule upcast(_:Map, _:Map, intT(), natN(N:Int)) => ok(intN(N))
  rule upcast(_:Map, _:Map, intT(), V:Val) => FAIL
    requires notBool isNumV(V)

  rule upcast(L:Map, G:Map, varT(X:Id, Ts:TArgList), V:Val)
    => upcastVar(findTypIn(L, G, X), L, G, Ts, V)

  rule upcast(L:Map, G:Map, tupT(Ts:TypList), tupV(Vs:ValList))
    => resTup(upcasts(L, G, Ts, Vs))
  rule upcast(_:Map, _:Map, tupT(_:TypList), V:Val) => FAIL
    requires notBool isTup(V)

  rule upcast(_:Map, _:Map, iterT(_:Typ, quest()), optV(noVal())) => ok(optV(noVal()))
  rule upcast(L:Map, G:Map, iterT(T:Typ, quest()), optV(someVal(V:Val)))
    => resOpt(upcast(L, G, T, V))
  rule upcast(L:Map, G:Map, iterT(T:Typ, star()), listV(Vs:ValList))
    => resList(upcasts(L, G, Typs2Repeat(T, Vs), Vs))

  rule upcast(_:Map, _:Map, _:Typ, V:Val) => ok(V) [owise]

  syntax Res ::= upcastVar(TypDefOpt, Map, Map, TArgList, Val)
                                            [function, symbol(upcastVar)]

  rule upcastVar(someTypDefOpt(defTD(Ps:TParamList, aliasDT(T:Typ))),
                 L:Map, G:Map, Ts:TArgList, V:Val)
    => upcast(L, G, substTyp(zipThetaMap(Ps, Ts), T), V)
  rule upcastVar(_:TypDefOpt, _:Map, _:Map, _:TArgList, V:Val) => ok(V) [owise]

  // ==========================================================================
  // Downcasts
  //
  //   def $downcast(C, NAT, NAT n) = OK (NAT n)
  //   def $downcast(C, NAT, INT i) = OK (NAT n)  -- if n = i
  //   def $downcast(C, NAT, val) = FAIL          -- if ~(val <: num)
  //   ... (as for upcasts)
  // ==========================================================================

  syntax Res ::= downcast(Map, Map, Typ, Val)
                                            [function, symbol(downcast)]

  rule downcast(_:Map, _:Map, natT(), natN(N:Int)) => ok(natN(N))
  rule downcast(_:Map, _:Map, natT(), intN(I:Int)) => ok(natN(I))
    requires I >=Int 0
  rule downcast(_:Map, _:Map, natT(), V:Val) => FAIL
    requires notBool isNumV(V)

  rule downcast(L:Map, G:Map, varT(X:Id, Ts:TArgList), V:Val)
    => downcastVar(findTypIn(L, G, X), L, G, Ts, V)

  rule downcast(L:Map, G:Map, tupT(Ts:TypList), tupV(Vs:ValList))
    => resTup(downcasts(L, G, Ts, Vs))
  rule downcast(_:Map, _:Map, tupT(_:TypList), V:Val) => FAIL
    requires notBool isTup(V)

  rule downcast(_:Map, _:Map, iterT(_:Typ, quest()), optV(noVal())) => ok(optV(noVal()))
  rule downcast(L:Map, G:Map, iterT(T:Typ, quest()), optV(someVal(V:Val)))
    => resOpt(downcast(L, G, T, V))
  rule downcast(L:Map, G:Map, iterT(T:Typ, star()), listV(Vs:ValList))
    => resList(downcasts(L, G, Typs2Repeat(T, Vs), Vs))

  rule downcast(_:Map, _:Map, _:Typ, V:Val) => ok(V) [owise]

  syntax Res ::= downcastVar(TypDefOpt, Map, Map, TArgList, Val)
                                            [function, symbol(downcastVar)]

  rule downcastVar(someTypDefOpt(defTD(Ps:TParamList, aliasDT(T:Typ))),
                   L:Map, G:Map, Ts:TArgList, V:Val)
    => downcast(L, G, substTyp(zipThetaMap(Ps, Ts), T), V)
  rule downcastVar(_:TypDefOpt, _:Map, _:Map, _:TArgList, V:Val) => ok(V) [owise]

  // ==========================================================================
  // Cast helpers
  // ==========================================================================

  syntax Res ::= upcasts(Map, Map, TypList, ValList)
                                            [function, symbol(upcasts)]
               | downcasts(Map, Map, TypList, ValList)
                                            [function, symbol(downcasts)]

  rule upcasts(_:Map, _:Map, .TypList, .ValList) => ok(.ValList)
  rule upcasts(L:Map, G:Map, (T:Typ, Ts:TypList), (V:Val, Vs:ValList))
    => consRes(upcast(L, G, T, V), upcasts(L, G, Ts, Vs))

  rule downcasts(_:Map, _:Map, .TypList, .ValList) => ok(.ValList)
  rule downcasts(L:Map, G:Map, (T:Typ, Ts:TypList), (V:Val, Vs:ValList))
    => consRes(downcast(L, G, T, V), downcasts(L, G, Ts, Vs))

  syntax Res ::= consRes(Res, Res)          [function, total, symbol(consRes)]

  rule consRes(ok(V:Val), ok(Vs:ValList)) => ok((V, Vs))
  rule consRes(_:Res, _:Res) => FAIL [owise]

  syntax Res ::= resTup(Res)                [function, total, symbol(resTup)]
               | resList(Res)               [function, total, symbol(resList)]
               | resOpt(Res)                [function, total, symbol(resOpt)]

  rule resTup(ok(Vs:ValList)) => ok(tupV(Vs))
  rule resTup(_:Res) => FAIL [owise]
  rule resList(ok(Vs:ValList)) => ok(listV(Vs))
  rule resList(_:Res) => FAIL [owise]
  rule resOpt(ok(V:Val)) => ok(optV(someVal(V)))
  rule resOpt(_:Res) => FAIL [owise]

  syntax TypList ::= Typs2Repeat(Typ, ValList)
                                            [function, total, symbol(Typs2Repeat)]

  rule Typs2Repeat(_:Typ, .ValList) => .TypList
  rule Typs2Repeat(T:Typ, (_:Val, Vs:ValList)) => T, Typs2Repeat(T, Vs)

  syntax Map ::= zipThetaMap(TParamList, TArgList)
                                            [function, symbol(zipThetaMap)]

  rule zipThetaMap(.TParamList, .TArgList) => .Map
  rule zipThetaMap((X:TParam, Xs:TParamList), (T:TArg, Ts:TArgList))
    => zipThetaMap(Xs, Ts) [{X}:>Id <- {T}:>Typ]

  // ==========================================================================
  // Value-type matching
  //
  //   dec $subtyp(tdenv, typ, val) : bool
  //   dec $subtyps(tdenv, typ*, val*) : bool
  // ==========================================================================

  syntax Bool ::= subtyp(Map, Typ, Val)     [function, total, symbol(subtyp)]
                | subtyps(Map, TypList, ValList)
                                            [function, total, symbol(subtyps)]

  rule subtyp(_:Map, boolT(), boolV(_:Bool)) => true
  rule subtyp(_:Map, natT(), natN(_:Int)) => true
  rule subtyp(_:Map, natT(), intN(I:Int)) => I >=Int 0
  rule subtyp(_:Map, intT(), _:Num) => true
  rule subtyp(_:Map, textT(), textV(_:String)) => true

  rule subtyp(E:Map, varT(X:Id, Ts:TArgList), V:Val)
    => subtypVar(findTypInSingle(E, X), E, Ts, V)

  rule subtyp(E:Map, tupT(Ts:TypList), tupV(Vs:ValList)) => subtyps(E, Ts, Vs)

  rule subtyp(_:Map, iterT(_:Typ, quest()), optV(noVal())) => true
  rule subtyp(E:Map, iterT(T:Typ, quest()), optV(someVal(V:Val)))
    => subtyp(E, T, V)
  rule subtyp(E:Map, iterT(T:Typ, star()), listV(Vs:ValList))
    => subtyps(E, Typs2Repeat(T, Vs), Vs)

  rule subtyp(_:Map, _:Typ, _:Val) => false [owise]

  syntax TypDefOpt ::= findTypInSingle(Map, Id)
                                            [function, total, symbol(findTypInSingle)]
  rule findTypInSingle(E:Map, X:Id) => someTypDefOpt({E[X]}:>TypDef) requires X in_keys(E)
  rule findTypInSingle(E:Map, X:Id) => noTypDefOpt() requires notBool (X in_keys(E))

  syntax Bool ::= subtypVar(TypDefOpt, Map, TArgList, Val)
                                            [function, total, symbol(subtypVar)]
                | subtypCase(TypListOpt, Map, Map, ValList)
                                            [function, total, symbol(subtypCase)]
                | subtypFields(Map, Map, TypFieldList, ValFieldList)
                                            [function, total, symbol(subtypFields)]

  rule subtypVar(someTypDefOpt(extTD()), _:Map, _:TArgList, extV(_:JSON)) => true

  rule subtypVar(someTypDefOpt(defTD(Ps:TParamList, aliasDT(T:Typ))),
                 E:Map, Ts:TArgList, V:Val)
    => subtyp(E, substTyp(zipThetaMap(Ps, Ts), T), V)

  rule subtypVar(someTypDefOpt(defTD(Ps:TParamList, variantDT(Cs:TypCaseList))),
                 E:Map, Ts:TArgList, injV(valCase(M:MixOp, Vs:ValList)))
    => subtypCase(assocTypCase(M, Cs), E, zipThetaMap(Ps, Ts), Vs)

  rule subtypVar(someTypDefOpt(defTD(Ps:TParamList, structDT(Fs:TypFieldList))),
                 E:Map, Ts:TArgList, strV(Gs:ValFieldList))
    => subtypFields(E, zipThetaMap(Ps, Ts), Fs, Gs)

  rule subtypVar(_:TypDefOpt, _:Map, _:TArgList, _:Val) => false [owise]

  rule subtypCase(someTypList(Ts:TypList), E:Map, Th:Map, Vs:ValList)
    => subtyps(E, substTypsMap(Th, Ts), Vs)
  rule subtypCase(noTypList(), _:Map, _:Map, _:ValList) => false

  rule subtypFields(_:Map, _:Map, .TypFieldList, .ValFieldList) => true
  rule subtypFields(E:Map, Th:Map,
                    (typField(A:Atom, T:Typ), Fs:TypFieldList),
                    (valField(A':Atom, V:Val), Gs:ValFieldList))
    => A ==String A'
       andBool subtyp(E, substTyp(Th, T), V)
       andBool subtypFields(E, Th, Fs, Gs)
  rule subtypFields(_:Map, _:Map, _:TypFieldList, _:ValFieldList) => false [owise]

  rule subtyps(_:Map, .TypList, .ValList) => true
  rule subtyps(E:Map, (T:Typ, Ts:TypList), (V:Val, Vs:ValList))
    => subtyp(E, T, V) andBool subtyps(E, Ts, Vs)
  rule subtyps(_:Map, _:TypList, _:ValList) => false [owise]

endmodule
