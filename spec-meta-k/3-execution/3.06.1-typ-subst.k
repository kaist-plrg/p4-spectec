// Type substitution, type casts, and value-type matching.
//
// Mirrors spec-meta/3-execution/3.05.1-typ-subst.watsup to
// 3.05.4-typ-cast.watsup.
//
//   dec $subst_typ(theta, typ) : typ
//   dec $upcast(ctx, typ, val) : res<val>
//   dec $downcast(ctx, typ, val) : res<val>
//   dec $subtyp(tdenv, typ, val) : bool
//   dec $subtyps(tdenv, typ*, val*) : bool
//
// A substitution is a tdenv of aliases (`tdenvOf` in 3.01-env.k), and the casts
// take the merged tdenv `updateMap(gtdenv, ltdenv)` like `$subtyp` does.

requires "3.02-context.k"
requires "3.05-eval-ops.k"

module TYP
  imports CONTEXT
  imports EVAL-OPS
  imports MAP
  imports BOOL
  imports K-EQUAL

  // ==========================================================================
  // Type substitution
  // ==========================================================================

  syntax Typ ::= substTyp(Map, Typ)         [function, symbol(substTyp)]
  syntax TypList ::= substTyps(Map, TypList)
                                            [function, total, symbol(substTyps)]
  syntax TArgList ::= substTargs(Map, TArgList)
                                            [function, total, symbol(substTargs)]

  rule substTyp(_:Map, T:OpTyp) => T
  rule substTyp(_:Map, funcT()) => funcT()
  rule substTyp((X:Id |-> defTD(.TParamList, aliasDT(T:Typ))) _:Map, varT(X, .TArgList)) => T
  rule substTyp(Th:Map, varT(X:Id, Ts:TArgList)) => varT(X, substTargs(Th, Ts)) [owise]
  rule substTyp(Th:Map, tupT(Ts:TypList)) => tupT(substTyps(Th, Ts))
  rule substTyp(Th:Map, iterT(T:Typ, I:Iter)) => iterT(substTyp(Th, T), I)

  rule substTyps(_:Map, .TypList) => .TypList
  rule substTyps(Th:Map, (T:Typ, Ts:TypList)) => substTyp(Th, T), substTyps(Th, Ts)

  rule substTargs(_:Map, .TArgList) => .TArgList
  rule substTargs(Th:Map, (T:Typ, Ts:TArgList)) => substTyp(Th, T), substTargs(Th, Ts)

  syntax TypList ::= typsOfTargs(TArgList)  [function, total, symbol(typsOfTargs)]
  rule typsOfTargs(.TArgList) => .TypList
  rule typsOfTargs((T:Typ, Ts:TArgList)) => T, typsOfTargs(Ts)

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

  syntax Res ::= upcast(Map, Typ, Val)      [function, symbol(upcast)]

  rule upcast(_:Map, intT(), intN(I:Int)) => ok(intN(I))
  rule upcast(_:Map, intT(), natN(N:Int)) => ok(intN(N))
  rule upcast(_:Map, intT(), V:Val) => FAIL
    requires notBool isNum(V)

  rule upcast(((X:Id |-> defTD(Ps:TParamList, aliasDT(T:Typ))) _:Map) #as E:Map,
              varT(X, Ts:TArgList), V:Val)
    => upcast(E, substTyp(tdenvOf(Ps, Ts), T), V)

  rule upcast(E:Map, tupT(Ts:TypList), tupV(Vs:ValList)) => resTup(upcasts(E, Ts, Vs))
  rule upcast(_:Map, tupT(_:TypList), V:Val) => FAIL
    requires notBool isTup(V)

  rule upcast(_:Map, iterT(_:Typ, quest()), optV(noVal())) => ok(optV(noVal()))
  rule upcast(E:Map, iterT(T:Typ, quest()), optV(someVal(V:Val))) => resOpt(upcast(E, T, V))
  rule upcast(E:Map, iterT(T:Typ, star()), listV(Vs:ValList))
    => resList(upcasts(E, Typs2Repeat(T, Vs), Vs))

  rule upcast(_:Map, _:Typ, V:Val) => ok(V) [owise]

  // ==========================================================================
  // Downcasts
  //
  //   def $downcast(C, NAT, NAT n) = OK (NAT n)
  //   def $downcast(C, NAT, INT i) = OK (NAT n)  -- if n = i
  //   def $downcast(C, NAT, val) = FAIL          -- if ~(val <: num)
  //   ... (as for upcasts)
  // ==========================================================================

  syntax Res ::= downcast(Map, Typ, Val)    [function, symbol(downcast)]

  rule downcast(_:Map, natT(), natN(N:Int)) => ok(natN(N))
  rule downcast(_:Map, natT(), intN(I:Int)) => ok(natN(I))
    requires I >=Int 0
  rule downcast(_:Map, natT(), V:Val) => FAIL
    requires notBool isNum(V)

  rule downcast(((X:Id |-> defTD(Ps:TParamList, aliasDT(T:Typ))) _:Map) #as E:Map,
                varT(X, Ts:TArgList), V:Val)
    => downcast(E, substTyp(tdenvOf(Ps, Ts), T), V)

  rule downcast(E:Map, tupT(Ts:TypList), tupV(Vs:ValList)) => resTup(downcasts(E, Ts, Vs))
  rule downcast(_:Map, tupT(_:TypList), V:Val) => FAIL
    requires notBool isTup(V)

  rule downcast(_:Map, iterT(_:Typ, quest()), optV(noVal())) => ok(optV(noVal()))
  rule downcast(E:Map, iterT(T:Typ, quest()), optV(someVal(V:Val))) => resOpt(downcast(E, T, V))
  rule downcast(E:Map, iterT(T:Typ, star()), listV(Vs:ValList))
    => resList(downcasts(E, Typs2Repeat(T, Vs), Vs))

  rule downcast(_:Map, _:Typ, V:Val) => ok(V) [owise]

  // ==========================================================================
  // Cast helpers
  // ==========================================================================

  syntax Res ::= upcasts(Map, TypList, ValList)
                                            [function, symbol(upcasts)]
               | downcasts(Map, TypList, ValList)
                                            [function, symbol(downcasts)]

  rule upcasts(_:Map, .TypList, .ValList) => ok(.ValList)
  rule upcasts(E:Map, (T:Typ, Ts:TypList), (V:Val, Vs:ValList))
    => consRes(upcast(E, T, V), upcasts(E, Ts, Vs))

  rule downcasts(_:Map, .TypList, .ValList) => ok(.ValList)
  rule downcasts(E:Map, (T:Typ, Ts:TypList), (V:Val, Vs:ValList))
    => consRes(downcast(E, T, V), downcasts(E, Ts, Vs))

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

  // ==========================================================================
  // Value-type matching
  //
  //   dec $subtyp(tdenv, typ, val) : bool
  //   dec $subtyps(tdenv, typ*, val*) : bool
  // ==========================================================================

  syntax Bool ::= subtyp(Map, Typ, Val)     [function, symbol(subtyp)]
                | subtyps(Map, TypList, ValList)
                                            [function, total, symbol(subtyps)]

  rule subtyp(_:Map, boolT(), boolV(_:Bool)) => true
  rule subtyp(_:Map, natT(), natN(_:Int)) => true
  rule subtyp(_:Map, natT(), intN(I:Int)) => I >=Int 0
  rule subtyp(_:Map, intT(), _:Num) => true
  rule subtyp(_:Map, textT(), textV(_:String)) => true

  rule subtyp((X:Id |-> extTD()) _:Map, varT(X, _:TArgList), extV(_:JSON)) => true
  rule subtyp(((X:Id |-> defTD(Ps:TParamList, aliasDT(T:Typ))) _:Map) #as E:Map,
              varT(X, Ts:TArgList), V:Val)
    => subtyp(E, substTyp(tdenvOf(Ps, Ts), T), V)
  rule subtyp(((X:Id |-> defTD(Ps:TParamList, variantDT(Cs:TypCaseList))) _:Map) #as E:Map,
              varT(X, Ts:TArgList), injV(valCase(M:MixOp, Vs:ValList)))
    => subtypCases(E, tdenvOf(Ps, Ts), M, Cs, Vs)
  rule subtyp(((X:Id |-> defTD(Ps:TParamList, structDT(Fs:TypFieldList))) _:Map) #as E:Map,
              varT(X, Ts:TArgList), strV(Gs:ValFieldList))
    => subtypFields(E, tdenvOf(Ps, Ts), Fs, Gs)

  rule subtyp(E:Map, tupT(Ts:TypList), tupV(Vs:ValList)) => subtyps(E, Ts, Vs)

  rule subtyp(_:Map, iterT(_:Typ, quest()), optV(noVal())) => true
  rule subtyp(E:Map, iterT(T:Typ, quest()), optV(someVal(V:Val)))
    => subtyp(E, T, V)
  rule subtyp(E:Map, iterT(T:Typ, star()), listV(Vs:ValList))
    => subtyps(E, Typs2Repeat(T, Vs), Vs)

  rule subtyp(_:Map, _:Typ, _:Val) => false [owise]

  // The case of the mixop, if any, with its types substituted.
  syntax Bool ::= subtypCases(Map, Map, MixOp, TypCaseList, ValList)
                                            [function, total, symbol(subtypCases)]
  rule subtypCases(E:Map, Th:Map, M:MixOp, (typCase(M, Ts:TypList), _:TypCaseList), Vs:ValList)
    => subtyps(E, substTyps(Th, Ts), Vs)
  rule subtypCases(E:Map, Th:Map, M:MixOp, (_:TypCase, Cs:TypCaseList), Vs:ValList)
    => subtypCases(E, Th, M, Cs, Vs) [owise]
  rule subtypCases(_:Map, _:Map, _:MixOp, .TypCaseList, _:ValList) => false

  syntax Bool ::= subtypFields(Map, Map, TypFieldList, ValFieldList)
                                            [function, total, symbol(subtypFields)]
  rule subtypFields(_:Map, _:Map, .TypFieldList, .ValFieldList) => true
  rule subtypFields(E:Map, Th:Map,
                    (typField(A:Atom, T:Typ), Fs:TypFieldList),
                    (valField(A, V:Val), Gs:ValFieldList))
    => subtyp(E, substTyp(Th, T), V) andBool subtypFields(E, Th, Fs, Gs)
  rule subtypFields(_:Map, _:Map, _:TypFieldList, _:ValFieldList) => false [owise]

  rule subtyps(_:Map, .TypList, .ValList) => true
  rule subtyps(E:Map, (T:Typ, Ts:TypList), (V:Val, Vs:ValList))
    => subtyp(E, T, V) andBool subtyps(E, Ts, Vs)
  rule subtyps(_:Map, _:TypList, _:ValList) => false [owise]

endmodule
