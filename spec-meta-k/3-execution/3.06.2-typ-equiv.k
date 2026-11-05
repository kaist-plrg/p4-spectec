// Type equivalence.
//
// Mirrors spec-meta/2-runtime/2.5-typ-equiv.watsup. The `tdenv` is the
// merged type definition environment (`$adds_map(C.GLOBAL.TYP, ..local..)`,
// `updateMap(gtdenv, ltdenv)` here).

requires "3.06.1-typ-subst.k"

module TYP-EQUIV
  imports TYP-SUBST

  // ==========================================================================
  // Outer alias expansion
  //
  //   dec $expand_typ(tdenv, typ) : typRes
  //
  // An unknown name, a parameterized use of a type parameter or an extern
  // type, and a wrong number of arguments are errors.
  // ==========================================================================

  syntax Res ::= expandTyp(Typ, Map)        [function, total, symbol(expandTyp)]

  rule expandTyp(varT(X:Id, Ts:TArgList), ((X |-> D:TypDef) _:Map) #as TD:Map)
    => expandVarT(TD, D, X, Ts)
  rule expandTyp(T:OpTyp, _:Map) => ok(T)
  rule expandTyp(tupT(Ts:TypList), _:Map) => ok(tupT(Ts))
  rule expandTyp(iterT(T:Typ, I:Iter), _:Map) => ok(iterT(T, I))
  rule expandTyp(funcT(Ps:TParamList, Ts:TypList, T:Typ), _:Map) => ok(funcT(Ps, Ts, T))
  rule expandTyp(_:Typ, _:Map) => ERROR [owise]

  syntax Res ::= expandVarT(Map, TypDef, Id, TArgList)
                                            [function, total, symbol(expandVarT)]
  rule expandVarT(TD:Map, typD(_, Ps:TParamList, aliasDT(T:Typ)), _:Id, Ts:TArgList)
    => expandTypRes(TD, substTyp(thetaOf(Ps, Ts), T))
    requires sameArity(Ps, Ts)
  rule expandVarT(_:Map, paramTD(), X:Id, .TArgList) => ok(varT(X, .TArgList))
  rule expandVarT(_:Map, extTypD(_), X:Id, .TArgList) => ok(varT(X, .TArgList))
  rule expandVarT(_:Map, typD(_, Ps:TParamList, structDT(_)), X:Id, Ts:TArgList) => ok(varT(X, Ts))
    requires sameArity(Ps, Ts)
  rule expandVarT(_:Map, typD(_, Ps:TParamList, variantDT(_)), X:Id, Ts:TArgList) => ok(varT(X, Ts))
    requires sameArity(Ps, Ts)
  rule expandVarT(_:Map, _:TypDef, _:Id, _:TArgList) => ERROR [owise]

  syntax Res ::= expandTypRes(Map, Res)     [function, symbol(expandTypRes)]
  rule expandTypRes(TD:Map, ok(T:Typ)) => expandTyp(T, TD)
  rule expandTypRes(_:Map, F:Failure) => F

  // ==========================================================================
  // Structural equivalence
  //
  //   syntax equivRes = ok<bool> | error
  //   dec $equiv_typ(tdenv, typ, typ) : equivRes
  //   dec $equiv_typs(tdenv, typ*, typ*) : equivRes
  //   dec $equiv_typ'(tdenv, typ, typ) : equivRes     -- equivExpandedTyp
  //
  // Nested function types are not structurally equivalent to anything: only
  // function-value membership invokes signature alpha-equivalence.
  // ==========================================================================

  syntax Res ::= equivTyp(Map, Typ, Typ)    [function, total, symbol(equivTyp)]
               | equivTyps(Map, TypList, TypList)
                                            [function, total, symbol(equivTyps)]
               | equivExpandedTyp(Map, Typ, Typ)
                                            [function, total, symbol(equivExpandedTyp)]

  rule equivTyp(TD:Map, Tl:Typ, Tr:Typ) => equivTypRes(TD, expandTyp(Tl, TD), expandTyp(Tr, TD))

  syntax Res ::= equivTypRes(Map, Res, Res) [function, total, symbol(equivTypRes)]
  rule equivTypRes(TD:Map, ok(Tl:Typ), ok(Tr:Typ)) => equivExpandedTyp(TD, Tl, Tr)
  rule equivTypRes(_:Map, _:Res, _:Res) => ERROR [owise]

  rule equivTyps(_:Map, .TypList, .TypList) => ok(true)
  rule equivTyps(TD:Map, (Tl:Typ, Tls:TypList), (Tr:Typ, Trs:TypList))
    => equivTypsRes(TD, equivTyp(TD, Tl, Tr), Tls, Trs)
  rule equivTyps(_:Map, _:TypList, _:TypList) => ok(false) [owise]

  syntax Res ::= equivTypsRes(Map, Res, TypList, TypList)
                                            [function, total, symbol(equivTypsRes)]
  rule equivTypsRes(TD:Map, ok(true), Tls:TypList, Trs:TypList) => equivTyps(TD, Tls, Trs)
  rule equivTypsRes(_:Map, R:Res, _:TypList, _:TypList) => R [owise]

  rule equivExpandedTyp(_:Map, boolT(), boolT()) => ok(true)
  rule equivExpandedTyp(_:Map, natT(), natT()) => ok(true)
  rule equivExpandedTyp(_:Map, intT(), intT()) => ok(true)
  rule equivExpandedTyp(_:Map, textT(), textT()) => ok(true)
  rule equivExpandedTyp(TD:Map, varT(X:Id, Tls:TArgList), varT(X, Trs:TArgList))
    => equivTyps(TD, typsOfTargs(Tls), typsOfTargs(Trs))
  rule equivExpandedTyp(TD:Map, tupT(Tls:TypList), tupT(Trs:TypList)) => equivTyps(TD, Tls, Trs)
  rule equivExpandedTyp(TD:Map, iterT(Tl:Typ, I:Iter), iterT(Tr:Typ, I)) => equivTyp(TD, Tl, Tr)
  rule equivExpandedTyp(_:Map, _:Typ, _:Typ) => ok(false) [owise]

  // ==========================================================================
  // Function signature alpha-equivalence
  //
  //   dec $equiv_func_typ(tdenv, typ, typ) : equivRes
  //
  // Both signatures' type parameters are renamed to the same fresh names,
  // bound as `PARAM`, and the parameter and result types compared pairwise.
  // ==========================================================================

  syntax Res ::= equivFuncTyp(Map, Typ, Typ)
                                            [function, total, symbol(equivFuncTyp)]
  rule equivFuncTyp(TD:Map, funcT(Pl:TParamList, Tl:TypList, Rl:Typ), funcT(Pr:TParamList, Tr:TypList, Rr:Typ))
    => equivFuncTypFresh(TD,
         freshIds(Pl, keys(TD) |Set idsTyp(funcT(Pl, Tl, Rl)) |Set idsTyp(funcT(Pr, Tr, Rr))),
         Pl, snocTyp(Tl, Rl), Pr, snocTyp(Tr, Rr))
    requires lengthTParams(Pl) ==Int lengthTParams(Pr) andBool lengthTyps(Tl) ==Int lengthTyps(Tr)
  rule equivFuncTyp(_:Map, funcT(Pl:TParamList, Tl:TypList, _), funcT(Pr:TParamList, Tr:TypList, _)) => ok(false)
    requires lengthTParams(Pl) =/=Int lengthTParams(Pr) orBool lengthTyps(Tl) =/=Int lengthTyps(Tr)
  rule equivFuncTyp(_:Map, _:Typ, _:Typ) => ERROR [owise]

  syntax Res ::= equivFuncTypFresh(Map, TParamList, TParamList, TypList, TParamList, TypList)
                                            [function, total, symbol(equivFuncTypFresh)]
  rule equivFuncTypFresh(TD:Map, PsF:TParamList, Pl:TParamList, Tls:TypList, Pr:TParamList, Trs:TypList)
    => equivFuncTypSubst(addParams(TD, PsF),
                         substTyps(thetaFresh(Pl, PsF), Tls),
                         substTyps(thetaFresh(Pr, PsF), Trs))

  syntax Res ::= equivFuncTypSubst(Map, Res, Res)
                                            [function, total, symbol(equivFuncTypSubst)]
  rule equivFuncTypSubst(TD:Map, ok(Tls:TypList), ok(Trs:TypList)) => equivTyps(TD, Tls, Trs)
  rule equivFuncTypSubst(_:Map, _:Res, _:Res) => ERROR [owise]

  // `$adds_map<id, typDef>(tdenv, tparam_fresh*, $repeat_<typDef>(PARAM, ..))`
  syntax Map ::= addParams(Map, TParamList) [function, total, symbol(addParams)]
  rule addParams(TD:Map, .TParamList) => TD
  rule addParams(TD:Map, (P:Id, Ps:TParamList)) => addParams(TD[P <- paramTD()], Ps)

endmodule
