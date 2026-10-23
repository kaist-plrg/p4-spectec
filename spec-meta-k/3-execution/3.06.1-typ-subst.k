// Type substitution.
//
// Mirrors spec-meta/3-execution/3.05.1-typ-subst.watsup.
//
//   syntax theta = map<id, typ>      a K Map, Id |-> Typ

requires "3.03-relation.k"

module TYP-SUBST
  imports RELATION
  imports SET
  imports K-EQUAL

  // ==========================================================================
  // Substitution environments
  //
  //   dec $theta_of_tdenv(tdenv) : theta
  //
  // The aliases without type parameters of a tdenv.
  // ==========================================================================

  syntax Map ::= thetaOfTdenv(Map)          [function, total, symbol(thetaOfTdenv)]
               | thetaOfTdenv(List, Map)    [function, total, symbol(thetaOfTdenvKeys)]
  rule thetaOfTdenv(M:Map) => thetaOfTdenv(keys_list(M), M)
  rule thetaOfTdenv(.List, _:Map) => .Map
  rule thetaOfTdenv(ListItem(X:KItem) Xs:List,
                    ((X |-> typD(_, .TParamList, aliasDT(T:Typ))) _:Map) #as M:Map)
    => (X |-> T) thetaOfTdenv(Xs, M)
  rule thetaOfTdenv(ListItem(_) Xs:List, M:Map) => thetaOfTdenv(Xs, M) [owise]

  // `{ (tparam ':' targ)* }`: as many arguments as parameters (`sameArity`).
  syntax Map ::= thetaOf(TParamList, TArgList)
                                            [function, symbol(thetaOf)]
  rule thetaOf(.TParamList, .TArgList) => .Map
  rule thetaOf((P:Id, Ps:TParamList), (T:Typ, Ts:TArgList)) => (P |-> T) thetaOf(Ps, Ts)

  syntax Bool ::= sameArity(TParamList, TArgList)
                                            [function, total, symbol(sameArity)]
  rule sameArity(.TParamList, .TArgList) => true
  rule sameArity((_:Id, Ps:TParamList), (_:Typ, Ts:TArgList)) => sameArity(Ps, Ts)
  rule sameArity(_:TParamList, _:TArgList) => false [owise]

  // ==========================================================================
  // Type names and fresh binders
  //
  //   dec $ids_typ(typ) : id*
  //   dec $ids_typs(typ*) : id*
  //   dec $fresh_id(id, id*) : id
  //   dec $fresh_ids(id*, id*) : id*
  //
  // The names used are a `Set`; fresh names are made in order.
  // ==========================================================================

  syntax Set ::= idsTyp(Typ)                [function, total, symbol(idsTyp)]
               | idsTyps(TypList)           [function, total, symbol(idsTyps)]
               | idsTargs(TArgList)         [function, total, symbol(idsTargs)]
               | idsTParams(TParamList)     [function, total, symbol(idsTParams)]

  rule idsTyp(varT(X:Id, Ts:TArgList)) => SetItem(X) |Set idsTargs(Ts)
  rule idsTyp(tupT(Ts:TypList)) => idsTyps(Ts)
  rule idsTyp(iterT(T:Typ, _:Iter)) => idsTyp(T)
  rule idsTyp(funcT(Ps:TParamList, Ts:TypList, T:Typ))
    => idsTParams(Ps) |Set idsTyps(Ts) |Set idsTyp(T)
  rule idsTyp(_:Typ) => .Set [owise]

  rule idsTyps(.TypList) => .Set
  rule idsTyps(T:Typ, Ts:TypList) => idsTyp(T) |Set idsTyps(Ts)

  rule idsTargs(.TArgList) => .Set
  rule idsTargs(T:Typ, Ts:TArgList) => idsTyp(T) |Set idsTargs(Ts)

  rule idsTParams(.TParamList) => .Set
  rule idsTParams(P:Id, Ps:TParamList) => SetItem(P) |Set idsTParams(Ps)

  // The names of a substitution: `id* ++ $ids_typs(typ_arg*)`.
  syntax Set ::= idsTheta(Map)              [function, total, symbol(idsTheta)]
               | idsTypItems(List)          [function, total, symbol(idsTypItems)]
  rule idsTheta(Th:Map) => keys(Th) |Set idsTypItems(values(Th))
  rule idsTypItems(.List) => .Set
  rule idsTypItems(ListItem(T:Typ) Ts:List) => idsTyp(T) |Set idsTypItems(Ts)
  rule idsTypItems(ListItem(_) Ts:List) => idsTypItems(Ts) [owise]

  syntax Id ::= freshId(Id, Set)            [function, total, symbol(freshId)]
  rule freshId(X:Id, Used:Set) => X
    requires notBool (X in Used)
  rule freshId(X:Id, Used:Set) => freshId({X}:>String +String "'", Used)
    requires X in Used

  syntax TParamList ::= freshIds(TParamList, Set)
                                            [function, total, symbol(freshIds)]
                      | freshIdsCons(Id, TParamList, Set)
                                            [function, total, symbol(freshIdsCons)]
  rule freshIds(.TParamList, _:Set) => .TParamList
  rule freshIds((X:Id, Xs:TParamList), Used:Set) => freshIdsCons(freshId(X, Used), Xs, Used)
  rule freshIdsCons(Y:Id, Xs:TParamList, Used:Set) => Y, freshIds(Xs, SetItem(Y) |Set Used)

  // ==========================================================================
  // Substitution in types and type sequences
  //
  //   syntax typRes = ok<typ> | error
  //   dec $subst_typ(theta, typ) : typRes
  //   dec $subst_typs(theta, typ*) : typsRes
  //
  // A parameterized use of a substituted name is an error.
  // ==========================================================================

  syntax Res ::= substTyp(Map, Typ)         [function, total, symbol(substTyp)]
               | substTyps(Map, TypList)    [function, total, symbol(substTyps)]
               | substTargs(Map, TArgList)  [function, total, symbol(substTargs)]

  rule substTyp(_:Map, T:OpTyp) => ok(T)
  rule substTyp((X |-> T:Typ) _:Map, varT(X:Id, .TArgList)) => ok(T)
  rule substTyp(Th:Map, varT(X:Id, Ts:TArgList)) => resVarT(X, substTargs(Th, Ts))
    requires notBool (X in_keys(Th))
  rule substTyp(Th:Map, tupT(Ts:TypList)) => resTupT(substTyps(Th, Ts))
  rule substTyp(Th:Map, iterT(T:Typ, I:Iter)) => resIterT(substTyp(Th, T), I)
  rule substTyp(Th:Map, funcT(Ps:TParamList, Ts:TypList, T:Typ))
    => substFuncTyp(Th, freshIds(Ps, idsTheta(Th) |Set idsTyp(funcT(Ps, Ts, T))), Ps, Ts, T)
  rule substTyp(_:Map, _:Typ) => ERROR [owise]

  rule substTyps(_:Map, .TypList) => ok(.TypList)
  rule substTyps(Th:Map, (T:Typ, Ts:TypList)) => consRes(substTyp(Th, T), substTyps(Th, Ts))

  rule substTargs(_:Map, .TArgList) => ok(.TArgList)
  rule substTargs(Th:Map, (T:Typ, Ts:TArgList)) => consRes(substTyp(Th, T), substTargs(Th, Ts))

  // A function type: its type parameters renamed apart from the names of
  // the substitution and of the type, then its parameter and result types
  // substituted.
  syntax Res ::= substFuncTyp(Map, TParamList, TParamList, TypList, Typ)
                                            [function, total, symbol(substFuncTyp)]
  rule substFuncTyp(Th:Map, PsF:TParamList, Ps:TParamList, Ts:TypList, T:Typ)
    => resFuncT(PsF,
                substTypsRes(Th, substTyps(thetaFresh(Ps, PsF), Ts)),
                substTypRes(Th, substTyp(thetaFresh(Ps, PsF), T)))

  // `{ (tparam ':' (VAR tparam_fresh eps))* }`
  syntax Map ::= thetaFresh(TParamList, TParamList)
                                            [function, symbol(thetaFresh)]
  rule thetaFresh(.TParamList, .TParamList) => .Map
  rule thetaFresh((P:Id, Ps:TParamList), (Q:Id, Qs:TParamList))
    => (P |-> varT(Q, .TArgList)) thetaFresh(Ps, Qs)

  // Substitution into an `OK` type or types.
  syntax Res ::= substTypRes(Map, Res)      [function, symbol(substTypRes)]
               | substTypsRes(Map, Res)     [function, symbol(substTypsRes)]
  rule substTypRes(Th:Map, ok(T:Typ)) => substTyp(Th, T)
  rule substTypRes(_:Map, F:Failure) => F
  rule substTypsRes(Th:Map, ok(Ts:TypList)) => substTyps(Th, Ts)
  rule substTypsRes(_:Map, F:Failure) => F

  // The type built from `OK` parts, or the first failure.
  syntax Res ::= resVarT(Id, Res)           [function, symbol(resVarT)]
               | resTupT(Res)               [function, symbol(resTupT)]
               | resIterT(Res, Iter)        [function, symbol(resIterT)]
               | resFuncT(TParamList, Res, Res)
                                            [function, symbol(resFuncT)]
  rule resVarT(X:Id, ok(Ts:TArgList)) => ok(varT(X, Ts))
  rule resVarT(_:Id, F:Failure) => F
  rule resTupT(ok(Ts:TypList)) => ok(tupT(Ts))
  rule resTupT(F:Failure) => F
  rule resIterT(ok(T:Typ), I:Iter) => ok(iterT(T, I))
  rule resIterT(F:Failure, _:Iter) => F
  rule resFuncT(Ps:TParamList, ok(Ts:TypList), ok(T:Typ)) => ok(funcT(Ps, Ts, T))
  rule resFuncT(_:TParamList, F:Failure, _:Res) => F
  rule resFuncT(_:TParamList, ok(_), F:Failure) => F

  // ==========================================================================
  // Type sequence helpers
  // ==========================================================================

  syntax TypList ::= typsOfTargs(TArgList)  [function, total, symbol(typsOfTargs)]
  rule typsOfTargs(.TArgList) => .TypList
  rule typsOfTargs((T:Typ, Ts:TArgList)) => T, typsOfTargs(Ts)

  syntax Int ::= lengthTyps(TypList)        [function, total, symbol(lengthTyps)]
               | lengthTParams(TParamList)  [function, total, symbol(lengthTParams)]
  rule lengthTyps(.TypList) => 0
  rule lengthTyps(_:Typ, Ts:TypList) => 1 +Int lengthTyps(Ts)
  rule lengthTParams(.TParamList) => 0
  rule lengthTParams(_:Id, Ps:TParamList) => 1 +Int lengthTParams(Ps)

  // `typ* ++ [ typ ]`
  syntax TypList ::= snocTyp(TypList, Typ)  [function, total, symbol(snocTyp)]
  rule snocTyp(.TypList, T:Typ) => T, .TypList
  rule snocTyp((T':Typ, Ts:TypList), T:Typ) => T', snocTyp(Ts, T)

  // `$repeat_<typ>(typ, n)`
  syntax TypList ::= repeatTyp(Typ, Int)    [function, total, symbol(repeatTyp)]
  rule repeatTyp(_:Typ, N:Int) => .TypList requires N <=Int 0
  rule repeatTyp(T:Typ, N:Int) => T, repeatTyp(T, N -Int 1) requires N >Int 0

endmodule
