// Value-type matching.
//
// Mirrors spec-meta/3-execution/3.05.3-typ-match.watsup. `fenv` is the
// global function environment and `tdenv` the merged type definition
// environment (`updateMap(gtdenv, ltdenv)`).
//
//   syntax subtypRes = ok<bool> | error

requires "3.06.2-typ-equiv.k"

module TYP-MATCH
  imports TYP-EQUIV

  // ==========================================================================
  // Values
  //
  //   dec $subtyp(fenv, tdenv, typ, val) : subtypRes
  // ==========================================================================

  syntax Res ::= subtyp(Map, Map, Typ, Val) [function, total, symbol(subtyp)]

  rule subtyp(_:Map, _:Map, boolT(), boolV(_:Bool)) => ok(true)
  rule subtyp(_:Map, _:Map, natT(), natN(_:Int)) => ok(true)
  rule subtyp(_:Map, _:Map, natT(), intN(I:Int)) => ok(I >=Int 0)
  rule subtyp(_:Map, _:Map, intT(), _:Num) => ok(true)
  rule subtyp(_:Map, _:Map, textT(), textV(_:String)) => ok(true)
  rule subtyp(FE:Map, TD:Map, varT(X:Id, Ts:TArgList), V:Val) => subtypVar(FE, TD, X, Ts, V)
  rule subtyp(FE:Map, TD:Map, tupT(Ts:TypList), tupV(Vs:ValList)) => subtyps(FE, TD, Ts, Vs)
  rule subtyp(FE:Map, TD:Map, iterT(T:Typ, quest()), optV(someVal(V:Val))) => subtyp(FE, TD, T, V)
  rule subtyp(_:Map, _:Map, iterT(_:Typ, quest()), optV(noVal())) => ok(true)
  rule subtyp(FE:Map, TD:Map, iterT(T:Typ, star()), listV(Vs:ValList)) => subtypList(FE, TD, T, Vs)
  rule subtyp(FE:Map, TD:Map, funcT(Ps:TParamList, Ts:TypList, T:Typ), funcV(X:Id))
    => subtypFunc(FE, TD, funcT(Ps, Ts, T), X)
  rule subtyp(_:Map, _:Map, _:Typ, _:Val) => ok(false) [owise]

  // ==========================================================================
  // Named types and type definitions
  //
  //   dec $subtyp_var(fenv, tdenv, id, typ*, val) : subtypRes
  //   dec $subtyp_def(fenv, tdenv, typDef, typ*, val) : subtypRes
  //
  // An unknown name, a wrong number of arguments, an extern type with
  // arguments, and a type parameter are errors.
  // ==========================================================================

  syntax Res ::= subtypVar(Map, Map, Id, TArgList, Val)
                                            [function, total, symbol(subtypVar)]
  rule subtypVar(FE:Map, ((X |-> D:TypDef) _:Map) #as TD:Map, X:Id, Ts:TArgList, V:Val)
    => subtypDef(FE, TD, D, Ts, V)
  rule subtypVar(_:Map, _:Map, _:Id, _:TArgList, _:Val) => ERROR [owise]

  syntax Res ::= subtypDef(Map, Map, TypDef, TArgList, Val)
                                            [function, total, symbol(subtypDef)]
  rule subtypDef(_:Map, _:Map, extTypD(_), .TArgList, V:Val) => subtypExtern(V)
  rule subtypDef(FE:Map, TD:Map, typD(_, Ps:TParamList, aliasDT(T:Typ)), Ts:TArgList, V:Val)
    => subtypRes(FE, TD, substTyp(thetaOf(Ps, Ts), T), V)
    requires sameArity(Ps, Ts)
  rule subtypDef(FE:Map, TD:Map, typD(_, Ps:TParamList, variantDT(Cs:TypCaseList)), Ts:TArgList, V:Val)
    => subtypVariant(FE, TD, thetaOf(Ps, Ts), Cs, V)
    requires sameArity(Ps, Ts)
  rule subtypDef(FE:Map, TD:Map, typD(_, Ps:TParamList, structDT(Fs:TypFieldList)), Ts:TArgList, V:Val)
    => subtypStruct(FE, TD, thetaOf(Ps, Ts), Fs, V)
    requires sameArity(Ps, Ts)
  rule subtypDef(_:Map, _:Map, _:TypDef, _:TArgList, _:Val) => ERROR [owise]

  // `$subtyp` against a substituted type.
  syntax Res ::= subtypRes(Map, Map, Res, Val)
                                            [function, symbol(subtypRes)]
  rule subtypRes(FE:Map, TD:Map, ok(T:Typ), V:Val) => subtyp(FE, TD, T, V)
  rule subtypRes(_:Map, _:Map, F:Failure, _:Val) => F

  // ==========================================================================
  // Extern values
  //
  //   dec $subtyp_extern(val) : subtypRes
  // ==========================================================================

  syntax Res ::= subtypExtern(Val)          [function, total, symbol(subtypExtern)]
  rule subtypExtern(extV(_:JSON)) => ok(true)
  rule subtypExtern(_:Val) => ok(false) [owise]

  // ==========================================================================
  // Variants and structs
  //
  //   dec $subtyp_variant(fenv, tdenv, theta, typCase*, val) : subtypRes
  //   dec $subtyp_struct(fenv, tdenv, theta, typField*, val) : subtypRes
  //   dec $subtyp_case(fenv, tdenv, theta, typ*, val*) : subtypRes
  // ==========================================================================

  syntax Res ::= subtypVariant(Map, Map, Map, TypCaseList, Val)
                                            [function, total, symbol(subtypVariant)]
  rule subtypVariant(FE:Map, TD:Map, Th:Map, Cs:TypCaseList, injV(valCase(M:MixOp, Vs:ValList)))
    => subtypVariantCase(FE, TD, Th, assocCase(M, Cs), Vs)
  rule subtypVariant(_:Map, _:Map, _:Map, _:TypCaseList, _:Val) => ok(false) [owise]

  // `$assoc_<mixop, typ*>(mixop, (mixop_case, typ_case*)*)`
  syntax TypListOpt ::= noTyps()            [symbol(noTyps)]
                      | someTyps(TypList)   [symbol(someTyps)]
  syntax TypListOpt ::= assocCase(MixOp, TypCaseList)
                                            [function, total, symbol(assocCase)]
  rule assocCase(_:MixOp, .TypCaseList) => noTyps()
  rule assocCase(M:MixOp, (typCase(M, Ts:TypList), _:TypCaseList)) => someTyps(Ts)
  rule assocCase(M:MixOp, (_:TypCase, Cs:TypCaseList)) => assocCase(M, Cs) [owise]

  syntax Res ::= subtypVariantCase(Map, Map, Map, TypListOpt, ValList)
                                            [function, total, symbol(subtypVariantCase)]
  rule subtypVariantCase(FE:Map, TD:Map, Th:Map, someTyps(Ts:TypList), Vs:ValList)
    => subtypCase(FE, TD, Th, Ts, Vs)
  rule subtypVariantCase(_:Map, _:Map, _:Map, noTyps(), _:ValList) => ok(false)

  // The fields are matched positionally, with their atoms compared.
  syntax Res ::= subtypStruct(Map, Map, Map, TypFieldList, Val)
                                            [function, total, symbol(subtypStruct)]
  rule subtypStruct(FE:Map, TD:Map, Th:Map, Fs:TypFieldList, strV(Gs:ValFieldList))
    => subtypCase(FE, TD, Th, typsOfFields(Fs), valsOfFields(Gs))
    requires sameAtoms(Fs, Gs)
  rule subtypStruct(_:Map, _:Map, _:Map, _:TypFieldList, _:Val) => ok(false) [owise]

  syntax Bool ::= sameAtoms(TypFieldList, ValFieldList)
                                            [function, total, symbol(sameAtoms)]
  rule sameAtoms(.TypFieldList, .ValFieldList) => true
  rule sameAtoms((typField(A:Atom, _:Typ), Fs:TypFieldList), (valField(A, _:Val), Gs:ValFieldList))
    => sameAtoms(Fs, Gs)
  rule sameAtoms(_:TypFieldList, _:ValFieldList) => false [owise]

  syntax TypList ::= typsOfFields(TypFieldList)
                                            [function, total, symbol(typsOfFields)]
  rule typsOfFields(.TypFieldList) => .TypList
  rule typsOfFields(typField(_:Atom, T:Typ), Fs:TypFieldList) => T, typsOfFields(Fs)

  syntax ValList ::= valsOfFields(ValFieldList)
                                            [function, total, symbol(valsOfFields)]
  rule valsOfFields(.ValFieldList) => .ValList
  rule valsOfFields(valField(_:Atom, V:Val), Gs:ValFieldList) => V, valsOfFields(Gs)

  syntax Res ::= subtypCase(Map, Map, Map, TypList, ValList)
                                            [function, total, symbol(subtypCase)]
  rule subtypCase(FE:Map, TD:Map, Th:Map, Ts:TypList, Vs:ValList)
    => subtypsRes(FE, TD, substTyps(Th, Ts), Vs)

  syntax Res ::= subtypsRes(Map, Map, Res, ValList)
                                            [function, symbol(subtypsRes)]
  rule subtypsRes(FE:Map, TD:Map, ok(Ts:TypList), Vs:ValList) => subtyps(FE, TD, Ts, Vs)
  rule subtypsRes(_:Map, _:Map, F:Failure, _:ValList) => F

  // ==========================================================================
  // Sequences and lists
  //
  //   dec $subtyps(fenv, tdenv, typ*, val*) : subtypRes
  //   dec $subtyp_list(fenv, tdenv, typ, val*) : subtypRes
  // ==========================================================================

  syntax Res ::= subtyps(Map, Map, TypList, ValList)
                                            [function, total, symbol(subtyps)]
  rule subtyps(_:Map, _:Map, .TypList, .ValList) => ok(true)
  rule subtyps(FE:Map, TD:Map, (T:Typ, Ts:TypList), (V:Val, Vs:ValList))
    => subtypsThen(FE, TD, subtyp(FE, TD, T, V), Ts, Vs)
  rule subtyps(_:Map, _:Map, _:TypList, _:ValList) => ok(false) [owise]

  syntax Res ::= subtypsThen(Map, Map, Res, TypList, ValList)
                                            [function, total, symbol(subtypsThen)]
  rule subtypsThen(FE:Map, TD:Map, ok(true), Ts:TypList, Vs:ValList) => subtyps(FE, TD, Ts, Vs)
  rule subtypsThen(_:Map, _:Map, R:Res, _:TypList, _:ValList) => R [owise]

  syntax Res ::= subtypList(Map, Map, Typ, ValList)
                                            [function, total, symbol(subtypList)]
  rule subtypList(_:Map, _:Map, _:Typ, .ValList) => ok(true)
  rule subtypList(FE:Map, TD:Map, T:Typ, (V:Val, Vs:ValList))
    => subtypListThen(FE, TD, subtyp(FE, TD, T, V), T, Vs)

  syntax Res ::= subtypListThen(Map, Map, Res, Typ, ValList)
                                            [function, total, symbol(subtypListThen)]
  rule subtypListThen(FE:Map, TD:Map, ok(true), T:Typ, Vs:ValList) => subtypList(FE, TD, T, Vs)
  rule subtypListThen(_:Map, _:Map, R:Res, _:Typ, _:ValList) => R [owise]

  // ==========================================================================
  // Function signatures and values
  //
  //   dec $param_typ(param) : typ
  //   dec $func_typ(funcDefn) : typ
  //   dec $subtyp_func(fenv, tdenv, typ, id) : subtypRes
  // ==========================================================================

  syntax Typ ::= paramTyp(Param)            [function, total, symbol(paramTyp)]
  rule paramTyp(expParam(T:Typ)) => T
  rule paramTyp(funParam(_:Id, Ps:TParamList, As:ParamList, T:Typ)) => funcT(Ps, paramTyps(As), T)

  syntax TypList ::= paramTyps(ParamList)   [function, total, symbol(paramTyps)]
  rule paramTyps(.ParamList) => .TypList
  rule paramTyps(A:Param, As:ParamList) => paramTyp(A), paramTyps(As)

  syntax Typ ::= funcTyp(FuncDefn)          [function, total, symbol(funcTyp)]
  rule funcTyp(extFuncD(_, Ps:TParamList, As:ParamList, T:Typ)) => funcT(Ps, paramTyps(As), T)
  rule funcTyp(builtinFuncD(_, Ps:TParamList, As:ParamList, T:Typ)) => funcT(Ps, paramTyps(As), T)
  rule funcTyp(tableFuncD(_, As:ParamList, T:Typ, _)) => funcT(.TParamList, paramTyps(As), T)
  rule funcTyp(funcD(_, Ps:TParamList, As:ParamList, T:Typ, _, _)) => funcT(Ps, paramTyps(As), T)

  syntax Res ::= subtypFunc(Map, Map, Typ, Id)
                                            [function, total, symbol(subtypFunc)]
  rule subtypFunc((X |-> D:FuncDefn) _:Map, TD:Map, T:Typ, X:Id) => equivFuncTyp(TD, T, funcTyp(D))
  rule subtypFunc(_:Map, _:Map, _:Typ, _:Id) => ERROR [owise]

endmodule
