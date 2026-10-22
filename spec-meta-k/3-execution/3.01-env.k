// Environments: values, type definitions, relations, and functions.
//
// Mirrors spec-meta/3-execution/3.01-env.watsup.

requires "../0-aux/0.1-config.k"
requires "../2-runtime/2.1-variable.k"
requires "../2-runtime/2.2-type-def.k"

module ENV
  imports CONFIG
  imports VARIABLE
  imports TYPE-DEF
  imports MAP

  // ==========================================================================
  // Type substitution environment
  //
  // A substitution is a tdenv of aliases, one per type parameter.
  // ==========================================================================

  syntax Map ::= tdenvOf(TParamList, TArgList)
                                            [function, symbol(tdenvOf)]

  rule tdenvOf(.TParamList, .TArgList) => .Map
  rule tdenvOf((X:Id, Xs:TParamList), (T:Typ, Ts:TArgList))
    => (X |-> typD(X, .TParamList, aliasDT(T))) tdenvOf(Xs, Ts)

  // ==========================================================================
  // Value finders and adders
  //
  //   dec $find_varrs(ctx, varr*) : val*?
  //   dec $adds_map<varr, val>
  // ==========================================================================

  // The values of the variables, all of which must be bound.
  syntax ValList ::= finds(VarrList, Map)   [function, symbol(finds)]
  rule finds(.VarrList, _:Map) => .ValList
  rule finds((R:Varr, Rs:VarrList), ((R |-> V:Val) _:Map) #as M:Map) => V, finds(Rs, M)

  syntax Map ::= addsMap(Map, VarrList, ValList)
                                            [function, symbol(addsMap)]
  rule addsMap(M:Map, .VarrList, .ValList) => M
  rule addsMap(M:Map, (R:Varr, Rs:VarrList), (V:Val, Vs:ValList))
    => addsMap(M[R <- V], Rs, Vs)

  // ==========================================================================
  // reldef / funcdef
  //
  //   syntax externRelDef = | EXT id
  //   syntax definedRelDef = | DEF rulgroup* elsgroup?
  //   syntax reldef = | externRelDef | definedRelDef
  //   syntax externFuncDef = | EXT id
  //   syntax builtinFuncDef = | BUILTIN id tparam* param*
  //   syntax tableFuncDef = | TABLE param* '->' tblrow*
  //   syntax definedFuncDef = | DEF tparam* '->' clause* elsclause?
  //   syntax funcdef =
  //     | externFuncDef | builtinFuncDef | tableFuncDef | definedFuncDef
  // ==========================================================================

  syntax ExternRelDef ::= extRel(Id)        [symbol(extRel)]
  syntax DefinedRelDef ::= defRel(RulGroupList, ElsGroupOpt)
                                            [symbol(defRel)]
  syntax RelDef ::= ExternRelDef | DefinedRelDef

  syntax ExternFuncDef ::= extFunc(Id)      [symbol(extFunc)]
  syntax BuiltinFuncDef ::= builtinFunc(Id, TParamList, ParamList)
                                            [symbol(builtinFunc)]
  syntax TableFuncDef ::= tableFunc(ParamList, TblRowList)
                                            [symbol(tableFunc)]
  syntax DefinedFuncDef ::= defFunc(TParamList, ClauseList, ElsClauseOpt)
                                            [symbol(defFunc)]
  syntax FuncDef ::= ExternFuncDef
                   | BuiltinFuncDef
                   | TableFuncDef
                   | DefinedFuncDef

endmodule
