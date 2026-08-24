// AL environments: type, relation, and function definitions.
//
// Mirrors spec-meta/al/2-env.watsup.

requires "0-config.k"
requires "../common/2-env.k"

module AL-ENV
  imports AL-CONFIG
  imports COMMON-ENV

  // ==========================================================================
  // typdef / reldef / funcdef
  //
  //   syntax typdef = | PARAM | EXT | DEF tparam* '->' deftyp
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

  syntax TypDef ::= paramTD()               [symbol(paramTD)]
                  | extTD()                 [symbol(extTD)]
                  | defTD(TParamList, DefTyp)
                                            [symbol(defTD)]

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

  // ==========================================================================
  // Type substitution environment
  //
  //   dec $theta_of_tdenv(tdenv) : theta
  //   dec $extend_tdenv(tdenv, tdenv) : tdenv
  //
  // `theta = map<id, typ>` is a `Map` too (`Id |-> Typ`), folded from a
  // `tdenv : Map` (`Id |-> TypDef`).  Only `DEF eps '->' (ALIAS typ)` bindings
  // contribute; everything else is skipped, as the watsup clauses (and their
  // absence) prescribe.
  // ==========================================================================

  syntax Map ::= thetaOfTdenv(Map)          [function, symbol(thetaOfTdenv)]

  rule thetaOfTdenv(.Map) => .Map
  rule thetaOfTdenv(X:Id |-> defTD(.TParamList, aliasDT(T:Typ)) M:Map)
    => (X |-> T) thetaOfTdenv(M)
  rule thetaOfTdenv(_X:Id |-> TD:TypDef M:Map) => thetaOfTdenv(M)
    requires notBool isAliasNoParams(TD)

  syntax Bool ::= isAliasNoParams(TypDef)   [function, total, symbol(isAliasNoParams)]
  rule isAliasNoParams(defTD(.TParamList, aliasDT(_:Typ))) => true
  rule isAliasNoParams(_:TypDef) => false [owise]

  //   dec $extend_tdenv(tdenv, tdenv) : tdenv
  //
  // `$add_map` overwrites in place, so `$extend_tdenv(G, L)` is exactly K's
  // right-biased `Map` update.
  syntax Map ::= extendTdenv(Map, Map)      [function, total, symbol(extendTdenv)]
  rule extendTdenv(G:Map, L:Map) => updateMap(G, L)

endmodule
