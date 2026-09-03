// AL environments: type, relation, and function definitions.
//
// Mirrors spec-meta/al/2-env.watsup.

requires "0-config.k"
requires "../common/2-env.k"

module AL-ENV
  imports AL-CONFIG
  imports COMMON-ENV

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
