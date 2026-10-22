// Environments: values, type definitions, relations, and functions.
//
// Mirrors spec-meta/3-execution/3.01-env.watsup and spec-meta/2-runtime/.

requires "../0-aux/0.1-config.k"

module ENV
  imports CONFIG
  imports MAP

  // ==========================================================================
  // Value environment key
  //
  //   syntax varr = id iter*
  // ==========================================================================

  syntax Varr ::= varr(Id, IterList)        [symbol(varr)]

  syntax VarrList ::= List{Varr, ","}       [symbol(varrList), terminator-symbol(.varrList)]

  // `varr?`
  syntax VarrOpt ::= noVarr()               [symbol(noVarr)]
                   | someVarr(Varr)         [symbol(someVarr)]

  // ==========================================================================
  // Type definition environment
  //
  //   syntax typdef = | PARAM | EXT | DEF tparam* '->' deftyp
  // ==========================================================================

  syntax TypDef ::= paramTD()               [symbol(paramTD)]
                  | extTD()                 [symbol(extTD)]
                  | defTD(TParamList, DefTyp)
                                            [symbol(defTD)]

  // ==========================================================================
  // Type substitution environment
  //
  //   if tdenv = `{ (tparam ':' (DEF eps '->' (ALIAS typ)))* `}
  //
  // A substitution is a tdenv of aliases, one per type parameter.
  // ==========================================================================

  syntax Map ::= tdenvOf(TParamList, TArgList)
                                            [function, symbol(tdenvOf)]

  rule tdenvOf(.TParamList, .TArgList) => .Map
  rule tdenvOf((X:Id, Xs:TParamList), (T:Typ, Ts:TArgList))
    => (X |-> defTD(.TParamList, aliasDT(T))) tdenvOf(Xs, Ts)

  // ==========================================================================
  // Iteration helpers
  //
  //   dec $is_iter_on_var(exp) : varr?
  //   dec $iter_vari(vari, iter) : vari
  //   dec $iter_varr(varr, iter) : varr
  // ==========================================================================

  syntax VarrOpt ::= isIterOnVar(Exp)       [function, total, symbol(isIterOnVar)]

  rule isIterOnVar(varE(X:Id)) => someVarr(varr(X, .IterList))
  rule isIterOnVar(iterE(E:Exp, iterExp(I:Iter, _:VariList)))
    => appendIterOpt(isIterOnVar(E), I)
  rule isIterOnVar(_:Exp) => noVarr() [owise]

  syntax VarrOpt ::= appendIterOpt(VarrOpt, Iter)
                                            [function, total, symbol(appendIterOpt)]

  rule appendIterOpt(noVarr(), _:Iter) => noVarr()
  rule appendIterOpt(someVarr(R:Varr), I:Iter) => someVarr(iterVarr(R, I))

  syntax Vari ::= iterVari(Vari, Iter)      [function, total, symbol(iterVari)]
  syntax Varr ::= iterVarr(Varr, Iter)      [function, total, symbol(iterVarr)]

  rule iterVari(vari(X:Id, T:Typ, Is:IterList), I:Iter)
    => vari(X, T, appendIter(Is, I))
  rule iterVarr(varr(X:Id, Is:IterList), I:Iter)
    => varr(X, appendIter(Is, I))

  syntax IterList ::= appendIter(IterList, Iter)
                                            [function, total, symbol(appendIter)]

  rule appendIter(.IterList, I:Iter) => I, .IterList
  rule appendIter((J:Iter, Is:IterList), I:Iter) => J, appendIter(Is, I)

  syntax Varr ::= varrOfVari(Vari)          [function, total, symbol(varrOfVari)]
  rule varrOfVari(vari(X:Id, _:Typ, Is:IterList)) => varr(X, Is)

  syntax VarrList ::= varrsOfVaris(VariList)
                                            [function, total, symbol(varrsOfVaris)]
  rule varrsOfVaris(.VariList) => .VarrList
  rule varrsOfVaris(V:Vari, Vs:VariList) => varrOfVari(V), varrsOfVaris(Vs)

  // `vari*` at one more `iter`.
  syntax VarrList ::= iterVaris(VariList, Iter)
                                            [function, total, symbol(iterVaris)]
  rule iterVaris(.VariList, _:Iter) => .VarrList
  rule iterVaris((V:Vari, Vs:VariList), I:Iter)
    => varrOfVari(iterVari(V, I)), iterVaris(Vs, I)

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
