// Environments: values, type definitions, relations, and functions.
//
// Mirrors spec-meta/4-execution/4.00-env.watsup. Each environment is a K
// `Map` held by the cell of its layer (0.1-config.k):
//
//   venv  = map<varr, val>       <lvenv>              Varr |-> Val
//   tdenv = map<id, typDef>      <gtdenv>, <ltdenv>   Id |-> TypDef
//   renv  = map<id, relDefn>     <grenv>              Id |-> RelDefn
//   fenv  = map<id, funcDefn>    <gfenv>              Id |-> FuncDefn
//   fnenv = map<id, id>          <lfenv>              Id |-> Id

requires "../2-runtime/2.1-variable.k"
requires "../2-runtime/2.2-type-def.k"
requires "3.00-result.k"

module ENV
  imports VARIABLE
  imports TYPE-DEF
  imports RESULT
  imports MAP

  // ==========================================================================
  // Value finders and adders
  //
  //   dec $find_varis(ctx, vari*) : val*?
  //   dec $adds_map<varr, val>
  // ==========================================================================

  // The values of the variables, or `ERROR` when one is unbound (`eps` of
  // `$find_varis`, which every caller turns into an error).
  syntax Res ::= finds(VarrList, Map)       [function, total, symbol(finds)]
  rule finds(.VarrList, _:Map) => ok(.ValList)
  rule finds((R:Varr, Rs:VarrList), ((R |-> V:Val) _:Map) #as M:Map)
    => consRes(ok(V), finds(Rs, M))
  rule finds(_:VarrList, _:Map) => ERROR [owise]

  syntax Map ::= addsMap(Map, VarrList, ValList)
                                            [function, symbol(addsMap)]
  rule addsMap(M:Map, .VarrList, .ValList) => M
  rule addsMap(M:Map, (R:Varr, Rs:VarrList), (V:Val, Vs:ValList))
    => addsMap(M[R <- V], Rs, Vs)

endmodule
