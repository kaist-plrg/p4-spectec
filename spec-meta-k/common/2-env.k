// Common environment keys and value-independent environment helpers.
//
// Mirrors spec-meta/common/2-env.watsup.

requires "1-syntax.k"

module COMMON-ENV
  imports COMMON-SYNTAX
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

  // `varr` of a `vari`, dropping the type: `id _ iter*` |-> `id iter*`.
  syntax Varr ::= varrOfVari(Vari)          [function, total, symbol(varrOfVari)]
  rule varrOfVari(vari(X:Id, _:Typ, Is:IterList)) => varr(X, Is)

  syntax VarrList ::= varrsOfVaris(VariList)
                                            [function, total, symbol(varrsOfVaris)]
  rule varrsOfVaris(.VariList) => .VarrList
  rule varrsOfVaris(V:Vari, Vs:VariList) => varrOfVari(V), varrsOfVaris(Vs)

endmodule
