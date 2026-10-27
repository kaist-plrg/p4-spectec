// Runtime variables and the iteration helpers.
//
// Mirrors spec-meta/2-runtime/2.1-variable.watsup, with `varr` of
// spec-meta/4-execution/4.00-env.watsup and `$is_iter_on_var` of
// spec-meta/4-execution/4.01-context.watsup.

requires "../1-syntax/1.0-syntax.k"

module VARIABLE
  imports SYNTAX

  // ==========================================================================
  // Runtime variables
  //
  //   syntax varr = id iter*
  // ==========================================================================

  syntax Varr ::= varr(Id, IterList)        [symbol(varr)]

  syntax VarrList ::= List{Varr, ","}       [symbol(varrList), terminator-symbol(.varrList)]

  // `varr?`
  syntax VarrOpt ::= noVarr()               [symbol(noVarr)]
                   | someVarr(Varr)         [symbol(someVarr)]

  // ==========================================================================
  // Iteration helpers
  //
  //   dec $is_iter_on_var(exp) : varr?
  //   dec $iter_vari(vari, iter) : vari
  // ==========================================================================

  syntax VarrOpt ::= isIterOnVar(Exp)       [function, total, symbol(isIterOnVar)]

  rule isIterOnVar(varE(X:Id)) => someVarr(varr(X, .IterList))
  rule isIterOnVar(iterE(E:Exp, iterExp(I:Iter, _:VariList)))
    => appendIterOpt(isIterOnVar(E), I)
  rule isIterOnVar(_:Exp) => noVarr() [owise]

  syntax VarrOpt ::= appendIterOpt(VarrOpt, Iter)
                                            [function, total, symbol(appendIterOpt)]

  rule appendIterOpt(noVarr(), _:Iter) => noVarr()
  rule appendIterOpt(someVarr(varr(X:Id, Is:IterList)), I:Iter)
    => someVarr(varr(X, appendIter(Is, I)))

  syntax Vari ::= iterVari(Vari, Iter)      [function, total, symbol(iterVari)]

  rule iterVari(vari(X:Id, T:Typ, Is:IterList), I:Iter)
    => vari(X, T, appendIter(Is, I))

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

endmodule
