// Standard library of the P4-SpecTec meta-language.
//
// Mirrors spec-meta/0-stdlib.watsup.

requires "1-syntax.k"

module STDLIB
  imports SYNTAX
  imports BOOL
  imports INT
  imports STRING
  imports K-EQUAL   // `#if _ #then _ #else _ #fi`

  // ==========================================================================
  // General sequence functions, instantiated at `val`
  //
  //   dec $repeat_<X>(X, nat) : X*
  //   builtin dec $transpose_<X>(X**) : X**
  //   builtin dec $assoc_<X, Y>(X, (X, Y)*) : Y?   at X = atom, Y = val
  // ==========================================================================

  // `val**`
  syntax ValListList ::= List{ValList, ";"} [symbol(valListList), terminator-symbol(.valListList)]

  syntax ValList ::= repeatVal(Val, Int)    [function, total, symbol(repeatVal)]
                   | appendVal(ValList, ValList)
                                            [function, total, symbol(appendVal)]

  rule repeatVal(_:Val, N:Int) => .ValList requires N <=Int 0
  rule repeatVal(V:Val, N:Int) => V, repeatVal(V, N -Int 1) requires N >Int 0

  rule appendVal(.ValList, Vs:ValList) => Vs
  rule appendVal((V:Val, Vs:ValList), Ws:ValList) => V, appendVal(Vs, Ws)

  syntax ValListList ::= transposeVal(ValListList)
                                            [function, symbol(transposeVal)]

  rule transposeVal(.ValListList) => .ValListList
  rule transposeVal(.ValList ; _:ValListList) => .ValListList
  rule transposeVal(Vss:ValListList)
    => headsOf(Vss) ; transposeVal(tailsOf(Vss))
    requires notBool isEmptyOuter(Vss) andBool notBool hasEmptyRow(Vss)

  syntax Bool ::= isEmptyOuter(ValListList) [function, total, symbol(isEmptyOuter)]
                | hasEmptyRow(ValListList)  [function, total, symbol(hasEmptyRow)]

  rule isEmptyOuter(.ValListList) => true
  rule isEmptyOuter(_:ValList ; _:ValListList) => false

  rule hasEmptyRow(.ValListList) => false
  rule hasEmptyRow(.ValList ; _:ValListList) => true
  rule hasEmptyRow((_:Val, _:ValList) ; Vss:ValListList) => hasEmptyRow(Vss)

  syntax ValList ::= headsOf(ValListList)   [function, symbol(headsOf)]
  syntax ValListList ::= tailsOf(ValListList)
                                            [function, symbol(tailsOf)]

  rule headsOf(.ValListList) => .ValList
  rule headsOf((V:Val, _:ValList) ; Vss:ValListList) => V, headsOf(Vss)

  rule tailsOf(.ValListList) => .ValListList
  rule tailsOf((_:Val, Vs:ValList) ; Vss:ValListList) => Vs ; tailsOf(Vss)

  syntax ValOpt ::= assocVal(Atom, ValFieldList)
                                            [function, total, symbol(assocVal)]

  rule assocVal(_:Atom, .ValFieldList) => noVal()
  rule assocVal(A:Atom, (valField(A, V:Val), _:ValFieldList)) => someVal(V)
  rule assocVal(A:Atom, (_:ValField, Fs:ValFieldList)) => assocVal(A, Fs) [owise]

endmodule
