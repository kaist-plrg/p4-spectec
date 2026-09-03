// Standard library of the P4-SpecTec meta-language.
//
// Mirrors spec-meta/common/0-stdlib.watsup.
//
// Watsup's generic functions (`$ite<X>`, `$repeat_<X>`, ...) are parametric.
// K has no generics, so each is instantiated at the sorts it is used at.

requires "1-syntax.k"

module COMMON-STDLIB
  imports COMMON-SYNTAX
  imports BOOL
  imports INT
  imports STRING
  imports K-EQUAL   // `#if _ #then _ #else _ #fi`

  // ==========================================================================
  // General bool functions
  //
  //   dec $exists_(bool*) : bool
  //   dec $forall_(bool*) : bool
  // ==========================================================================

  syntax BoolList ::= List{Bool, ","}       [symbol(boolList), terminator-symbol(.boolList)]

  syntax Bool ::= exists(BoolList)          [function, total, symbol(exists)]
                | forall(BoolList)          [function, total, symbol(forall)]

  rule exists(.BoolList) => false
  rule exists(B:Bool, Bs:BoolList) => B orBool exists(Bs)

  rule forall(.BoolList) => true
  rule forall(B:Bool, Bs:BoolList) => B andBool forall(Bs)

  // ==========================================================================
  // General sequence functions, instantiated at `val`
  //
  //   dec $repeat_<X>(X, nat) : X*
  //   builtin dec $rev_<X>(X*) : X*
  //   builtin dec $transpose_<X>(X**) : X**
  //   builtin dec $assoc_<X, Y>(X, (X, Y)*) : Y?   at X = atom, Y = val
  // ==========================================================================

  // `val**`
  syntax ValListList ::= List{ValList, ";"} [symbol(valListList), terminator-symbol(.valListList)]

  syntax ValList ::= repeatVal(Val, Int)    [function, total, symbol(repeatVal)]
                   | revVal(ValList)        [function, total, symbol(revVal)]
                   | appendVal(ValList, ValList)
                                            [function, total, symbol(appendVal)]

  rule repeatVal(_:Val, N:Int) => .ValList requires N <=Int 0
  rule repeatVal(V:Val, N:Int) => V, repeatVal(V, N -Int 1) requires N >Int 0

  rule revVal(Vs:ValList) => revValAcc(Vs, .ValList)

  syntax ValList ::= revValAcc(ValList, ValList)
                                            [function, total, symbol(revValAcc)]
  rule revValAcc(.ValList, Acc:ValList) => Acc
  rule revValAcc((V:Val, Vs:ValList), Acc:ValList) => revValAcc(Vs, (V, Acc))

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

  // `$assoc_<atom, val>`: lookup in an association list of `atom`-keyed pairs.
  // The pair list is exactly `valfield*`, so it is taken as a `ValFieldList`.
  syntax ValOpt ::= assocVal(Atom, ValFieldList)
                                            [function, total, symbol(assocVal)]

  rule assocVal(_:Atom, .ValFieldList) => noVal()
  rule assocVal(A:Atom, valField(A':Atom, V:Val), Fs:ValFieldList)
    => #if A ==String A' #then someVal(V) #else assocVal(A, Fs) #fi

endmodule
