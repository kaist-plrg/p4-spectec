// Standard library of the P4-SpecTec meta-language.
//
// Mirrors spec-meta/0-aux/0.0-stdlib.watsup.

requires "../1-syntax/1.0-syntax.k"

module STDLIB
  imports SYNTAX
  imports BOOL
  imports INT
  imports STRING
  imports BYTES
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

  syntax Int ::= lengthVals(ValList)        [function, total, symbol(lengthVals)]
  rule lengthVals(.ValList) => 0
  rule lengthVals(_:Val, Vs:ValList) => 1 +Int lengthVals(Vs)

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

  // ==========================================================================
  // Text by characters
  //
  // A K string holds the UTF-8 bytes of a text, but a text counts its
  // characters: the bytes that do not continue one (`10xxxxxx`).
  // ==========================================================================

  syntax Int ::= lengthText(String)         [function, total, symbol(lengthText)]
  rule lengthText(S:String) => countChars(String2Bytes(S), 0, 0)

  // The characters from byte `I` on, after `N` of them.
  syntax Int ::= countChars(Bytes, Int, Int)
                                            [function, total, symbol(countChars)]
  rule countChars(B:Bytes, I:Int, N:Int) => N requires I >=Int lengthBytes(B)
  rule countChars(B:Bytes, I:Int, N:Int) => countChars(B, I +Int 1, N)
    requires I <Int lengthBytes(B) andBool continuesChar(B[I])
  rule countChars(B:Bytes, I:Int, N:Int) => countChars(B, I +Int 1, N +Int 1) [owise]

  // Characters `I` up to `J`.
  syntax String ::= substrText(String, Int, Int)
                                            [function, total, symbol(substrText)]
  rule substrText(S:String, I:Int, J:Int)
    => substrString(S, charOffset(String2Bytes(S), 0, I), charOffset(String2Bytes(S), 0, J))

  // The byte offset of the character `C` characters on from byte `I`.
  syntax Int ::= charOffset(Bytes, Int, Int)
                                            [function, total, symbol(charOffset)]
  rule charOffset(B:Bytes, I:Int, _:Int) => I requires I >=Int lengthBytes(B)
  rule charOffset(B:Bytes, I:Int, C:Int) => charOffset(B, I +Int 1, C)
    requires I <Int lengthBytes(B) andBool continuesChar(B[I])
  rule charOffset(_:Bytes, I:Int, C:Int) => I requires C <=Int 0 [owise]
  rule charOffset(B:Bytes, I:Int, C:Int) => charOffset(B, I +Int 1, C -Int 1)
    requires C >Int 0 [owise]

  syntax Bool ::= continuesChar(Int)        [function, total, symbol(continuesChar)]
  rule continuesChar(X:Int) => X &Int 192 ==Int 128

endmodule
