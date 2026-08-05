// Type substitution.
//
// Mirrors spec-meta/common/5.0-eval-typ.watsup.

requires "1-syntax.k"

module COMMON-EVAL-TYP
  imports COMMON-SYNTAX
  imports MAP
  imports BOOL
  imports K-EQUAL

  syntax Typ ::= substTyp(Map, Typ)         [function, total, symbol(substTyp)]
               | substTypP(Map, Typ)        [function, total, symbol(substTypP)]

  syntax TypList ::= substTypsMap(Map, TypList)
                                            [function, total, symbol(substTypsMap)]

  rule substTyp(.Map, T:Typ) => T
  rule substTyp(Th:Map, T:Typ) => substTypP(Th, T) [owise]

  rule substTypP(_:Map, natT()) => natT()
  rule substTypP(_:Map, intT()) => intT()
  rule substTypP(_:Map, textT()) => textT()
  rule substTypP(_:Map, boolT()) => boolT()
  rule substTypP(_:Map, funcT()) => funcT()

  // `VAR id eps` bound in theta is replaced by its binding; otherwise the
  // arguments are substituted and the head kept.
  rule substTypP(Th:Map, varT(X:Id, .TypList)) => {Th[X]}:>Typ
    requires X in_keys(Th)
  rule substTypP(Th:Map, varT(X:Id, .TypList)) => varT(X, .TypList)
    requires notBool (X in_keys(Th))
  rule substTypP(Th:Map, varT(X:Id, Ts:TypList)) => varT(X, substTypsMap(Th, Ts))
    requires Ts =/=K .TypList

  rule substTypP(Th:Map, tupT(Ts:TypList)) => tupT(substTypsMap(Th, Ts))
  rule substTypP(Th:Map, iterT(T:Typ, I:Iter)) => iterT(substTypP(Th, T), I)

  rule substTypsMap(_:Map, .TypList) => .TypList
  rule substTypsMap(Th:Map, (T:Typ, Ts:TypList))
    => substTypP(Th, T), substTypsMap(Th, Ts)

endmodule
