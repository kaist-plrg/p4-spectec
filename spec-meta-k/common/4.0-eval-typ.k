// Type substitution.
//
// Mirrors spec-meta/common/5.0-eval-typ.watsup.

requires "1-syntax.k"

module COMMON-EVAL-TYP
  imports COMMON-SYNTAX
  imports MAP
  imports BOOL
  imports K-EQUAL

  syntax Typ ::= substTyp(Map, Typ)             [function, total, symbol(substTyp)]
               | substTypInner(Map, Typ)        [function, total, symbol(substTypInner)]

  syntax TypList ::= substTypsMap(Map, TypList) [function, total, symbol(substTypsMap)]

  rule substTyp(.Map, T:Typ) => T
  rule substTyp(Th:Map, T:Typ) => substTypInner(Th, T) [owise]

  rule substTypInner(_:Map, natT()) => natT()
  rule substTypInner(_:Map, intT()) => intT()
  rule substTypInner(_:Map, textT()) => textT()
  rule substTypInner(_:Map, boolT()) => boolT()
  rule substTypInner(_:Map, funcT()) => funcT()

  // `VAR id eps` bound in theta is replaced by its binding; otherwise the
  // arguments are substituted and the head kept.
  rule substTypInner(Th:Map, varT(X:Id, .TypList)) => {Th[X]}:>Typ
    requires X in_keys(Th)
  rule substTypInner(Th:Map, varT(X:Id, .TypList)) => varT(X, .TypList)
    requires notBool (X in_keys(Th))
  rule substTypInner(Th:Map, varT(X:Id, Ts:TypList)) => varT(X, substTypsMap(Th, Ts))
    requires Ts =/=K .TypList

  rule substTypInner(Th:Map, tupT(Ts:TypList)) => tupT(substTypsMap(Th, Ts))
  rule substTypInner(Th:Map, iterT(T:Typ, I:Iter)) => iterT(substTypInner(Th, T), I)

  rule substTypsMap(_:Map, .TypList) => .TypList
  rule substTypsMap(Th:Map, (T:Typ, Ts:TypList))
    => substTypInner(Th, T), substTypsMap(Th, Ts)

endmodule
