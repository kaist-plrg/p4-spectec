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
  syntax TArgList ::= substTargsMap(Map, TArgList)
                                                [function, total, symbol(substTargsMap)]
                   | targsOfTyps(TypList)       [function, total, symbol(targsOfTyps)]
  syntax TypList ::= typsOfTargs(TArgList)      [function, total, symbol(typsOfTargs)]

  rule substTyp(.Map, T:Typ) => T
  rule substTyp(Th:Map, T:Typ) => substTypInner(Th, T) [owise]

  rule substTypInner(_:Map, natT()) => natT()
  rule substTypInner(_:Map, intT()) => intT()
  rule substTypInner(_:Map, textT()) => textT()
  rule substTypInner(_:Map, boolT()) => boolT()
  rule substTypInner(_:Map, funcT()) => funcT()

  rule substTypInner(Th:Map, varT(X:Id, .TArgList)) => {Th[X]}:>Typ
    requires X in_keys(Th)
  rule substTypInner(Th:Map, varT(X:Id, .TArgList)) => varT(X, .TArgList)
    requires notBool (X in_keys(Th))
  rule substTypInner(Th:Map, varT(X:Id, Ts:TArgList)) => varT(X, substTargsMap(Th, Ts))
    requires Ts =/=K .TArgList

  rule substTypInner(Th:Map, tupT(Ts:TypList)) => tupT(substTypsMap(Th, Ts))
  rule substTypInner(Th:Map, iterT(T:Typ, I:Iter)) => iterT(substTypInner(Th, T), I)

  rule substTypsMap(_:Map, .TypList) => .TypList
  rule substTypsMap(Th:Map, (T:Typ, Ts:TypList))
    => substTypInner(Th, T), substTypsMap(Th, Ts)

  rule substTargsMap(_:Map, .TArgList) => .TArgList
  rule substTargsMap(Th:Map, (T:TArg, Ts:TArgList))
    => substTypInner(Th, {T}:>Typ), substTargsMap(Th, Ts)

  rule typsOfTargs(.TArgList) => .TypList
  rule typsOfTargs((T:TArg, Ts:TArgList)) => {T}:>Typ, typsOfTargs(Ts)

  rule targsOfTyps(.TypList) => .TArgList
  rule targsOfTyps((T:Typ, Ts:TypList)) => T, targsOfTyps(Ts)

endmodule
