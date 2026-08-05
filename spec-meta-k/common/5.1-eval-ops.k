// Operator semantics of the P4-SpecTec meta-language.
//
// Mirrors spec-meta/common/5.1-eval-ops.watsup.

requires "1-syntax.k"
requires "0-stdlib.k"

module COMMON-EVAL-OPS
  imports COMMON-STDLIB

  // ==========================================================================
  // Unary operations
  //
  //   dec $unop_number(numunop, num) : val
  // ==========================================================================

  syntax Val ::= unopNumber(NumUnOp, Num)   [function, symbol(unopNumber)]

  rule unopNumber(plusOp(), natN(N:Int)) => natN(N)
  rule unopNumber(plusOp(), intN(I:Int)) => intN(I)
  rule unopNumber(minusOp(), natN(N:Int)) => intN(0 -Int N)
  rule unopNumber(minusOp(), intN(I:Int)) => intN(0 -Int I)

  // ==========================================================================
  // Binary operations
  //
  //   dec $binop_bool(boolbinop, bool, bool) : bool
  //   dec $binop_number(numbinop, num, num) : val
  // ==========================================================================

  syntax Bool ::= binopBool(BoolBinOp, Bool, Bool)
                                            [function, total, symbol(binopBool)]

  rule binopBool(andOp(), B:Bool, B':Bool) => B andBool B'
  rule binopBool(orOp(), B:Bool, B':Bool) => B orBool B'
  rule binopBool(implOp(), B:Bool, B':Bool) => B impliesBool B'
  rule binopBool(equivOp(), B:Bool, B':Bool) => B ==Bool B'

  syntax Val ::= binopNumber(NumBinOp, Num, Num)
                                            [function, symbol(binopNumber)]

  rule binopNumber(addOp(), natN(L:Int), natN(R:Int)) => natN(L +Int R)
  rule binopNumber(addOp(), intN(L:Int), intN(R:Int)) => intN(L +Int R)
  // `NAT - NAT` is an `INT`, as in watsup.
  rule binopNumber(subOp(), natN(L:Int), natN(R:Int)) => intN(L -Int R)
  rule binopNumber(subOp(), intN(L:Int), intN(R:Int)) => intN(L -Int R)
  rule binopNumber(mulOp(), natN(L:Int), natN(R:Int)) => natN(L *Int R)
  rule binopNumber(mulOp(), intN(L:Int), intN(R:Int)) => intN(L *Int R)
  rule binopNumber(divOp(), natN(L:Int), natN(R:Int)) => natN(L /Int R)
    requires R =/=Int 0
  rule binopNumber(divOp(), intN(L:Int), intN(R:Int)) => intN(L /Int R)
    requires R =/=Int 0
  rule binopNumber(modOp(), natN(L:Int), natN(R:Int)) => natN(L %Int R)
    requires R =/=Int 0
  rule binopNumber(modOp(), intN(L:Int), intN(R:Int)) => intN(L %Int R)
    requires R =/=Int 0
  rule binopNumber(powOp(), natN(L:Int), natN(R:Int)) => natN(L ^Int R)
    requires R >=Int 0
  rule binopNumber(powOp(), intN(L:Int), intN(R:Int)) => intN(L ^Int R)
    requires R >=Int 0

  // ==========================================================================
  // Comparison operations
  //
  //   dec $cmpop_poly(polycmpop, val, val) : bool
  //   dec $cmpop_number(numcmpop, num, num) : bool
  // ==========================================================================

  syntax Bool ::= cmpopPoly(PolyCmpOp, Val, Val)
                                            [function, total, symbol(cmpopPoly)]

  // Structural equality on values.  `==K` compares the underlying terms, which
  // for constructor applications over builtins is exactly watsup's `=`.
  rule cmpopPoly(eqOp(), L:Val, R:Val) => L ==K R
  rule cmpopPoly(neOp(), L:Val, R:Val) => L =/=K R

  syntax Bool ::= cmpopNumber(NumCmpOp, Num, Num)
                                            [function, symbol(cmpopNumber)]

  rule cmpopNumber(ltOp(), natN(L:Int), natN(R:Int)) => L <Int R
  rule cmpopNumber(ltOp(), intN(L:Int), intN(R:Int)) => L <Int R
  rule cmpopNumber(gtOp(), natN(L:Int), natN(R:Int)) => L >Int R
  rule cmpopNumber(gtOp(), intN(L:Int), intN(R:Int)) => L >Int R
  rule cmpopNumber(leOp(), natN(L:Int), natN(R:Int)) => L <=Int R
  rule cmpopNumber(leOp(), intN(L:Int), intN(R:Int)) => L <=Int R
  rule cmpopNumber(geOp(), natN(L:Int), natN(R:Int)) => L >=Int R
  rule cmpopNumber(geOp(), intN(L:Int), intN(R:Int)) => L >=Int R

  // ==========================================================================
  // Constructor checks
  //
  //   dec $is_tup(val) : bool
  //   dec $is_fun(val) : bool
  // ==========================================================================

  syntax Bool ::= isTup(Val)                [function, total, symbol(isTup)]
                | isFun(Val)                [function, total, symbol(isFun)]
                | isNumV(Val)                [function, total, symbol(isNumV)]

  rule isTup(tupV(_:ValList)) => true
  rule isTup(_:Val) => false [owise]

  rule isFun(funcV(_:Id)) => true
  rule isFun(_:Val) => false [owise]

  // `val <: num`, used by `$upcast`/`$downcast` side conditions.
  rule isNumV(_:Num) => true
  rule isNumV(_:Val) => false [owise]

endmodule
