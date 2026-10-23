// Operator semantics of the P4-SpecTec meta-language.
//
// Mirrors spec-meta/3-execution/3.04-eval-ops.watsup.

requires "3.03-relation.k"

module EVAL-OPS
  imports RELATION

  // ==========================================================================
  // Unary numeric operations
  //
  //   dec $numUnop(numUnop, num) : val
  // ==========================================================================

  syntax Val ::= numUnop(NumUnOp, Num)      [function, total, symbol(numUnop)]

  rule numUnop(plusOp(), natN(N:Int)) => natN(N)
  rule numUnop(plusOp(), intN(I:Int)) => intN(I)
  rule numUnop(minusOp(), natN(N:Int)) => intN(0 -Int N)
  rule numUnop(minusOp(), intN(I:Int)) => intN(0 -Int I)

  // ==========================================================================
  // Binary boolean operations
  //
  //   dec $boolBinop(boolBinop, bool, bool) : bool
  // ==========================================================================

  syntax Bool ::= boolBinop(BoolBinOp, Bool, Bool)
                                            [function, total, symbol(boolBinop)]

  rule boolBinop(andOp(), B:Bool, B':Bool) => B andBool B'
  rule boolBinop(orOp(), B:Bool, B':Bool) => B orBool B'
  rule boolBinop(implOp(), B:Bool, B':Bool) => B impliesBool B'
  rule boolBinop(equivOp(), B:Bool, B':Bool) => B ==Bool B'

  // ==========================================================================
  // Binary numeric operations
  //
  //   syntax numBinopRes = ok<val> | error
  //   dec $numBinop(numBinop, num, num) : numBinopRes
  //
  // A nat/int mix, a division by zero, and `POW` (no clause in the spec) are
  // errors.
  // ==========================================================================

  syntax Res ::= numBinop(NumBinOp, Num, Num)
                                            [function, total, symbol(numBinop)]

  rule numBinop(addOp(), natN(L:Int), natN(R:Int)) => ok(natN(L +Int R))
  rule numBinop(addOp(), intN(L:Int), intN(R:Int)) => ok(intN(L +Int R))
  // `NAT - NAT` is an `INT`.
  rule numBinop(subOp(), natN(L:Int), natN(R:Int)) => ok(intN(L -Int R))
  rule numBinop(subOp(), intN(L:Int), intN(R:Int)) => ok(intN(L -Int R))
  rule numBinop(mulOp(), natN(L:Int), natN(R:Int)) => ok(natN(L *Int R))
  rule numBinop(mulOp(), intN(L:Int), intN(R:Int)) => ok(intN(L *Int R))
  rule numBinop(divOp(), natN(L:Int), natN(R:Int)) => ok(natN(L /Int R))
    requires R =/=Int 0
  rule numBinop(divOp(), intN(L:Int), intN(R:Int)) => ok(intN(L /Int R))
    requires R =/=Int 0
  rule numBinop(modOp(), natN(L:Int), natN(R:Int)) => ok(natN(L %Int R))
    requires R =/=Int 0
  rule numBinop(modOp(), intN(L:Int), intN(R:Int)) => ok(intN(L %Int R))
    requires R =/=Int 0
  rule numBinop(_:NumBinOp, _:Num, _:Num) => ERROR [owise]

  // ==========================================================================
  // Polymorphic comparison operations
  //
  //   dec $polyCmpop(polyCmpop, val, val) : bool
  // ==========================================================================

  syntax Bool ::= polyCmpop(PolyCmpOp, Val, Val)
                                            [function, total, symbol(polyCmpop)]

  rule polyCmpop(eqOp(), L:Val, R:Val) => L ==K R
  rule polyCmpop(neOp(), L:Val, R:Val) => L =/=K R

  // ==========================================================================
  // Numeric comparison operations
  //
  //   syntax numCmpRes = ok<bool> | error
  //   dec $numCmpop(numCmpop, num, num) : numCmpRes
  // ==========================================================================

  syntax Res ::= numCmpop(NumCmpOp, Num, Num)
                                            [function, total, symbol(numCmpop)]

  rule numCmpop(ltOp(), natN(L:Int), natN(R:Int)) => ok(L <Int R)
  rule numCmpop(ltOp(), intN(L:Int), intN(R:Int)) => ok(L <Int R)
  rule numCmpop(gtOp(), natN(L:Int), natN(R:Int)) => ok(L >Int R)
  rule numCmpop(gtOp(), intN(L:Int), intN(R:Int)) => ok(L >Int R)
  rule numCmpop(leOp(), natN(L:Int), natN(R:Int)) => ok(L <=Int R)
  rule numCmpop(leOp(), intN(L:Int), intN(R:Int)) => ok(L <=Int R)
  rule numCmpop(geOp(), natN(L:Int), natN(R:Int)) => ok(L >=Int R)
  rule numCmpop(geOp(), intN(L:Int), intN(R:Int)) => ok(L >=Int R)
  rule numCmpop(_:NumCmpOp, _:Num, _:Num) => ERROR [owise]

  // ==========================================================================
  // Pattern tests
  //
  //   dec $matches(val, pat) : bool
  // ==========================================================================

  syntax Bool ::= matches(Val, Pat)         [function, total, symbol(matches)]

  rule matches(injV(valCase(M:MixOp, _:ValList)), injPat(M':MixOp)) => M ==K M'
  rule matches(listV(Vs:ValList), consPat()) => lengthVals(Vs) >Int 0
  rule matches(listV(Vs:ValList), fixedPat(N:Int)) => lengthVals(Vs) ==Int N
  rule matches(listV(Vs:ValList), nilPat()) => lengthVals(Vs) ==Int 0
  rule matches(optV(someVal(_:Val)), somePat()) => true
  rule matches(optV(noVal()), somePat()) => false
  rule matches(optV(someVal(_:Val)), nonePat()) => false
  rule matches(optV(noVal()), nonePat()) => true
  rule matches(_:Val, _:Pat) => false [owise]

endmodule
