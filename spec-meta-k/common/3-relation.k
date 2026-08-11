// Result sorts representing backtracking in evaluation.
//
// Mirrors spec-meta/common/4-relation.watsup and spec-meta/al/4-relation.watsup.
//
//   syntax res<X> = OK X | FAIL
//
// K has no generics, so `res<X>` is instantiated once per `X` used by AL:
//
//   res<val>  -> ValRes    ($upcast, $downcast)
//   res<val*> -> ValsRes   ($upcast/$downcast on a list of operands)
//   unitres   -> UnitRes   (declared for completeness; AL uses no unit result)

requires "2-env.k"

module COMMON-RELATION
  imports COMMON-ENV

  syntax KItem ::= "FAIL"

  //   syntax unitres = OK | FAIL
  syntax UnitRes ::= okU()                  [symbol(okU)]
                   | failU()                [symbol(failU)]

  //   syntax valres = res<val>
  syntax ValRes ::= okV(Val)                [symbol(okV)]
                  | failV()                 [symbol(failV)]

  //   syntax valsres = res<val*>
  syntax ValsRes ::= okVs(ValList)          [symbol(okVs)]
                   | failVs()               [symbol(failVs)]

endmodule
