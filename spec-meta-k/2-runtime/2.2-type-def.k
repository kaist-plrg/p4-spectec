// Runtime type definitions.
//
// Mirrors spec-meta/2-runtime/2.2-type-def.watsup.

requires "../1-syntax/1.0-syntax.k"

module TYPE-DEF
  imports SYNTAX

  // ==========================================================================
  // Runtime type definitions
  //
  //   syntax typDef = | typDefn | PARAM
  // ==========================================================================

  syntax TypDef ::= TypDefn
                  | paramTD()               [symbol(paramTD)]

endmodule
