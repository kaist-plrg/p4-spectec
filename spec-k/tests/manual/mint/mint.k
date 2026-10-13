// Machine-integer operations that the regression tests of K do not use.
// Values come from the program, so that kompile does not fold them.
module MINT-TEST
  imports INT
  imports BOOL
  imports LIST
  imports MINT

  syntax MInt{8}
  syntax MInt{16}

  configuration <k> $PGM:Pgm </k>

  syntax Pgm ::= unsigned(Int, Int) | minmax(Int, Int) | widths(Int, Int) | literal(Int)

  syntax KItem ::= #unsigned(MInt{8}, MInt{8}) | #minmax(MInt{8}, MInt{8}) | #widths(MInt{8}, MInt{16})

  rule <k> unsigned(I, J) => #unsigned(Int2MInt(I)::MInt{8}, Int2MInt(J)::MInt{8}) </k>
  rule <k> #unsigned(A, B) => ListItem(MInt2Unsigned(A /uMInt B)) ListItem(MInt2Unsigned(A %uMInt B))
                              ListItem(MInt2Unsigned(A >>lMInt B))
                              ListItem(A <uMInt B) ListItem(A <=uMInt B) ListItem(A >uMInt B) ListItem(A >=uMInt B) </k>

  rule <k> minmax(I, J) => #minmax(Int2MInt(I)::MInt{8}, Int2MInt(J)::MInt{8}) </k>
  rule <k> #minmax(A, B) => ListItem(MInt2Unsigned(uMinMInt(A, B))) ListItem(MInt2Unsigned(uMaxMInt(A, B)))
                            ListItem(MInt2Signed(sMinMInt(A, B))) ListItem(MInt2Signed(sMaxMInt(A, B))) </k>

  rule <k> widths(I, J) => #widths(Int2MInt(I)::MInt{8}, Int2MInt(J)::MInt{16}) </k>
  rule <k> #widths(A, B) => ListItem(MInt2Unsigned(roundMInt(A)::MInt{16}))
                            ListItem(MInt2Unsigned(signExtendMInt(A)::MInt{16}))
                            ListItem(MInt2Unsigned(roundMInt(B)::MInt{8}))
                            ListItem(MInt2Signed(signExtendMInt(B)::MInt{8})) </k>

  syntax Int ::= f(MInt{8}) [function]
  rule f(5p8) => 1
  rule f(_) => 0 [owise]
  rule <k> literal(I) => ListItem(f(Int2MInt(I))) ListItem(f(Int2MInt(I +Int 1))) </k>
endmodule
