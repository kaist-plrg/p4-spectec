// Machine integers (MINT), computed by the K runtime, at widths 8, 64, and
// 256, and the list hooks that take or give one (64 and 256 bits, the widths
// the LLVM backend supports for them). A 256-bit value above 2^64 goes back to
// the runtime, which reads such a literal wrong; the KORE interface gives it
// as Int2MInt of its value. Values come from the program, so that kompile does
// not fold them.
module MINT-OPS
  imports INT
  imports BOOL
  imports LIST
  imports MINT

  configuration <k> $PGM:Pgm </k> <out> .List </out>

  // the widths the rules cast to, which K parses only once declared
  syntax MInt{8}
  syntax MInt{64}

  syntax Pgm ::= arith(Int) | wide(Int) | lists(Int)

  // wrapping at 8 bits, signed and unsigned readings, a comparison
  rule <k> arith(I) => .K </k>
       <out> _ => ListItem(MInt2Unsigned(Int2MInt(I)::MInt{8} +MInt Int2MInt(250)::MInt{8}))
                  ListItem(MInt2Signed(--MInt Int2MInt(I)::MInt{64}))
                  ListItem(Int2MInt(I)::MInt{64} <uMInt Int2MInt(I +Int 1)::MInt{64}) </out>

  // a product above 2^64, then the product again with 3
  syntax MInt{256} ::= big(Int) [function]
  rule big(I) => Int2MInt(I)::MInt{256} *MInt Int2MInt(2 ^Int 100)::MInt{256}
  rule <k> wide(I) => .K </k>
       <out> _ => ListItem(MInt2Unsigned(big(I) *MInt Int2MInt(3)::MInt{256})) </out>

  // get, update, and size with machine-integer indices
  rule <k> lists(I) => .K </k>
       <out> _ => ListItem((ListItem(I) ListItem(I +Int 1)) [ Int2MInt(1)::MInt{64} ])
                  ListItem((ListItem(I) ListItem(I +Int 1)) [ Int2MInt(0)::MInt{256} ])
                  ListItem((ListItem(I) ListItem(I +Int 1)) [ Int2MInt(1)::MInt{64} <- 9 ])
                  ListItem(MInt2Unsigned(size(ListItem(I) ListItem(I))::MInt{64}))
                  ListItem(MInt2Unsigned(size(ListItem(I))::MInt{256})) </out>
endmodule
