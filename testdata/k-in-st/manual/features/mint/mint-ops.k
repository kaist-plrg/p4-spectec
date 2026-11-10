// Machine integers (MINT), computed by the K runtime, at widths 8, 64, and
// 256, and the list hooks that take or give one (64 and 256 bits, the widths
// the LLVM backend supports for them). No value wider than 64 bits goes back
// to the runtime: it misreads such a literal (1p256 as 2^192), and such a run
// is out of scope; the 256-bit size is checked as it is printed. Values come
// from the program, so that kompile does not fold them.
module MINT-OPS
  imports INT
  imports BOOL
  imports LIST
  imports MINT
  imports K-EQUAL

  configuration <k> $PGM:Pgm </k> <out> .List </out>

  // the widths the rules cast to, which K parses only once declared
  syntax MInt{8}
  syntax MInt{64}
  syntax MInt{256}

  syntax Pgm ::= arith(Int) | lists(Int) | literals(Int)

  // wrapping at 8 bits, signed and unsigned readings, a comparison
  rule <k> arith(I) => .K </k>
       <out> _ => ListItem(MInt2Unsigned(Int2MInt(I)::MInt{8} +MInt Int2MInt(250)::MInt{8}))
                  ListItem(MInt2Signed(--MInt Int2MInt(I)::MInt{64}))
                  ListItem(Int2MInt(I)::MInt{64} <uMInt Int2MInt(I +Int 1)::MInt{64}) </out>

  // get, update, and size with machine-integer indices
  rule <k> lists(I) => .K </k>
       <out> _ => ListItem((ListItem(I) ListItem(I +Int 1)) [ Int2MInt(1)::MInt{64} ])
                  ListItem((ListItem(I) ListItem(I +Int 1)) [ Int2MInt(0)::MInt{256} ])
                  ListItem((ListItem(I) ListItem(I +Int 1)) [ Int2MInt(1)::MInt{64} <- 9 ])
                  ListItem(MInt2Unsigned(size(ListItem(I) ListItem(I))::MInt{64}))
                  ListItem(size(ListItem(I))::MInt{256}) </out>

  // literals written with a sign, a capital P, or a value past the width are
  // the value the runtime computes: matched, compared, and given to a hook
  syntax KItem ::= chk(MInt{8}, Int)
  rule <k> literals(I) => chk(Int2MInt(I)::MInt{8}, I) </k>
  rule <k> chk(-1p8, I) => .K </k>
       <out> _ => ListItem(Int2MInt(I -Int 250)::MInt{8} ==K 5P8)
                  ListItem(Int2MInt(I -Int 250)::MInt{8} ==K +5p8)
                  ListItem(Int2MInt(I -Int 211)::MInt{8} ==K 300p8)
                  ListItem(MInt2Unsigned(-1p8)) </out>
endmodule
