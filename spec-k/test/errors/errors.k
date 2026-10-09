// Programs on which krun ends with an error: a hook given an invalid argument,
// a function with no equation for its arguments (also in a requires clause,
// where the rules after it are not tried), or the initial term. The K
// spec must fail at the same step. Arguments come from the program, so that
// kompile does not fold them.
module ERRORS
  imports INT
  imports STRING
  imports LIST
  imports MAP
  imports BYTES
  imports FLOAT
  imports RANGEMAP

  configuration <k> $PGM:Pgm </k>

  syntax Pgm ::= Int
               | later(Int) | step(Int)
               | tdiv(Int) | tmod(Int) | ediv(Int) | emod(Int) | pow(Int) | shl(Int) | shr(Int) | log2(Int)
               | substr(Int) | chr(Int) | string2int(String) | string2base(String) | find(Int)
               | listget(Int) | listupdate(Int) | listmake(Int)
               | maplookup(Int) | mapconcat(Int) | mapchoice(Map)
               | bytesget(Int) | bytesupdate(Int) | bytessubstr(Int) | replaceat(Int) | padright(Int)
               | memset(Int) | int2bytes(Int)
               | rangelookup(Int) | rangeempty(Int) | rangeconcat(Int) | rangefind(Int) | rangechoice(RangeMap)
               | float2int(Float) | int2float(Int)
               | noequation(Int) | norequires(Int) | norequireseq(Int)

  // the error comes after one step
  rule <k> later(I) => step(I) </k>
  rule <k> step(I) => 1 /Int I </k>

  rule <k> tdiv(I) => 1 /Int I </k>
  rule <k> tmod(I) => 1 %Int I </k>
  rule <k> ediv(I) => 1 divInt I </k>
  rule <k> emod(I) => 1 modInt I </k>
  rule <k> pow(I) => 2 ^Int I </k>
  rule <k> shl(I) => 1 <<Int I </k>
  rule <k> shr(I) => 1 >>Int I </k>
  rule <k> log2(I) => log2Int(I) </k>

  rule <k> substr(I) => substrString("abc", 2, I) </k>
  rule <k> chr(I) => chrChar(I) </k>
  rule <k> string2int(S) => String2Int(S) </k>
  rule <k> string2base(S) => String2Base(S, 10) </k>
  rule <k> find(I) => findString("abc", "b", I) </k>

  rule <k> listget(I) => ListItem(1) [ I ] </k>
  rule <k> listupdate(I) => ListItem(1) [ I <- 2 ] </k>
  rule <k> listmake(I) => makeList(I, 0) </k>

  rule <k> maplookup(I) => (1 |-> 2) [ I ] </k>
  rule <k> mapconcat(I) => (1 |-> 2) (I |-> 3) </k>
  rule <k> mapchoice(M) => choice(M) </k>

  rule <k> bytesget(I) => b"a" [ I ] </k>
  rule <k> bytesupdate(I) => b"a" [ 0 <- I ] </k>
  rule <k> bytessubstr(I) => substrBytes(b"abc", 2, I) </k>
  rule <k> replaceat(I) => replaceAtBytes(b"ab", I, b"xyz") </k>
  rule <k> padright(I) => padRightBytes(b"a", 3, I) </k>
  rule <k> memset(I) => memsetBytes(b"abc", 0, 2, I) </k>
  rule <k> int2bytes(I) => Int2Bytes(I, 5, BE) </k>

  rule <k> rangelookup(I) => ([ 0, 2 ) r|-> 5) [ I ] </k>
  rule <k> rangeempty(I) => ([ I, 3 ) r|-> 5) </k>
  rule <k> rangeconcat(I) => ([ 0, 2 ) r|-> 1) ([ I, 3 ) r|-> 2) </k>
  rule <k> rangefind(I) => find_range(([ 0, 2 ) r|-> 5), I) </k>
  rule <k> rangechoice(M) => choice(M) </k>

  rule <k> float2int(F) => Float2Int(F) </k>
  rule <k> int2float(I) => Int2Float(1, I, 8) </k>

  syntax Int ::= g(Int) [function]
  rule g(0) => 1
  rule <k> noequation(I) => g(I) </k>

  rule <k> norequires(I) => 1 </k> requires g(I) ==Int 1
  rule <k> norequires(_) => 2 </k> [owise]

  syntax Int ::= h(Int) [function]
  rule h(I) => 1 requires g(I) ==Int 1
  rule h(_) => 2 [owise]
  rule <k> norequireseq(I) => h(I) </k>
endmodule
