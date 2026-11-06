// Runs that end in an error: a hook of a domain module that fails, a function
// without an equation for its argument, or a pattern whose function is stuck.
// Each program ends the run at its error, but for int2bytes, whose negative
// length does not fail, and the -ok programs, whose function has an equation.
module ERRORS
  imports INT
  imports STRING
  imports BYTES
  imports FLOAT
  imports LIST
  imports MAP
  imports SET
  imports RANGEMAP

  configuration <k> $PGM:Pgm </k>

  // A function with an equation for 0 only
  syntax Int ::= g(Int) [function]
  rule g(0) => 1

  // The initial term cannot be evaluated: 1 /Int 0.
  syntax Pgm ::= Int

  // The error comes after one step: 1 /Int 0 in the second configuration.
  syntax Pgm ::= later(Int) | step(Int)
  rule <k> later(I) => step(I) </k>
  rule <k> step(I) => 1 /Int I </k>

  // INT

  // t-division by zero.
  syntax Pgm ::= tdiv(Int)
  rule <k> tdiv(I) => 1 /Int I </k>

  // t-modulus by zero.
  syntax Pgm ::= tmod(Int)
  rule <k> tmod(I) => 1 %Int I </k>

  // Euclidean division by zero.
  syntax Pgm ::= ediv(Int)
  rule <k> ediv(I) => 1 divInt I </k>

  // Euclidean modulus by zero.
  syntax Pgm ::= emod(Int)
  rule <k> emod(I) => 1 modInt I </k>

  // A negative exponent.
  syntax Pgm ::= pow(Int)
  rule <k> pow(I) => 2 ^Int I </k>

  // A negative left shift.
  syntax Pgm ::= shl(Int)
  rule <k> shl(I) => 1 <<Int I </k>

  // A negative right shift.
  syntax Pgm ::= shr(Int)
  rule <k> shr(I) => 1 >>Int I </k>

  // The logarithm of zero.
  syntax Pgm ::= log2(Int)
  rule <k> log2(I) => log2Int(I) </k>

  // STRING

  // A character code above 255.
  syntax Pgm ::= chr(Int)
  rule <k> chr(I) => chrChar(I) </k>

  // A negative start for findString.
  syntax Pgm ::= find(Int)
  rule <k> find(I) => findString("abc", "b", I) </k>

  // A substring that ends before it starts.
  syntax Pgm ::= substr(Int)
  rule <k> substr(I) => substrString("abc", 2, I) </k>

  // A string that is not an integer.
  syntax Pgm ::= string2int(String)
  rule <k> string2int(S) => String2Int(S) </k>

  // A string that is not an integer in base 10.
  syntax Pgm ::= string2base(String)
  rule <k> string2base(S) => String2Base(S, 10) </k>

  // BYTES

  // A byte index out of range.
  syntax Pgm ::= bytesget(Int)
  rule <k> bytesget(I) => b"a" [ I ] </k>

  // A byte substring that ends before it starts.
  syntax Pgm ::= bytessubstr(Int)
  rule <k> bytessubstr(I) => substrBytes(b"abc", 2, I) </k>

  // A byte update with a value above 255.
  syntax Pgm ::= bytesupdate(Int)
  rule <k> bytesupdate(I) => b"a" [ 0 <- I ] </k>

  // A byte replacement past the end.
  syntax Pgm ::= replaceat(Int)
  rule <k> replaceat(I) => replaceAtBytes(b"ab", I, b"xyz") </k>

  // Padding with a value above 255.
  syntax Pgm ::= padright(Int)
  rule <k> padright(I) => padRightBytes(b"a", 3, I) </k>

  // memsetBytes with a value above 127.
  syntax Pgm ::= memset(Int)
  rule <k> memset(I) => memsetBytes(b"abc", 0, 2, I) </k>

  // Int2Bytes of a negative length, which does not fail.
  syntax Pgm ::= int2bytes(Int)
  rule <k> int2bytes(I) => Int2Bytes(I, 5, BE) </k>

  // FLOAT

  // Float2Int of NaN.
  syntax Pgm ::= float2int(Float)
  rule <k> float2int(F) => Float2Int(F) </k>

  // LIST

  // A list index out of range.
  syntax Pgm ::= listget(Int)
  rule <k> listget(I) => ListItem(1) [ I ] </k>

  // A list update out of range.
  syntax Pgm ::= listupdate(Int)
  rule <k> listupdate(I) => ListItem(1) [ I <- 2 ] </k>

  // A fill that runs past the end of the list.
  syntax Pgm ::= listfill(Int)
  rule <k> listfill(I) => fillList(ListItem(0) ListItem(0), I, 2, 9) </k>

  // RANGEMAP

  // Range maps with overlapping ranges joined.
  syntax Pgm ::= rangeconcat(Int)
  rule <k> rangeconcat(I) => ([ I, I +Int 2 ) r|-> 1) ([ I +Int 1, I +Int 3 ) r|-> 2) </k>

  // An empty range.
  syntax Pgm ::= rangeempty(Int)
  rule <k> rangeempty(I) => [ I, I ) r|-> 1 </k>

  // A list of negative length.
  syntax Pgm ::= listmake(Int)
  rule <k> listmake(I) => makeList(I, 0) </k>

  // MAP

  // A map lookup of a missing key.
  syntax Pgm ::= maplookup(Int)
  rule <k> maplookup(I) => (1 |-> 2) [ I ] </k>

  // A concatenation of maps with the same key.
  syntax Pgm ::= mapconcat(Int)
  rule <k> mapconcat(I) => (1 |-> 2) (I |-> 3) </k>

  // choice of the empty map.
  syntax Pgm ::= mapchoice(Map)
  rule <k> mapchoice(M) => choice(M) </k>

  // FUNCTIONS

  // A function with no equation for its argument, on a right-hand side.
  syntax Pgm ::= noequation(Int)
  rule <k> noequation(I) => g(I) </k>

  // A function with no equation for its argument, in the requires clause
  // of a rule: the run ends, and the owise rule is not tried.
  syntax Pgm ::= norequires(Int)
  rule <k> norequires(I) => 1 </k> requires g(I) ==Int 1
  rule <k> norequires(_) => 2 </k> [owise]

  // A function with no equation for its argument, in the requires clause
  // of an equation: the run ends, and the owise equation is not tried.
  syntax Pgm ::= norequireseq(Int)
  syntax Int ::= h(Int) [function]
  rule h(I) => 1 requires g(I) ==Int 1
  rule h(_) => 2 [owise]
  rule <k> norequireseq(I) => h(I) </k>

  // A function in a map key on a left-hand side, its variables bound:
  // matching evaluates it, and the run ends where it has no equation.
  syntax Pgm ::= mapkey(Int)
  syntax KItem ::= inMap(Int, Map)
  rule <k> mapkey(I) => inMap(I, 1 |-> 3) </k>
  rule <k> inMap(I, g(I) |-> V _:Map) => V </k>
  rule <k> inMap(_, _) => 0 </k> [owise]

  // A function in a set element on a left-hand side, likewise.
  syntax Pgm ::= setelement(Int)
  syntax KItem ::= inSet(Int, Set)
  rule <k> setelement(I) => inSet(I, SetItem(1)) </k>
  rule <k> inSet(I, SetItem(g(I)) _:Set) => 1 </k>
  rule <k> inSet(_, _) => 0 </k> [owise]

  // A function in a list index (L[I <- V]) on a left-hand side, likewise.
  syntax Pgm ::= listindex(Int)
  syntax KItem ::= inList(Int, List)
  rule <k> listindex(I) => inList(I, ListItem(5) ListItem(6)) </k>
  rule <k> inList(I, _ [ g(I) <- V ]) => V </k>
  rule <k> inList(_, _) => 0 </k> [owise]
endmodule
