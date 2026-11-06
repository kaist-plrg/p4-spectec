// A step bound (krun --depth): a run stops after the bound, or where no rule
// applies if that comes first. Each program counts down from its number;
// <name>.depth bounds its runs.
module DEPTH
  imports INT

  configuration <k> $PGM:Int </k>

  rule <k> N:Int => N -Int 1 </k> requires N >Int 0
endmodule
