// Configuration for AL evaluation.

requires "../common/0-stdlib.k"
requires "1-syntax.k"

module AL-CONFIG
  imports COMMON-STDLIB
  imports AL-SYNTAX
  imports MAP
  imports LIST

  syntax KItem ::= logDebug(Val)
                 | "initFFI" "(" ")"
                 | "afterLoad" "(" ")"

  configuration
    <al>
      <k> initFFI() ~> logDebug(textV("entry-al")) ~> $PGM:Script ~> afterLoad() </k>
      // The program the spec in `<k>` is run on, or `noP4()` when the script is
      // self-contained.  `P4Opt` is declared in 1-syntax.k because krun parses
      // this cell's value; the two entries it selects are in 6-entry.k.
      <p4prog> $P4:P4Opt </p4prog>
      // The *target spec*: the path of the SpecTec program `<k>` is running.
      // A builtin or extern call is answered against it by the host
      // (al/4.2-extern-ffi.k), so the host has to be told which spec that is,
      // and only the invocation knows -- `make k-run` and `make k-typecheck`
      // each name their own target.  It arrives as a third krun variable,
      // alongside `$PGM` and `$P4`.
      //
      // `String` needs no `-p` wrapper: krun parses `-cSPEC='"spec"'` directly,
      // quotes included.  Like `$P4` it has no default (K gives configuration
      // variables none), so every krun line must pass it.
      //
      // Relative to the repo root, which is why K runs must start there.
      <specdir> $SPEC:String </specdir>
      <global>
        <gtdenv> .Map </gtdenv>
        <grenv> .Map </grenv>
        <gfenv> .Map </gfenv>
      </global>
      <local>
        <ltdenv> .Map </ltdenv>
        <lfenv> .Map </lfenv>
        <lvenv> .Map </lvenv>
      </local>
      <caller>
        <cfenv> .Map </cfenv>
      </caller>
      <saves> .List </saves>
      <callstack> .List </callstack>
    </al>

  syntax LocalSave ::= localSave(Map, Map, Map) [symbol(localSave)]
  syntax CallSave ::= callSave(LocalSave, Map) [symbol(callSave)]

endmodule
