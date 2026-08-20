// Configuration for AL evaluation.

requires "../common/0-stdlib.k"
requires "1-syntax.k"

module AL-CONFIG
  imports COMMON-STDLIB
  imports AL-SYNTAX
  imports MAP
  imports LIST

  syntax KItem ::= logDebug(Val)
                 // Starts the OCaml runtime behind the external interface.
                 // Seeded at the *head* of `<k>` below, ahead of `$PGM`, so
                 // that it necessarily runs before any rule that could reach a
                 // builtin; the C shim assumes this has happened and does not
                 // check.  Consumed by one rule in al/4.2-extern-ffi.k.
                 | "initFFI" "(" ")"
                 | "afterLoad" "(" ")"

  configuration
    <al>
      <k> initFFI() ~> logDebug(textV("entry-al")) ~> $PGM:Script ~> afterLoad() </k>
      // The program the spec in `<k>` is run on, or `noP4()` when the script is
      // self-contained.  `P4Opt` is declared in 1-syntax.k because krun parses
      // this cell's value; the two entries it selects are in 6-entry.k.
      <p4prog> $P4:P4Opt </p4prog>
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
      <log> .List </log>
      <result> .K </result>
      // `ml_init_c`'s return value, and hence a witness that the OCaml
      // runtime came up: `1` once `initFFI()` has reduced, `0` if it never
      // did.  Landing it here is also what stops K discarding the call as an
      // unused pure term -- see al/4.2-extern-ffi.k.
      <ffiinit> 0 </ffiinit>
    </al>

  syntax LocalSave ::= localSave(Map, Map, Map) [symbol(localSave)]
  syntax CallSave ::= callSave(LocalSave, Map) [symbol(callSave)]

endmodule
