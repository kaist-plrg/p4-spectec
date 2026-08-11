// Configuration for AL evaluation.

requires "../common/0-stdlib.k"
requires "1-syntax.k"

module AL-CONFIG
  imports COMMON-STDLIB
  imports AL-SYNTAX
  imports MAP
  imports LIST

  syntax KItem ::= logDebug(Val)
                 | "afterLoad" "(" ")"

  configuration
    <al>
      <k> logDebug(textV("entry-al")) ~> $PGM:Script ~> afterLoad() </k>
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
      <builtinreq> "" </builtinreq>
    </al>

  syntax LocalSave ::= localSave(Map, Map, Map)
                                            [symbol(localSave)]

endmodule
