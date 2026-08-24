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
      <p4prog> $P4:P4Opt </p4prog>
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
