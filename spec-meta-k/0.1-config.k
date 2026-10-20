// Configuration for AL evaluation.

requires "0.0-stdlib.k"
requires "1-syntax.k"

module CONFIG
  imports STDLIB
  imports SYNTAX
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
      <callstack> .List </callstack>
    </al>

endmodule
