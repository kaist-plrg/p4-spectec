// Configuration for AL evaluation.

requires "0.0-stdlib.k"
requires "../1-syntax/1.0-syntax.k"

module CONFIG
  imports STDLIB
  imports SYNTAX
  imports MAP
  imports LIST

  syntax KItem ::= logDebug(Val)
                 | serve(JSON)

  // `<k>` serves the host's calls from the start (3.14-entry.k). `<host>`
  // holds the addresses of the host functions (3.04.2-extern-host.k), which
  // the host passes as configuration variables.
  configuration
    <al>
      <k> serve({ "ready" : null, .JSONs }) </k>
      <host>
        <hbuffer> $HBUFFER:Int </hbuffer>
        <heval> $HEVAL:Int </heval>
        <hserve> $HSERVE:Int </hserve>
        <hstrlen> $HSTRLEN:Int </hstrlen>
      </host>
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
      <callstack> .List </callstack>
    </al>

endmodule
