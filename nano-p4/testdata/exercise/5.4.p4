// Evaluates a conditional statement where the condition is false,
// exercising the else branch of the interpreter.
#include <nano_model.p4>

action decide(out bool pass) {
    bool x = false;
    if (x) {
        pass = false;
    } else {
        pass = true;
    }
}

parser Parser(packet_in pkt, out Header hdr) {
    state start {
        transition accept;
    }
}

control Filter(inout Header hdr, out bool pass) {
    apply {
        decide(pass);
    }
}

NanoSwitch(Parser(), Filter()) main;
