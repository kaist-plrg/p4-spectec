// Exercises out and inout parameter write-back via an action call.
// The action sets pass = true via an out parameter; the packet should forward.
#include <nano_model.p4>

action allow(out bool pass) {
    pass = true;
}

parser Parser(packet_in pkt, out Header hdr) {
    state start {
        pkt.extract(hdr.nanonet);
        transition accept;
    }
}

control Filter(inout Header hdr, out bool pass) {
    apply {
        allow(pass);
    }
}

NanoSwitch(Parser(), Filter()) main;
