// Calls an action with an out Nanonet parameter, then reads back a field.
#include <nano_model.p4>

action getDefault(out Nanonet n) {
}

parser Parser(packet_in pkt, out Header hdr) {
    state start {
        transition accept;
    }
}

control Filter(inout Header hdr, out bool pass) {
    apply {
        Nanonet n = hdr.nanonet;
        getDefault(n);
        pass = !n.drop;
    }
}

NanoSwitch(Parser(), Filter()) main;
