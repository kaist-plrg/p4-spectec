#include <nano_model.p4>

struct Header {}

parser Parser(packet_in pkt, out Header hdr) {
    state start {
        transition accept;
    }
}

control Filter(inout Header hdr, out bool pass) {
    apply {
        bool x = true;
        pass = x != false;
    }
}

NanoSwitch<Header>(Parser<Header>(), Filter<Header>()) main;
