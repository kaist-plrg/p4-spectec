#include <nano_model.p4>

struct Header {}

action set_pass(out bool pass, bool val) {
    pass = val;
}

parser Parser(packet_in pkt, out Header hdr) {
    state start {
        transition accept;
    }
}

control Filter(inout Header hdr, out bool pass) {
    apply {
        set_pass(pass, true);
    }
}

NanoSwitch<Header>(Parser<Header>(), Filter<Header>()) main;
