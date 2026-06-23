#include <nano_model.p4>

struct Header {}

parser Parser(packet_in pkt, out Header hdr) {
    state start {
        transition accept;
    }
}

control Filter(inout Header hdr, out bool pass) {
    const bit<8> THRESHOLD = 8w128;
    apply {
        bit<8> val = 8w64;
        pass = val < THRESHOLD;
    }
}

NanoSwitch<Header>(Parser<Header>(), Filter<Header>()) main;
