#include <nano_model.p4>

parser Parser(packet_in pkt, out Header hdr) {
    state start {
        transition accept;
    }
}

control Filter(inout Header hdr, out bool pass) {
    apply {
        bit<8> val = 8w64;
        pass = val < 8w128;
    }
}

NanoSwitch(Parser(), Filter()) main;
