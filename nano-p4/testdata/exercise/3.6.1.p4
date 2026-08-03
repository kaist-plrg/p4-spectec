#include <nano_model.p4>

// Valid: directional params first, directionless at the end.
action compute(out bool pass, bit<8> threshold) {
    pass = threshold < 8w20;
}

parser Parser(packet_in pkt, out Header hdr) {
    state start {
        pkt.extract(hdr.nanonet);
        transition accept;
    }
}

control Filter(inout Header hdr, out bool pass) {
    apply {
        compute(pass, 8w10);
    }
}

NanoSwitch(Parser(), Filter()) main;
