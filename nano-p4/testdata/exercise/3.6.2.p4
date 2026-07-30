#include <nano_model.p4>

// Invalid: single directionless param before a directional one.
action compute(bit<8> threshold, out bool pass) {
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
        compute(8w10, pass);
    }
}

NanoSwitch(Parser(), Filter()) main;
