#include <nano_model.p4>

// Valid: all directional params (no directionless params).
action compute(out bool pass, inout bit<8> x) {
    x = x + 8w1;
    pass = x < 8w10;
}

parser Parser(packet_in pkt, out Header hdr) {
    state start {
        pkt.extract(hdr.nanonet);
        transition accept;
    }
}

control Filter(inout Header hdr, out bool pass) {
    apply {
        bit<8> x = 8w5;
        compute(pass, x);
    }
}

NanoSwitch(Parser(), Filter()) main;
