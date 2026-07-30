#include <nano_model.p4>

// Invalid: directionless params in the middle, followed by a directional one.
action compute(bit<8> a, bit<8> b, out bool pass) {
    pass = a < b;
}

parser Parser(packet_in pkt, out Header hdr) {
    state start {
        pkt.extract(hdr.nanonet);
        transition accept;
    }
}

control Filter(inout Header hdr, out bool pass) {
    apply {
        compute(8w10, 8w20, pass);
    }
}

NanoSwitch(Parser(), Filter()) main;
