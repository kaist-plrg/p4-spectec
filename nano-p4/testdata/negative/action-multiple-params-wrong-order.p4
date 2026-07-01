// Error: action directionless params must trail directed params;
action compute(bit<8> a, bit<8> b, out bool pass) {
    pass = a < b;
}

#include <nano_model.p4>

parser Parser(packet_in pkt, out Header hdr) {
    state start {
        transition accept;
    }
}

control Filter(inout Header hdr, out bool pass) {
    apply {
        bit<8> x = 8w10;
        bit<8> y = 8w20;
        compute(x, y, pass);
    }
}

NanoSwitch(Parser(), Filter()) main;
