#include <nano_model.p4>

struct Header {}

action compute(out bool pass, bit<8> a, bit<8> b) {
    pass = a < b;
}

parser Parser(packet_in pkt, out Header hdr) {
    state start {
        transition accept;
    }
}

control Filter(inout Header hdr, out bool pass) {
    apply {
        bit<8> x = 8w10;
        bit<8> y = 8w20;
        compute(pass, x, y);
    }
}

NanoSwitch(Parser(), Filter()) main;
