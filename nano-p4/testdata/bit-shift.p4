#include <nano_model.p4>

parser Parser(packet_in pkt, out Header hdr) {
    state start {
        transition accept;
    }
}

control Filter(inout Header hdr, out bool pass) {
    apply {
        bit<8> x = 8w1;
        bit<8> shifted_left = x << 8w4;
        bit<8> shifted_right = shifted_left >> 8w2;
        pass = shifted_left == 8w16 && shifted_right == 8w4;
    }
}

NanoSwitch(Parser(), Filter()) main;
