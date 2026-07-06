#include <nano_model.p4>

parser Parser(packet_in pkt, out Header hdr) {
    state start {
        transition accept;
    }
}

control Filter(inout Header hdr, out bool pass) {
    apply {
        bit<8> x = 8w0xAB;
        bit<8> mask = 8w0xF0;
        bit<8> hi = x & mask;
        bit<8> lo = x & 8w0x0F;
        bit<8> combined = hi | lo;
        bit<8> flipped = ~x;
        pass = hi == 8w0xA0 && lo == 8w0x0B && combined == x && flipped == 8w0x54;
    }
}

NanoSwitch(Parser(), Filter()) main;
