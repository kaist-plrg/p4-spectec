// Error: bitwise & between bit<8> and bit<16>. operands must have the same type
#include <nano_model.p4>

parser Parser(packet_in pkt, out Header hdr) {
    state start {
        transition accept;
    }
}

control Filter(inout Header hdr, out bool pass) {
    apply {
        bit<8> x = 8w0xAB;
        bit<16> mask = 16w0x00F0;
        bit<8> hi = x & mask;
        pass = hi == 8w0xA0;
    }
}

NanoSwitch(Parser(), Filter()) main;
