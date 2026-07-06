// Error: comparison between bit<8> and bit<16>
#include <nano_model.p4>

parser Parser(packet_in pkt, out Header hdr) {
    state start {
        transition accept;
    }
}

control Filter(inout Header hdr, out bool pass) {
    apply {
        bit<8> x = 8w5;
        bit<16> y = 16w10;
        pass = x < y;
    }
}

NanoSwitch(Parser(), Filter()) main;
