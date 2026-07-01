// Error: arithmetic operands must have the same type
#include <nano_model.p4>

parser Parser(packet_in pkt, out Header hdr) {
    state start {
        transition accept;
    }
}

control Filter(inout Header hdr, out bool pass) {
    apply {
        bit<8> x = 8w10;
        bit<16> y = 16w3;
        bit<8> sum = x + y;
        pass = sum == 8w13;
    }
}

NanoSwitch(Parser(), Filter()) main;
