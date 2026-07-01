// Error: && requires both operands to be bool
#include <nano_model.p4>

parser Parser(packet_in pkt, out Header hdr) {
    state start {
        transition accept;
    }
}

control Filter(inout Header hdr, out bool pass) {
    apply {
        bit<8> a = 8w100;
        bit<8> b = 8w200;
        pass = a && b > a;
    }
}

NanoSwitch(Parser(), Filter()) main;
