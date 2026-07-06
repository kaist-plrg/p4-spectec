#include <nano_model.p4>

parser Parser(packet_in pkt, out Header hdr) {
    state start {
        transition accept;
    }
}

control Filter(inout Header hdr, out bool pass) {
    apply {
        bit<8> x = 8w5;
        if (x > 8w0) {
            if (x < 8w10) {
                pass = true;
            } else {
                pass = false;
            }
        } else {
            pass = false;
        }
    }
}

NanoSwitch(Parser(), Filter()) main;
