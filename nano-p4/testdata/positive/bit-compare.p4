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
        pass = a < b && b > a && a <= 8w100 && b >= 8w200;
    }
}

NanoSwitch(Parser(), Filter()) main;
