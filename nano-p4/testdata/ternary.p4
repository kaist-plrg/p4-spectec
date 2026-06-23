#include <nano_model.p4>

struct Header {}

parser Parser(packet_in pkt, out Header hdr) {
    state start {
        transition accept;
    }
}

control Filter(inout Header hdr, out bool pass) {
    apply {
        bit<8> x = 8w5;
        bit<8> y = 8w10;
        bool result = (x < y) ? true : false;
        pass = result;
    }
}

NanoSwitch<Header>(Parser<Header>(), Filter<Header>()) main;
