#include <nano_model.p4>

struct Header {}

parser Parser(packet_in pkt, out Header hdr) {
    state start {
        transition accept;
    }
}

control Filter(inout Header hdr, out bool pass) {
    apply {
        bit<8> x = 8w10;
        bit<8> y = 8w3;
        bit<8> sum = x + y;
        bit<8> diff = x - y;
        bit<8> prod = x * y;
        pass = sum == 8w13 && diff == 8w7 && prod == 8w30;
    }
}

NanoSwitch<Header>(Parser<Header>(), Filter<Header>()) main;
