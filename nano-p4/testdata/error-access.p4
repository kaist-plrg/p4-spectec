#include <nano_model.p4>

struct Header {}

parser Parser(packet_in pkt, out Header hdr) {
    state start {
        transition accept;
    }
}

control Filter(inout Header hdr, out bool pass) {
    apply {
        error e = error.NoError;
        pass = e == error.NoError;
    }
}

NanoSwitch(Parser(), Filter()) main;
