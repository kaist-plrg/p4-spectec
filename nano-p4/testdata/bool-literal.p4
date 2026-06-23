#include <nano_model.p4>

struct Header {}

parser Parser(packet_in pkt, out Header hdr) {
    state start {
        transition accept;
    }
}

control Filter(inout Header hdr, out bool pass) {
    apply {
        bool accept_pkt = true;
        bool drop_pkt = false;
        pass = accept_pkt != drop_pkt;
    }
}

NanoSwitch(Parser(), Filter()) main;
