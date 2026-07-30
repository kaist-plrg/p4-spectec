#include <nano_model.p4>

action set_pass_true(out bool pass) {
    pass = true;
}

parser Parser(packet_in pkt, out Header hdr) {
    state start {
        pkt.extract(hdr.nanonet);
        transition accept;
    }
}

control Filter(inout Header hdr, out bool pass) {
    apply {
        bit<8> x = 8w30;
        set_pass_true(x); // should fail! x is not type bool
    }
}

NanoSwitch(Parser(), Filter()) main;
