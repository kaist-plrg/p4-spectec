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
        set_pass_true(false); // should fail! false is not an l-value.
    }
}

NanoSwitch(Parser(), Filter()) main;
