#include <nano_model.p4>

parser Parser(packet_in pkt, out Header hdr) {
    state start {
        pkt.extract(hdr.nanonet);
        transition parse_next;
    }

    state parse_next {
        transition accept;
    }
}

control Filter(inout Header hdr, out bool pass) {
    apply {
        bit<8> val = 8w1;
        if (hdr.nanonet.dst == val) {
            pass = true;
        } else {
            pass = false;
        }
    }
}

NanoSwitch(Parser(), Filter()) main;
