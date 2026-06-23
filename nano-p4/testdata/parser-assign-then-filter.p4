#include <nano_model.p4>

header Hdr {
    bit<8> kind;
    bit<8> value;
}

struct Header {
    Hdr h;
}

parser Parser(packet_in pkt, out Header hdr) {
    state start {
        pkt.extract<Hdr>(hdr.h);
        transition accept;
    }
}

control Filter(inout Header hdr, out bool pass) {
    apply {
        bit<8> threshold = 8w128;
        if (hdr.h.kind == 8w1) {
            pass = hdr.h.value > threshold;
        } else {
            pass = hdr.h.value < threshold;
        }
    }
}

NanoSwitch(Parser(), Filter()) main;
