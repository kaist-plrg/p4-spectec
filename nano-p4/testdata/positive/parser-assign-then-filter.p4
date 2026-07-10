#include <nano_model.p4>

parser Parser(packet_in pkt, out Header hdr) {
    state start {
        pkt.extract(hdr.nanonet);
        transition accept;
    }
}

control Filter(inout Header hdr, out bool pass) {
    apply {
        bit<8> threshold = 8w128;
        if (hdr.nanonet.drop) {
            pass = hdr.nanonet.src > threshold;
            hdr.nanonet.drop = false;
        } else {
            pass = hdr.nanonet.src < threshold;
        }
    }
}

NanoSwitch(Parser(), Filter()) main;
