#include <nano_model.p4>

parser Parser(packet_in pkt, out Header hdr) {
    state start {
        pkt.extract(hdr.nanonet);
        transition accept;
    }
}

control Filter(inout Header hdr, out bool pass) {
    apply {
        hdr.nanonet.src = 8w10;
    }
}

NanoSwitch(Parser(), Filter()) main;
