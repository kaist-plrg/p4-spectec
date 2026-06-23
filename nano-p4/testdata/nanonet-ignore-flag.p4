#include <nano_model.p4>

header Nanonet {
    bool   drop;
    bit<7> packetType;
    bit<8> src;
    bit<8> dst;
}

struct Header {
    Nanonet nanonet;
}

parser Parser(packet_in pkt, out Header hdr) {
    state start {
        pkt.extract<Nanonet>(hdr.nanonet);
        transition accept;
    }
}

control Filter(inout Header hdr, out bool pass) {
    apply {
        pass = !hdr.nanonet.drop;
    }
}

NanoSwitch(Parser(), Filter()) main;
