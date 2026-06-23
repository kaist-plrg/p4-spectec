#include <nano_model.p4>

header Ethernet {
    bit<48> dst;
    bit<48> src;
    bit<16> ethertype;
}

struct Header {
    Ethernet eth;
}

parser Parser(packet_in pkt, out Header hdr) {
    state start {
        pkt.extract<Ethernet>(hdr.eth);
        transition accept;
    }
}

control Filter(inout Header hdr, out bool pass) {
    apply {
        pass = hdr.eth.dst == 48w0xFFFFFFFFFFFF;
    }
}

NanoSwitch<Header>(Parser<Header>(), Filter<Header>()) main;
