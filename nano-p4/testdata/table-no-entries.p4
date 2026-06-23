#include <nano_model.p4>

header Ethernet {
    bit<48> dst;
    bit<48> src;
    bit<16> ethertype;
}

struct Header {
    Ethernet eth;
}

action drop(out bool pass) {
    pass = false;
}

action fwd(out bool pass) {
    pass = true;
}

parser Parser(packet_in pkt, out Header hdr) {
    state start {
        pkt.extract<Ethernet>(hdr.eth);
        transition accept;
    }
}

control Filter(inout Header hdr, out bool pass) {
    table classify {
        key = { hdr.eth.ethertype : exact; }
        actions = { drop; fwd; NoAction; }
    }
    apply {
        pass = true;
        classify.apply();
    }
}

NanoSwitch<Header>(Parser<Header>(), Filter<Header>()) main;
