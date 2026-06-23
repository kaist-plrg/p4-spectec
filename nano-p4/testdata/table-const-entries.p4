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
    table acl {
        key = { hdr.eth.ethertype : exact; }
        actions = { drop; fwd; }
        const entries = {
            (0x0800) : fwd(pass);
            (0x0806) : fwd(pass);
            (0xDEAD) : drop(pass);
        }
    }
    apply {
        pass = true;
        acl.apply();
    }
}

NanoSwitch<Header>(Parser<Header>(), Filter<Header>()) main;
