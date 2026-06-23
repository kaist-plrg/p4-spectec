#include <nano_model.p4>

header Ethernet {
    bit<16> ethertype;
}

struct Header {
    Ethernet eth;
}

parser Parser(packet_in pkt, out Header hdr) {
    state start {
        pkt.extract<Ethernet>(hdr.eth);
        transition select(hdr.eth.ethertype) {
            0x0800 : parse_ipv4;
            0xFFFF : reject;
        }
    }

    state parse_ipv4 {
        transition accept;
    }
}

control Filter(inout Header hdr, out bool pass) {
    apply {
        pass = true;
    }
}

NanoSwitch<Header>(Parser<Header>(), Filter<Header>()) main;
