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
            16w0x0800 : parse_ipv4;
            16w0x86DD : parse_ipv6;
        }
    }

    state parse_ipv4 {
        transition accept;
    }

    state parse_ipv6 {
        transition accept;
    }
}

control Filter(inout Header hdr, out bool pass) {
    apply {
        pass = true;
    }
}

NanoSwitch<Header>(Parser(), Filter()) main;
