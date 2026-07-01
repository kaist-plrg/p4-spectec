#include <nano_model.p4>

parser Parser(packet_in pkt, out Header hdr) {
    state start {
        pkt.extract(hdr.nanonet);
        transition select(hdr.nanonet.packetType) {
            7w1 : parse_type_a;
            7w2 : parse_type_b;
        }
    }

    state parse_type_a {
        transition accept;
    }

    state parse_type_b {
        transition accept;
    }
}

control Filter(inout Header hdr, out bool pass) {
    apply {
        pass = true;
    }
}

NanoSwitch(Parser(), Filter()) main;
