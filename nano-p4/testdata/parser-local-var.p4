#include <nano_model.p4>

header Hdr {
    bit<8> proto;
}

struct Header {
    Hdr h;
}

parser Parser(packet_in pkt, out Header hdr) {
    bit<8> version = 8w1;
    state start {
        pkt.extract<Hdr>(hdr.h);
        transition select(hdr.h.proto) {
            8w1 : parse_v1;
            8w2 : reject;
        }
    }

    state parse_v1 {
        transition accept;
    }
}

control Filter(inout Header hdr, out bool pass) {
    apply {
        pass = true;
    }
}

NanoSwitch(Parser(), Filter()) main;
