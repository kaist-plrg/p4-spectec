// Error: two states share the name 'parse_v1', which is illegal.
#include <nano_model.p4>

parser Parser(packet_in pkt, out Header hdr) {
    state start {
        pkt.extract(hdr.nanonet);
        transition parse_v1;
    }

    state parse_v1 {
        transition accept;
    }

    state parse_v1 {
        transition reject;
    }
}

control Filter(inout Header hdr, out bool pass) {
    apply {
        pass = true;
    }
}

NanoSwitch(Parser(), Filter()) main;
