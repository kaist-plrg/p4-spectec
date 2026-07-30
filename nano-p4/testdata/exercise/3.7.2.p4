// Error: no state named 'start', which is required as the parser entry point.
#include <nano_model.p4>

parser Parser(packet_in pkt, out Header hdr) {
    state parse_v1 {
        pkt.extract(hdr.nanonet);
        transition accept;
    }
}

control Filter(inout Header hdr, out bool pass) {
    apply {
        pass = true;
    }
}

NanoSwitch(Parser(), Filter()) main;
