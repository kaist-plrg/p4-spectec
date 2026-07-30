// Writes to a nested struct and header field via member-access l-values.
#include <nano_model.p4>

parser Parser(packet_in pkt, out Header hdr) {
    state start {
        pkt.extract(hdr.nanonet);
        transition accept;
    }
}

control Filter(inout Header hdr, out bool pass) {
    apply {
        hdr.nanonet.drop = false;
        pass = !hdr.nanonet.drop;
    }
}

NanoSwitch(Parser(), Filter()) main;
