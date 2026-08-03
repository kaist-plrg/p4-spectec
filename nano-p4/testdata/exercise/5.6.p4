// Exercises parser state transitions driven by a select expression.
// The parser reads the nanonet header and dispatches on packetType:
// type 1 packets are forwarded; type 2 packets are dropped.
#include <nano_model.p4>

parser Parser(packet_in pkt, out Header hdr) {
    state start {
        pkt.extract(hdr.nanonet);
        transition select(hdr.nanonet.packetType) {
            7w1 : parse_data;
            7w2 : parse_drop;
        }
    }

    state parse_data {
        transition accept;
    }

    state parse_drop {
        transition reject;
    }
}

control Filter(inout Header hdr, out bool pass) {
    apply {
        pass = true;
    }
}

NanoSwitch(Parser(), Filter()) main;
