// Error: select expression is bit<7> (packetType), but case labels are bit<8>
#include <nano_model.p4>

parser Parser(packet_in pkt, out Header hdr) {
    bit<7> version = 7w1;
    state start {
        pkt.extract(hdr.nanonet);
        transition select(hdr.nanonet.packetType) {
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
