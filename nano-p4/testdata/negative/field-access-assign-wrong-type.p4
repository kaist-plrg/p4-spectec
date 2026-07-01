// Error: hdr.nanonet.src is bit<8>, assigning bool to it is a type mismatch
#include <nano_model.p4>

parser Parser(packet_in pkt, out Header hdr) {
    state start {
        pkt.extract(hdr.nanonet);
        transition accept;
    }
}

control Filter(inout Header hdr, out bool pass) {
    apply {
        hdr.nanonet.src = true;
        pass = true;
    }
}

NanoSwitch(Parser(), Filter()) main;
