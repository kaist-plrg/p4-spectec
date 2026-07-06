// Error: comparison operator < requires integer operands, not bool
#include <nano_model.p4>

parser Parser(packet_in pkt, out Header hdr) {
    state start {
        transition accept;
    }
}

control Filter(inout Header hdr, out bool pass) {
    apply {
        bool val = true;
        pass = val < false;
    }
}

NanoSwitch(Parser(), Filter()) main;
