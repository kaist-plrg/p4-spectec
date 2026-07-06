// Error: arithmetic operator + requires integer operands, not bool
#include <nano_model.p4>

parser Parser(packet_in pkt, out Header hdr) {
    state start {
        transition accept;
    }
}

control Filter(inout Header hdr, out bool pass) {
    apply {
        bool accept_pkt = true;
        bool drop_pkt = false;
        bool result = accept_pkt + drop_pkt;
        pass = result;
    }
}

NanoSwitch(Parser(), Filter()) main;
