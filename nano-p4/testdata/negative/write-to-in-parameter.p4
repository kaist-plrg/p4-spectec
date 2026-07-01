// Error: a value is written to an in parameter
action compute(in bool pass) {
    pass = false;
}

#include <nano_model.p4>

parser Parser(packet_in pkt, out Header hdr) {
    state start {
        transition accept;
    }
}

control Filter(inout Header hdr, out bool pass) {
    apply {
        compute(true);
    }
}

NanoSwitch(Parser(), Filter()) main;
