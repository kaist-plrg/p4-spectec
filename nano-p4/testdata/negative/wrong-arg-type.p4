// Error: reject expects bool for rej, but caller passes bit<8>
#include <nano_model.p4>

action reject(out bool pass, bool rej) {
    pass = !rej;
}

parser Parser(packet_in pkt, out Header hdr) {
    state start {
        transition accept;
    }
}

control Filter(inout Header hdr, out bool pass) {
    apply {
        bit<8> x = 8w1;
        reject(pass, x);
    }
}

NanoSwitch(Parser(), Filter()) main;
