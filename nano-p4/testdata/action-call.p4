#include <nano_model.p4>

struct Header {}

parser Parser(packet_in pkt, out Header hdr) {
    state start {
        transition accept;
    }
}

control Filter(inout Header hdr, out bool pass) {
    action reject(bool rej) {
        pass = rej;
    }

    apply {
        bool x = true;
        reject(x);
    }
}

NanoSwitch(NanoParser(), NanoFilter()) main;
