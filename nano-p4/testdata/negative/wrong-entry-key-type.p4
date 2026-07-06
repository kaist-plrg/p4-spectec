// Error: table entry key has type bit<8> but key expression has type bool
#include <nano_model.p4>

action reject(out bool pass, bit<8> rej) {
    if (rej == 8w0) {
        pass = true;
    } else {
        pass = false;
    }
}

parser Parser(packet_in pkt, out Header hdr) {
    state start {
        transition accept;
    }
}

control Filter(inout Header hdr, out bool pass) {
    table t {
        key = { hdr.nanonet.drop : exact; }
        actions = { reject(pass); }

        const entries = {
            (8w1) : reject(pass, 8w1);
            (8w0) : reject(pass, 8w0);
        }
    }
    apply {
        t.apply();
    }
}

NanoSwitch(Parser(), Filter()) main;
