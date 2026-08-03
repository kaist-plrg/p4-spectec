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
        pkt.extract(hdr.nanonet);
        transition accept;
    }
}

control Filter(inout Header hdr, out bool pass) {
    table t {
        key = { hdr.nanonet.drop : exact; }
        actions = { reject(pass); }

        const entries = {
            (true) : reject(pass, 8w1);
            (false) : reject(pass, 8w0);
        }
    }
    apply {
        t.apply();
    }
}

NanoSwitch(Parser(), Filter()) main;
