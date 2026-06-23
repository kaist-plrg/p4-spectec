#include <nano_model.p4>

header Nanonet {
    bool ignore;
    bit<7> packetType;
    bit<8> src;
    bit<8> dst;
}

struct Header {
    Nanonet nanonet;
}

parser Parser(packet_in pkt, out Header hdr) {
    state start {
        transition accept;
    }
}

control Filter(inout Header hdr, out bool pass) {
    action reject(bit<8> rej) {
        if (rej == 0) {
            pass = true;
        } else {
            pass = false;
        }
    }
    table t {
        key = { hdr.nanonet.ignore : exact; }
        actions = { reject; }

        const entries = {
            (true) : reject(8w1);
            (false) : reject(8w0);
        }
    }
    apply {
        bool x = true;
        t.apply();
    }
}

NanoSwitch(NanoParser(), NanoFilter()) main;
