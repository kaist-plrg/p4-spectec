#include <nano_model.p4>

header Nanonet {
    bool drop;
    bit<7> packetType;
    bit<8> src;
    bit<8> dst;
}

struct Header {
    Nanonet nanonet;
}

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
        actions = { reject; }

        const entries = {
            (true) : reject(pass, 8w1);
            (false) : reject(pass, 8w0);
        }
    }
    apply {
        t.apply();
    }
}

NanoSwitch<Header>(Parser<Header>(), Filter<Header>()) main;
