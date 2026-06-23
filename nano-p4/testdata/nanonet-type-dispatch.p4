#include <nano_model.p4>

header Nanonet {
    bool   drop;
    bit<7> packetType;
    bit<8> src;
    bit<8> dst;
}

struct Header {
    Nanonet nanonet;
}

action allow(out bool pass) {
    pass = true;
}

action deny(out bool pass) {
    pass = false;
}

parser Parser(packet_in pkt, out Header hdr) {
    state start {
        pkt.extract<Nanonet>(hdr.nanonet);
        transition accept;
    }
}

control Filter(inout Header hdr, out bool pass) {
    table dispatch {
        key = { hdr.nanonet.packetType : exact; }
        actions = { allow; deny; }
        const entries = {
            (7w1) : allow(pass);
            (7w2) : allow(pass);
            (7w0) : deny(pass);
        }
    }
    apply {
        pass = false;
        dispatch.apply();
    }
}

NanoSwitch<Header>(Parser(), Filter()) main;
