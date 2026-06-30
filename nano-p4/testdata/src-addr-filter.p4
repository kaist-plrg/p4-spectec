#include <nano_model.p4>

action allow(out bool pass) {
    pass = true;
}

action deny(out bool pass) {
    pass = false;
}

parser Parser(packet_in pkt, out Header hdr) {
    state start {
        pkt.extract(hdr.nanonet);
        transition accept;
    }
}

control Filter(inout Header hdr, out bool pass) {
    table src_acl {
        key = { hdr.nanonet.src : exact; }
        actions = { allow(pass); deny(pass); }
        const entries = {
            (8w1) : allow(pass);
            (8w2) : allow(pass);
            (8w3) : deny(pass);
        }
    }
    apply {
        pass = false;
        src_acl.apply();
    }
}

NanoSwitch(Parser(), Filter()) main;
