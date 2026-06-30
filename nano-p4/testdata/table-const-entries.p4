#include <nano_model.p4>

action drop(out bool pass) {
    pass = false;
}

action fwd(out bool pass) {
    pass = true;
}

parser Parser(packet_in pkt, out Header hdr) {
    state start {
        pkt.extract(hdr.nanonet);
        transition accept;
    }
}

control Filter(inout Header hdr, out bool pass) {
    table acl {
        key = { hdr.nanonet.packetType : exact; }
        actions = { drop(pass); fwd(pass); }
        const entries = {
            (7w1) : fwd(pass);
            (7w2) : fwd(pass);
            (7w0) : drop(pass);
        }
    }
    apply {
        pass = true;
        acl.apply();
    }
}

NanoSwitch(Parser(), Filter()) main;
