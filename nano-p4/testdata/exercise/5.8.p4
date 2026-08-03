// A control with a const-entries table and a default-pass fallback.
// packetType 1 is explicitly dropped; all other types fall through and pass.
#include <nano_model.p4>

action drop(out bool pass) {
    pass = false;
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
        actions = { drop(pass); NoAction; }
        const entries = {
            (7w1) : drop(pass);
        }
    }
    apply {
        pass = true;
        acl.apply();
    }
}

NanoSwitch(Parser(), Filter()) main;
