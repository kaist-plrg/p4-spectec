// A control with a const-entries table that routes packets by type.
// packetType 1 is forwarded; all other types are dropped.
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
        }
    }
    apply {
        pass = false;
        acl.apply();
    }
}

NanoSwitch(Parser(), Filter()) main;
