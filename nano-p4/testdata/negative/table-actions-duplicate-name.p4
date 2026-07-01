// Error: each action in the list of actions for a table must have a distinct name
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
    bool foobar = true;

    table acl {
        key = { hdr.nanonet.packetType : exact; }
        actions = { fwd(foobar); fwd(pass); }
    }

    apply {
        pass = true;
        acl.apply();
    }
}

NanoSwitch(Parser(), Filter()) main;
