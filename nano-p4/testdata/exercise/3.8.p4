// Valid: control with a table local declaration.
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
    table classify {
        key = { hdr.nanonet.packetType : exact; }
        actions = { drop(pass); fwd(pass); NoAction; }
    }
    apply {
        pass = true;
        classify.apply();
    }
}

NanoSwitch(Parser(), Filter()) main;
