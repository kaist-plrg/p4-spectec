#include <nano_model.p4>

action match(out bool pass, bool act) {
    pass = act;
}

parser Parser(packet_in pkt, out Header hdr) {
    state start {
        pkt.extract(hdr.nanonet);
        transition accept;
    }
}

control Filter(inout Header hdr, out bool pass) {
    table tbl {
        key = { hdr.nanonet.packetType : exact; }
        actions = {
            match(pass); NoAction;
        }

        const entries = {
            (7w1) : match(pass, true);
            (7w2) : match(pass, false);
        }
    }

    apply {
        pass = true;
        tbl.apply();
    }
}

NanoSwitch(Parser(), Filter()) main;
