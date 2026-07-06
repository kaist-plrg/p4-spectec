// Error: action match expects bool but entry passes bit<7> as the data-plane arg
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
            (7w1) : match(pass, 7w1);
            (7w2) : match(pass, 7w0);
        }
    }

    apply {
        pass = true;
        tbl.apply();
    }
}

NanoSwitch(Parser(), Filter()) main;
