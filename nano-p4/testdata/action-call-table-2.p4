#include <nano_model.p4>

header Ethernet {
    bit<48> destination;
    bit<48> source;
    bit<16> protocol;
}

struct Header {
    Ethernet ethernet;
}

action match(out bool pass, bool act) {
    pass = act;
}

parser Parser(packet_in pkt, out Header hdr) {
    state start {
        pkt.extract<Ethernet>(hdr.ethernet);
        transition accept;
    }
}

control Filter(inout Header hdr, out bool pass) {
    table tbl {
        key = { hdr.ethernet.protocol : exact; }
        actions = {
            match; NoAction;
        }

        const entries = {
            (0x0800) : match(pass, true);
            (0xD000) : match(pass, false);
        }
    }

    apply {
        pass = true;
        tbl.apply();
    }
}

NanoSwitch(Parser(), Filter()) main;
