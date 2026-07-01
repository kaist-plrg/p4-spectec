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
        transition select(hdr.nanonet.packetType) {
            7w1 : parse_data;
            7w0 : reject;
        }
    }

    state parse_data {
        transition accept;
    }
}

control Filter(inout Header hdr, out bool pass) {
    table protocol_filter {
        key = { hdr.nanonet.src : exact; }
        actions = { allow(pass); deny(pass); }
        const entries = {
            (8w6)  : allow(pass);
            (8w17) : allow(pass);
            (8w1)  : deny(pass);
        }
    }
    apply {
        pass = false;
        protocol_filter.apply();
    }
}

NanoSwitch(Parser(), Filter()) main;
