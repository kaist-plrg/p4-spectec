#include <nano_model.p4>

header Ethernet {
    bit<48> dst;
    bit<48> src;
    bit<16> ethertype;
}

struct Header {
    Ethernet eth;
}

action set_result(out bool pass, bool val) {
    pass = val;
}

parser Parser(packet_in pkt, out Header hdr) {
    state start {
        pkt.extract<Ethernet>(hdr.eth);
        transition accept;
    }
}

control Filter(inout Header hdr, out bool pass) {
    table filter_table {
        key = { hdr.eth.ethertype : exact; }
        actions = { set_result; NoAction; }
        const entries = {
            (0x0800) : set_result(pass, true);
            (0x0806) : set_result(pass, true);
            (0x86DD) : set_result(pass, true);
            (0xDEAD) : set_result(pass, false);
        }
    }
    apply {
        pass = false;
        filter_table.apply();
    }
}

NanoSwitch(Parser(), Filter()) main;
