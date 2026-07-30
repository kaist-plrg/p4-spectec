#include <nano_model.p4>

parser Parser(packet_in pkt, out Header hdr) {
    state start {
        pkt.extract(hdr.nanonet);
        transition accept;
    }
}

control Filter(inout Header hdr, out bool pass) {
    table filter_table {
        key = { hdr.nanonet.packetType : exact; }
        actions = { set_result(pass); NoAction; }
    }
    apply {
        pass = false;
        filter_table.apply();
    }
}

NanoSwitch(Parser(), Filter()) main;
