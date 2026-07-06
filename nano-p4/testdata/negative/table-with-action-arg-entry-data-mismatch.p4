// Error: a different out variable is passed to set_result than the one used in the table entry data
#include <nano_model.p4>

action set_result(out bool pass, bool val) {
    pass = val;
}

parser Parser(packet_in pkt, out Header hdr) {
    state start {
        pkt.extract(hdr.nanonet);
        transition accept;
    }
}

control Filter(inout Header hdr, out bool pass) {
    bool result = false;
    table filter_table {
        key = { hdr.nanonet.packetType : exact; }
        actions = { set_result(pass); NoAction; }
        const entries = {
            (7w1) : set_result(result, true);
            (7w2) : set_result(pass, true);
            (7w3) : set_result(pass, true);
            (7w0) : set_result(pass, 8w0);
        }
    }
    apply {
        pass = false;
        filter_table.apply();
    }
}

NanoSwitch(Parser(), Filter()) main;
