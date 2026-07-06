// Error: ~ (bitwise not) requires an integer operand, not bool
#include <nano_model.p4>

parser Parser(packet_in pkt, out Header hdr) {
    state start {
        pkt.extract(hdr.nanonet);
        transition accept;
    }
}

control Filter(inout Header hdr, out bool pass) {
    apply {
        pass = ~hdr.nanonet.drop;
    }
}

NanoSwitch(Parser(), Filter()) main;
