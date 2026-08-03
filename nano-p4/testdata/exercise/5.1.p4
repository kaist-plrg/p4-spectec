// Assigns to a variable declared in an outer scope from inside a nested block.
#include <nano_model.p4>

parser Parser(packet_in pkt, out Header hdr) {
    state start {
        transition accept;
    }
}

control Filter(inout Header hdr, out bool pass) {
    apply {
        bool result = false;
        {
            result = true;
        }
        pass = result;
    }
}

NanoSwitch(Parser(), Filter()) main;
