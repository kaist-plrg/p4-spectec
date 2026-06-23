#include <nano_model.p4>

header test_header {
    bit<8> value;
}

header next_header {
    bit<8> value;
}

struct Header {
    test_header first;
    next_header next;
}

parser Parser(packet_in pkt, out Header hdr) {
    state start {
        pkt.extract<test_header>(hdr.first);
        transition parse_next;
    }

    state parse_next {
        pkt.extract<next_header>(hdr.next);
        transition accept;
    }
}

control Filter(inout Header hdr, out bool pass) {
    apply {
        bit<8> val = 8w1;
        if (hdr.next.value == val) {
            pass = true;
        } else {
            pass = false;
        }
    }
}

NanoSwitch(Parser(), Filter()) main;
