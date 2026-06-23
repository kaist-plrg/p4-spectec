#include <nano_model.p4>

header Inner {
    bit<8> value;
}

struct Outer {
    Inner inner;
}

struct Header {
    Outer outer;
}

parser Parser(packet_in pkt, out Header hdr) {
    state start {
        pkt.extract<Inner>(hdr.outer.inner);
        transition accept;
    }
}

control Filter(inout Header hdr, out bool pass) {
    apply {
        pass = hdr.outer.inner.value == 8w42;
    }
}

NanoSwitch(Parser(), Filter()) main;
