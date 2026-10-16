#include <core.p4>
#include <ebpf_model.p4>

header Short {
    bit<8> x;
}

header Long {
    bit<16> y;
}

header_union U {
    Short s;
    Long l;
}

struct Headers {
    bit<8> tag;
}

parser TestParser(packet_in packet, out Headers headers) {
    state start {
        headers.tag = 0;
        transition accept;
    }
}

control TestFilter(inout Headers headers, out bool accept) {
    apply {
        U u;
        // The minimum size of a header union is the maximum of its fields' minimum sizes.
        accept = u.minSizeInBits() == 16;
    }
}

ebpfFilter<Headers>(TestParser(), TestFilter()) main;
