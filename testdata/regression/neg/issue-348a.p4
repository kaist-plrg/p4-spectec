#include <core.p4>
#include <ebpf_model.p4>

header H {
    bit<8> x;
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
        const H[2] hs = { { 8w0 }, { 8w7 } };
        tuple<bit<8>> values = { 8w9 };
        // Stack shifts mutate their receiver, which must be a writable l-value.
        hs.pop_front(1);
        bit<8> value = values[hs[0].x];
        accept = value == 8w9;
    }
}

ebpfFilter<Headers>(TestParser(), TestFilter()) main;
