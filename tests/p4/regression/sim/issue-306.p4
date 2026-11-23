#include <core.p4>
#include <ebpf_model.p4>

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
        list<int> values = {1};
        accept = false;
        for (bit<8> value in values) {
            accept = value + 8w1 == 8w2;
        }
    }
}

ebpfFilter<Headers>(TestParser(), TestFilter()) main;
