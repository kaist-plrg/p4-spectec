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
    // An element may carry at most one @name annotation.
    @name("first") @name("second") action permit() { accept = true; }
    table choose {
        actions = { permit; }
        default_action = permit();
    }
    apply { choose.apply(); }
}

ebpfFilter<Headers>(TestParser(), TestFilter()) main;
