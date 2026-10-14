#include <core.p4>
#include <ebpf_model.p4>

struct Headers {
    bit<8> tag;
}

action choose() { }

parser TestParser(packet_in packet, out Headers headers) {
    state start {
        headers.tag = 0;
        transition accept;
    }
}

control TestFilter(inout Headers headers, out bool accept) {
    table decision {
        // Table typing resolves the dotted global reference at any cursor.
        actions = { .choose; }
        default_action = .choose();
    }
    apply {
        // Table application invokes the action at LOCAL.
        decision.apply();
        // A direct dotted call at LOCAL must also resolve to the global action.
        .choose();
        accept = true;
    }
}

ebpfFilter<Headers>(TestParser(), TestFilter()) main;
