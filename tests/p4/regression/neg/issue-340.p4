// Keep the core declarations, but rename the global NoAction action.
#define NoAction PreludeNoAction
#include <core.p4>
#undef NoAction
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
    // Table_ok inserts .NoAction as the default action, which does not exist.
    table choose {
        actions = { }
    }
    apply {
        choose.apply();
        accept = true;
    }
}

ebpfFilter<Headers>(TestParser(), TestFilter()) main;
