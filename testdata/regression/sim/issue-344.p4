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
        // Top-level identity casts are restricted, but element casts inside
        // an aggregate cast still apply to every element type.
        list<string> names = (list<string>) { "a" };
        list<match_kind> kinds = (list<match_kind>) { exact };
        accept = true;
    }
}

ebpfFilter<Headers>(TestParser(), TestFilter()) main;
