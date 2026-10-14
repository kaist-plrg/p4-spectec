#include <core.p4>
#include <ebpf_model.p4>

struct Headers {
    bit<8> tag;
}

parser ChildParser(packet_in input, out Headers output) {
    state start {
        output.tag = 0;
        transition accept;
    }
}

parser TestParser(packet_in packet, out Headers headers) {
    ChildParser() child;
    state start {
        // Identity casts are not allowed on objects such as externs.
        child.apply((packet_in) packet, headers);
        transition accept;
    }
}

control TestFilter(inout Headers headers, out bool accept) {
    apply {
        accept = true;
    }
}

ebpfFilter<Headers>(TestParser(), TestFilter()) main;
