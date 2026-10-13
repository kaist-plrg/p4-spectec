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
        const H h = { 8w0 };
        // Validity updates mutate their receiver, which must be a writable l-value.
        h.setInvalid();
        accept = true;
    }
}

ebpfFilter<Headers>(TestParser(), TestFilter()) main;
