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
        list<bit<8>> left = {8w1};
        list<bit<8>> right = {8w1, 8w2};
        list<bit<8>> same = {8w1};
        list<bit<8>> other = {8w2};
        // Program_ok accepts this; the zipped bin_eq repetition has unequal lengths.
        // Equal-length lists must still compare elementwise.
        accept = left != right && left == same && left != other;
    }
}

ebpfFilter<Headers>(TestParser(), TestFilter()) main;
