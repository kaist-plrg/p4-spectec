#include <core.p4>
#include <ebpf_model.p4>

struct Headers { bool flag; }

parser TestParser(packet_in packet, out Headers headers) {
    state start {
        headers.flag = false;
        transition accept;
    }
}

control TestFilter(inout Headers headers, out bool accept) {
    CounterArray(32w1, false) wrapper = {
        CounterArray(32w1, false) inner;
        CounterArray(32w1, false) inner;
    };
    apply {
        wrapper.increment(32w0);
        accept = true;
    }
}

ebpfFilter<Headers>(TestParser(), TestFilter()) main;
