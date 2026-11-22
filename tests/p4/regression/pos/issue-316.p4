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
    // A table's @name distinct from other objects is accepted.
    CounterArray(32w1, false) counter;

    @name("route_external")
    table route {
        actions = { NoAction; }
        default_action = NoAction();
    }

    apply {
        counter.increment(32w0);
        accept = true;
    }
}

ebpfFilter<Headers>(TestParser(), TestFilter()) main;
