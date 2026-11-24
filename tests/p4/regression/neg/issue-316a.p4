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
    // A table's @name may not reuse the store path of another object.
    CounterArray(32w1, false) counter;

    @name("counter")
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
