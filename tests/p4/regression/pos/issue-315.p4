#include <core.p4>
#include <ebpf_model.p4>

struct Headers { bool flag; }

parser TestParser(packet_in packet, out Headers headers) {
    state start {
        headers.flag = false;
        transition accept;
    }
}

control CounterOwner(out bool accept) {
    CounterArray(32w1, false) counter;
    apply {
        counter.increment(32w0);
        accept = true;
    }
}

control TableOwner() {
    table counter {
        actions = { NoAction; }
        default_action = NoAction();
    }
    apply { }
}

control TestFilter(inout Headers headers, out bool accept) {
    // Distinct @name annotations give the instances distinct paths.
    @name("shared") CounterOwner() first;
    @name("separate") TableOwner() second;
    apply {
        first.apply(accept);
    }
}

ebpfFilter<Headers>(TestParser(), TestFilter()) main;
