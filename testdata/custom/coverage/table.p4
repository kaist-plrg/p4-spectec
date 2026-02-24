#include <core.p4>
#include <v1model.p4>

header Hdr {
    bit<8> a;
}

struct Headers {
    Hdr op;
    Hdr[2] h;
}

struct Meta {}

bit<8> n_exit() {
    exit;
    return 8w1;
}

parser p(packet_in b, out Headers h, inout Meta m, inout standard_metadata_t sm) {
    state start {
        b.extract(h.op);

        transition select(h.op.a) {
            0x00: key_abort;
            0x01: entry_abort;
            default: accept;
        }
    }

    state key_abort {
        h.op.a = 0x10 + h.op.a;
        transition accept;
    }

    state entry_abort {
        h.op.a = 0x10 + h.op.a;
        transition accept;
    }
}

control vrfy(inout Headers h, inout Meta m) { apply {} }
control update(inout Headers h, inout Meta m) { apply {} }

control ingress(inout Headers h, inout Meta m, inout standard_metadata_t sm) {
    action nop() {}
    table t1 {
        actions = { nop; }
        key = {
            n_exit() : exact;
        }
        const entries = {
            1 : nop();
        }
        default_action = nop;
    }
    table t2 {
        actions = { nop; }
        key = {
            h.op.a : exact;
        }
        const entries = {
            n_exit() : nop();
        }
        default_action = nop;
    }
    apply {
        if (h.op.a == 0x10) {
            t1.apply();
        }

        else if (h.op.a == 0x11) {
            t2.apply();
        }
    }
}

control egress(inout Headers h, inout Meta m, inout standard_metadata_t sm) { apply {} }

control deparser(packet_out b, in Headers h) {
    apply { b.emit(h.op); b.emit(h.h); }
}

V1Switch(p(), vrfy(), ingress(), egress(), update(), deparser()) main;
