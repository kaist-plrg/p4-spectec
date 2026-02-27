#include <core.p4>
#include <v1model.p4>

header Hdr {
    bit<8> a;
}

struct Headers {
    Hdr op;
}

struct Meta {}

bit<8> bto8(in bool x) {
    return 8w0;
}

parser p(packet_in b, out Headers h, inout Meta m, inout standard_metadata_t sm) {
    state start {
        b.extract(h.op);

        transition select(h.op.a) {
            0x00: for_in_range_lhs_abort;
            0x01: for_in_range_rhs_abort;
            default: accept;
        }
    }

    state for_in_range_lhs_abort {
        h.op.a = 0x10 + h.op.a;
        transition accept;
    }

    state for_in_range_rhs_abort {
        h.op.a = 0x10 + h.op.a;
        transition accept;
    }
}

control vrfy(inout Headers h, inout Meta m) { apply {} }
control update(inout Headers h, inout Meta m) { apply {} }

control ingress(inout Headers h, inout Meta m, inout standard_metadata_t sm) {
    action f() {
        exit;
    }

    table t1 { actions = { f; } key = { h.op.a : exact; } default_action = f; }
    table t2 { actions = { f; } key = { h.op.a : exact; } default_action = f; }

    apply {
        if (h.op.a == 0x10) {
            for (bit<8> i in bto8(t1.apply().hit) .. 3) { }
        }

        else if (h.op.a == 0x11) {
            for (bit<8> i in 1 .. bto8(t2.apply().hit)) { }
        }
    }
}

control egress(inout Headers h, inout Meta m, inout standard_metadata_t sm) { apply {} }

control deparser(packet_out b, in Headers h) {
    apply { b.emit(h.op); }
}

V1Switch(p(), vrfy(), ingress(), egress(), update(), deparser()) main;
