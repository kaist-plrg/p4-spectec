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

enum Suits { Clubs, Diamonds, Hearts, Spades }

action f() { }

parser p(packet_in b, out Headers h, inout Meta m, inout standard_metadata_t sm) {
    state start {
        b.extract(h.op);

        transition select(h.op.a) {
            0x00: dot_prefix_enum;
            0x01: dot_prefix_function;
            default: accept;
        }
    }

    state dot_prefix_enum {
        h.op.a = 0x10 + h.op.a;
        transition accept;
    }

    state dot_prefix_function {
        h.op.a = 0x10 + h.op.a;
        transition accept;
    }
}

control vrfy(inout Headers h, inout Meta m) { apply {} }
control update(inout Headers h, inout Meta m) { apply {} }

control ingress(inout Headers h, inout Meta m, inout standard_metadata_t sm) {
    table t {
        actions = { .f; }
        key = { h.op.a : exact; }
        entries = {
            0x11 : .f();
        }
    }

    apply {
        if (h.op.a == 0x10) {
            Suits x = .Suits.Hearts;
        }

        else if (h.op.a == 0x11) {
            t.apply();
        }
    }
}

control egress(inout Headers h, inout Meta m, inout standard_metadata_t sm) { apply {} }

control deparser(packet_out b, in Headers h) {
    apply { b.emit(h.op); b.emit(h.h); }
}

V1Switch(p(), vrfy(), ingress(), egress(), update(), deparser()) main;
