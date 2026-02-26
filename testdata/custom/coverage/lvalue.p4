#include <core.p4>
#include <v1model.p4>

header Hdr {
    bit<8> a;
}

struct Headers {
    Hdr op;
    Hdr[2] h;
}

header Hdr2 {
    bit<8> a;
    bit<16> b;
    bit<16> c;
}

header Hdr3 {
    bit<8> a;
}

struct Meta {}

void f(out bit<8> a) { }

parser p(packet_in b, out Headers h, inout Meta m, inout standard_metadata_t sm) {
    state start {
        b.extract(h.op);

        transition select(h.op.a) {
            0x00: dontcare;
            0x01: lvalue_member_abort;
            0x02: lvalue_index_abort;
            0x03: lvalue_slice_base_abort;
            0x04: lvalue_slice_rhs_abort;
            0x05: lvalue_parenthesized;
            default: accept;
        }
    }

    state dontcare {
        h.op.a = 0x10 + h.op.a;
        f(a = _);
        transition accept;
    }

    state lvalue_member_abort {
        h.op.a = 0x10 + h.op.a;
        b.extract(h.h.next);
        b.extract(h.h.next);
        // f(h.h.next.a);
        transition accept;
    }

    state lvalue_index_abort {
        h.op.a = 0x10 + h.op.a;
        // h.h[h.h.last.a].a = 8w1;
        transition accept;
    }

    state lvalue_slice_base_abort {
        h.op.a = 0x10 + h.op.a;
        b.extract(h.h.next);
        b.extract(h.h.next);
        // h.h.next.a[7:6] = 2w1;
        transition accept;
    }

    state lvalue_slice_rhs_abort {
        h.op.a = 0x10 + h.op.a;
        bit<16> x = 16w3;
        x[15:8] = h.h.last.a;
        transition accept;
    }

    state lvalue_parenthesized {
        h.op.a = 0x10 + h.op.a;
        bit<16> x;
        (x) = 3;
        transition accept;
    }
}

control vrfy(inout Headers h, inout Meta m) { apply {} }
control update(inout Headers h, inout Meta m) { apply {} }

control ingress(inout Headers h, inout Meta m, inout standard_metadata_t sm) {
    apply { }
}

control egress(inout Headers h, inout Meta m, inout standard_metadata_t sm) { apply {} }

control deparser(packet_out b, in Headers h) {
    apply { b.emit(h.op); b.emit(h.h); }
}

V1Switch(p(), vrfy(), ingress(), egress(), update(), deparser()) main;
