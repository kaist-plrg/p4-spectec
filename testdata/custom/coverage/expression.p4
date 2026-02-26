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

struct Meta {}

parser p(packet_in b, out Headers h, inout Meta m, inout standard_metadata_t sm) {
    state start {
        b.extract(h.op);

        transition select(h.op.a) {
            0x00: default_expr;
            0x01: unary_abort;
            0x02: land_lhs_abort;
            0x03: land_rhs_abort;
            0x04: lor_lhs_abort;
            0x05: lor_rhs_abort;
            0x06: cast_abort;
            0x07: invalid_header;
            0x08: tuple_abort;
            0x09: tuple_default_abort;
            0x0A: tuple_default;
            0x0B: struct_abort;
            0x0C: struct_cont;
            0x0D: struct_default;
            0x0E: struct_default_abort;
            0x0F: header_stack_size;
            0x10: index_access_abort;
            0x11: index_access_tuple;
            0x12: callee_abort;
            default: accept;
        }
    }

    state default_expr {
        h.op.a = 0x10 + h.op.a;
        Hdr x = ...;
        transition accept;
    }

    state unary_abort {
        h.op.a = 0x10 + h.op.a;
        bit<8> a = -h.h.last.a;
        transition accept;
    }

    state land_lhs_abort {
        h.op.a = 0x10 + h.op.a;
        bool a = (h.h.last.a == 8w0) && true;
        transition accept;
    }

    state land_rhs_abort {
        h.op.a = 0x10 + h.op.a;
        bool a = true && (h.h.last.a == 8w0);
        transition accept;
    }

    state lor_lhs_abort {
        h.op.a = 0x10 + h.op.a;
        bool a = (h.h.last.a == 8w0) || true;
        transition accept;
    }

    state lor_rhs_abort {
        h.op.a = 0x10 + h.op.a;
        bool a = false || (h.h.last.a == 8w0);
        transition accept;
    }

    state cast_abort {
        h.op.a = 0x10 + h.op.a;
        bit<16> casted = (bit<16>) h.h.last.a;
        transition accept;
    }

    state invalid_header {
        h.op.a = 0x10 + h.op.a;
        Hdr x = {#};
        transition accept;
    }

    state tuple_abort {
        h.op.a = 0x10 + h.op.a;
        tuple<bit<8>, bool> x = { h.h.last.a, false };
        transition accept;
    }

    state tuple_default_abort {
        h.op.a = 0x10 + h.op.a;
        tuple<bit<8>, bool> x = { h.h.last.a, ... };
        transition accept;
    }

    state tuple_default {
        h.op.a = 0x10 + h.op.a;
        tuple<bit<8>, bit<16>> x = { ... };
        transition accept;
    }

    state struct_cont {
        h.op.a = 0x10 + h.op.a;
        b.extract(h.h[0]);
        b.extract(h.h[1]);
        Hdr2 x = { c = 16w3, b = 16w2, a = 8w1 };
        h.h[0].a = x.a - 1;
        transition accept;
    }

    state struct_abort {
        h.op.a = 0x10 + h.op.a;
        Hdr x = { a = h.h.last.a };
        transition accept;
    }

    state struct_default {
        h.op.a = 0x10 + h.op.a;
        b.extract(h.h[0]);
        b.extract(h.h[1]);
        Hdr2 x = { a = 8w1, ... };
        h.h[0].a = (bit<8>) x.b;
        transition accept;
    }

    state struct_default_abort {
        h.op.a = 0x10 + h.op.a;
        Hdr2 x = { a = h.h.last.a, ... };
        transition accept;
    }

    state header_stack_size {
        h.op.a = 0x10 + h.op.a;
        h.h[0].a = (bit<8>) h.h.size - 2;
        transition accept;
    }

    state index_access_abort {
        h.op.a = 0x10 + h.op.a;
        bit<8> x = h.h[ h.h.last.a ].a;
        transition accept;
    }

    state index_access_tuple {
        h.op.a = 0x10 + h.op.a;
        tuple<bit<8>, bit<16>> t = { 8w1, 16w2 };
        h.h[0].a = t[0] - 1;
        transition accept;
    }

    state callee_abort {
        h.op.a = 0x10 + h.op.a;
        bool x = h.h[ h.h.last.a ].isValid();
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
