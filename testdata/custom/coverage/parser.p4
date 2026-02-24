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

parser p(packet_in b, out Headers h, inout Meta m, inout standard_metadata_t sm) {
    state start {
        b.extract(h.op);

        transition select(h.op.a) {
            0x00: parser_empty;
            0x01: parser_if_non_else;
            0x02: parser_if_else_reject;
            0x03: parser_select_reject;
            0x04: parser_select_case_reject;
            0x05: parser_vardec_reject;
            default: accept;
        }
    }

    state parser_empty {
        h.op.a = 0x10 + h.op.a;
        ;
        transition accept;
    }

    state parser_if_non_else {
        if (true) {
            h.op.a = 0x10 + h.op.a;
        }

        if (h.h.last.a == 0) { }

        transition accept;
    }

    state parser_if_else_reject {
        if (true) {
            h.op.a = 0x10 + h.op.a;
        } else {
            h.op.a = 0;
        }

        if (false) {
            h.op.a = 0;
        } else {
        }

        if (h.h.last.a == 0) {} else {}

        transition accept;
    }

    state parser_select_reject {
        h.op.a = 0x10 + h.op.a;
        transition select (h.h.last.a) {
            default : accept;
        }
    }

    state parser_select_case_reject {
        h.op.a = 0x10 + h.op.a;
        transition select (8w1) {
            h.h.last.a : accept;
        }
    }

    state parser_vardec_reject {
        h.op.a = 0x10 + h.op.a;
        bit<8> x = h.h.last.a;
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
