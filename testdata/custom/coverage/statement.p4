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

bool b_exit() {
    exit;
    return true;
}

bit<8> n_exit() {
    exit;
    return 8w1;
}

bool b_return_exit() {
    return b_exit();
}

parser p(packet_in b, out Headers h, inout Meta m, inout standard_metadata_t sm) {
    state start {
        b.extract(h.op);

        transition select(h.op.a) {
            0x00: assignment_compound_abort;
            0x01: call_abort;
            0x02: conditional_cond_abort;
            0x03: conditional_else_cond_abort;
            0x04: return_abort;
            0x05: switch_abort;
            default: accept;
        }
    }

    state assignment_compound_abort {
        h.op.a = 0x10 + h.op.a;
        bit<8> x = 0;
        x += h.h.last.a;
        transition accept;
    }

    state call_abort {
        h.op.a = 0x10 + h.op.a;
        b.extract(h.h.next);
        b.extract(h.h.next);
        h.h.next.isValid();
        transition accept;
    }

    state conditional_cond_abort {
        h.op.a = 0x10 + h.op.a;
        transition accept;
    }

    state conditional_else_cond_abort {
        h.op.a = 0x10 + h.op.a;
        transition accept;
    }

    state return_abort {
        h.op.a = 0x10 + h.op.a;
        transition accept;
    }

    state switch_abort {
        h.op.a = 0x10 + h.op.a;
        transition accept;
    }
}

control vrfy(inout Headers h, inout Meta m) { apply {} }
control update(inout Headers h, inout Meta m) { apply {} }

control ingress(inout Headers h, inout Meta m, inout standard_metadata_t sm) {
    apply {
        if (h.op.a == 0x12) {
            if (b_exit())
                log_msg("unreachable");
        }

        else if (h.op.a == 0x13) {
            if (b_exit())
                log_msg("unreachable");
            else
                log_msg("unreachable");
        }

        else if (h.op.a == 0x14) {
            b_return_exit();
        }

        else if (h.op.a == 0x15) {
            switch (n_exit()) {
                default: {
                    bit<8> nop = 0;
                }
            }
        }
    }
}

control egress(inout Headers h, inout Meta m, inout standard_metadata_t sm) { apply {} }

control deparser(packet_out b, in Headers h) {
    apply { b.emit(h.op); b.emit(h.h); }
}

V1Switch(p(), vrfy(), ingress(), egress(), update(), deparser()) main;
