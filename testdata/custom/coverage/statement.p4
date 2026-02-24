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
            0x06: switch_table_fallthrough;
            0x07: switch_general_fallthrough;
            0x08: switch_general_non_fallthrough_mismatch;
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

    state switch_table_fallthrough {
        h.op.a = 0x10 + h.op.a;
        b.extract(h.h.next);
        b.extract(h.h.next);
        transition accept;
    }

    state switch_general_fallthrough {
        h.op.a = 0x10 + h.op.a;
        b.extract(h.h.next);
        b.extract(h.h.next);
        transition accept;
    }

    state switch_general_non_fallthrough_mismatch {
        h.op.a = 0x10 + h.op.a;
        b.extract(h.h.next);
        b.extract(h.h.next);
        transition accept;
    }
}

control vrfy(inout Headers h, inout Meta m) { apply {} }
control update(inout Headers h, inout Meta m) { apply {} }

control ingress(inout Headers h, inout Meta m, inout standard_metadata_t sm) {
    action foo() { }
    action nop() { }

    table t {
        actions = { nop; foo; }
        key = {
            h.op.a : exact;
        }
        const entries = {
            100 : nop();
            101 : foo();
        }
        default_action = nop;
    }

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

        else if (h.op.a == 0x16) {
            bit<8> x = 101;
            switch (t.apply().action_run) {
                nop:
                foo: { x = 100; }
            }
            h.h[0].a = x - 100;
        }

        else if (h.op.a == 0x17) {
            bit<8> x = 101;
            switch (x) {
                101:
                102: { x = 100; }
            }
            h.h[0].a = x - 100;
        }

        else if (h.op.a == 0x18) {
            bit<8> x = 100;
            switch (x) {
                1:
                2: { x = 1; }
            }
            h.h[0].a = x - 100;
        }

    }
}

control egress(inout Headers h, inout Meta m, inout standard_metadata_t sm) { apply {} }

control deparser(packet_out b, in Headers h) {
    apply { b.emit(h.op); b.emit(h.h); }
}

V1Switch(p(), vrfy(), ingress(), egress(), update(), deparser()) main;
