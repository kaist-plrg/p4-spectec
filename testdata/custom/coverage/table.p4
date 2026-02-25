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

bit<8> bool_to_bit8(in bool x) {
    return 8w0;
}

parser p(packet_in b, out Headers h, inout Meta m, inout standard_metadata_t sm) {
    state start {
        b.extract(h.op);

        transition select(h.op.a) {
            0x00: key_head_abort;
            0x01: key_cons_abort;
            0x02: entry_abort;
            0x03: multiple_entries_cons_abort;
            default: accept;
        }
    }

    state key_head_abort {
        h.op.a = 0x10 + h.op.a;
        transition accept;
    }

    state key_cons_abort {
        h.op.a = 0x10 + h.op.a;
        transition accept;
    }

    state entry_abort {
        h.op.a = 0x10 + h.op.a;
        transition accept;
    }

    state multiple_entries_cons_abort {
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

    table t { actions = { f; } key = { h.op.a : exact; } default_action = f; }

    action nop() {}
    table t_key_head_abort {
        actions = { nop; }
        key = {
            bool_to_bit8(t.apply().hit) : exact;
        }
        const entries = {
            1 : nop();
        }
        default_action = nop;
    }

    table t_key_cons_abort {
        actions = { nop; }
        key = {
            h.op.a : exact;
            bool_to_bit8(t.apply().hit) : exact;
        }
        const entries = {
            (1, 1) : nop();
        }
        default_action = nop;
    }

    table t_entry_abort {
        actions = { nop; }
        key = {
            h.op.a : exact;
        }
        const entries = {
            bool_to_bit8(t.apply().hit) : nop();
        }
        default_action = nop;
    }

    table t_multiple_entries_cons_abort {
        actions = { nop; }
        key = {
            h.op.a : exact;
        }
        const entries = {
            1 : nop();
            bool_to_bit8(t.apply().hit) : nop();
        }
        default_action = nop;
    }

    apply {
        if (h.op.a == 0x10) {
            t_key_head_abort.apply();
        }

        else if (h.op.a == 0x11) {
            t_key_cons_abort.apply();
        }

        else if (h.op.a == 0x12) {
            t_entry_abort.apply();
        }

        else if (h.op.a == 0x13) {
            t_multiple_entries_cons_abort.apply();
        }
    }
}

control egress(inout Headers h, inout Meta m, inout standard_metadata_t sm) { apply {} }

control deparser(packet_out b, in Headers h) {
    apply { b.emit(h.op); b.emit(h.h); }
}

V1Switch(p(), vrfy(), ingress(), egress(), update(), deparser()) main;
