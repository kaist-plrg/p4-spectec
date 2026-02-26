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

bit<8> bto8(in bool x) {
    return 8w0;
}

void set_value(out bit<8> i, in bit<8> val) {
    i = val;
}

Hdr[2] get_header_stack(in bool x) {
    Hdr[2] hdrs;
    return hdrs;
}

parser p(packet_in b, out Headers h, inout Meta m, inout standard_metadata_t sm) {
    state start {
        b.extract(h.op);

        transition select(h.op.a) {
            0x00: for_init_assign_abort;
            0x01: for_init_vardecl_abort;
            0x02: for_init_vardecl_empty;
            0x03: for_in_header_stack_abort;
            0x04: for_in_header_stack;
            0x05: for_in_list;
            0x06: for_in_range_lhs_abort;
            0x07: for_in_range_rhs_abort;
            0x08: for_in_body_abort;
            0x09: for_in_break;
            0x0A: for_annotation_inside;
            0x0B: for_function_update;
            0x0C: for_condition_abort;
            0x0D: for_condition_false;
            0x0E: for_return;
            0x0F: test;
            default: accept;
        }
    }

    state for_init_assign_abort {
        h.op.a = 0x10 + h.op.a;
        transition accept;
    }

    state for_init_vardecl_abort {
        h.op.a = 0x10 + h.op.a;
        transition accept;
    }

    state for_init_vardecl_empty {
        h.op.a = 0x10 + h.op.a;
        transition accept;
    }

    state for_init_empty {
        h.op.a = 0x10 + h.op.a;
        transition accept;
    }

    state for_in_header_stack_abort {
        h.op.a = 0x10 + h.op.a;
        transition accept;
    }

    state for_in_header_stack {
        h.op.a = 0x10 + h.op.a;
        transition accept;
    }

    state for_in_list {
        h.op.a = 0x10 + h.op.a;
        transition accept;
    }

    state for_in_range_lhs_abort {
        h.op.a = 0x10 + h.op.a;
        transition accept;
    }

    state for_in_range_rhs_abort {
        h.op.a = 0x10 + h.op.a;
        transition accept;
    }

    state for_in_body_abort {
        h.op.a = 0x10 + h.op.a;
        transition accept;
    }

    state for_in_break {
        h.op.a = 0x10 + h.op.a;
        transition accept;
    }

    state for_annotation_inside {
        h.op.a = 0x10 + h.op.a;
        transition accept;
    }

    state for_function_update {
        h.op.a = 0x10 + h.op.a;
        transition accept;
    }

    state for_condition_abort {
        h.op.a = 0x10 + h.op.a;
        transition accept;
    }

    state for_condition_false {
        h.op.a = 0x10 + h.op.a;
        transition accept;
    }

    state for_return {
        h.op.a = 0x10 + h.op.a;
        transition accept;
    }

    state test {
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
    table t3 { actions = { f; } key = { h.op.a : exact; } default_action = f; }
    table t4 { actions = { f; } key = { h.op.a : exact; } default_action = f; }
    table t5 { actions = { f; } key = { h.op.a : exact; } default_action = f; }
    table t6 { actions = { f; } key = { h.op.a : exact; } default_action = f; }

    apply {
        if (h.op.a == 0x10) {
            bit<8> i;
            for (i = bto8(t1.apply().hit); i < 3; i = i + 1) { }
        }

        else if (h.op.a == 0x11) {
            for (bit<8> i = bto8(t2.apply().hit); i < 3; i = i + 1) { }
        }

        else if (h.op.a == 0x12) {
            for (bit<8> i; false;) { }
        }

        else if (h.op.a == 0x13) {
            for (Hdr h in get_header_stack(t3.apply().hit)) { }
        }

        else if (h.op.a == 0x14) {
            for (Hdr h in h.h) { }
        }

        else if (h.op.a == 0x15) {
            list<bit<8>> lst = {1, 2, 3};
            for (bit<8> i in lst) { }
        }

        else if (h.op.a == 0x16) {
            for (bit<8> i in bto8(t4.apply().hit) .. 3) { }
        }

        else if (h.op.a == 0x17) {
            for (bit<8> i in 1 .. bto8(t5.apply().hit)) { }
        }

        else if (h.op.a == 0x18) {
            for (bit<8> i in 1 .. 3) {
                exit;
            }
        }

        else if (h.op.a == 0x19) {
            for (bit<8> i in 1 .. 3) {
                if (i == 2) {
                    break;
                }
            }
        }

        else if (h.op.a == 0x1A) {
            bit<8> c = 0;
            for (@my_anno bit<8> i in 1 .. 3) {
                c = c + 1;
            }
            h.h[0].a = c - 3;
        }

        else if (h.op.a == 0x1B) {
            for (bit<8> i = 0; i < 2; set_value(i, 8w100)) { }
        }

        else if (h.op.a == 0x1C) {
            for (bit<8> i = 0; i < bto8(t6.apply().hit); i = i + 1) { }
        }

        else if (h.op.a == 0x1D) {
            for (bit<8> i = 0; false; i = i + 1) { }
        }

        else if (h.op.a == 0x1E) {
            for (bit<8> i = 0; i < 1; i = i + 1) {
                return;
            }
        }
    }
}

control egress(inout Headers h, inout Meta m, inout standard_metadata_t sm) { apply {} }

control deparser(packet_out b, in Headers h) {
    apply { b.emit(h.op); b.emit(h.h); }
}

V1Switch(p(), vrfy(), ingress(), egress(), update(), deparser()) main;
