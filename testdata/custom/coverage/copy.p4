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

void arg_abort(in bit<8> arg) { }
void f_in(in bit<8> a) { }
void f_inout(inout bit<8> pirate) { }

parser subparser(packet_in b, inout Hdr hdr, inout Headers h, in bit<8> w) {
    bit<8> x = h.h.last.a;

    state start {
        transition accept;
    }
}

parser p(packet_in b, out Headers h, inout Meta m, inout standard_metadata_t sm) {
    state start {
        b.extract(h.op);

        transition select(h.op.a) {
            0x00: copy_in_abort;
            0x01: copy_inout_abort;
            0x02: action_abort;
            0x03: defined_function_abort;
            0x04: extern_function_abort;
            0x05: parser_apply_copyin_abort;
            0x06: parser_apply_local_decl_abort;
            0x07: control_apply_copyin_abort;
            0x08: control_apply_local_decl_abort;
            default: accept;
        }
    }

    state copy_in_abort {
        h.op.a = 0x10 + h.op.a;
        f_in(h.h.last.a);
        transition accept;
    }

    state copy_inout_abort {
        h.op.a = 0x10 + h.op.a;
        b.extract(h.h.next);
        b.extract(h.h.next);
        f_inout(h.h.next.a);
        transition accept;
    }

    state action_abort {
        h.op.a = 0x10 + h.op.a;
        transition accept;
    }

    state defined_function_abort {
        h.op.a = 0x10 + h.op.a;
        transition accept;
    }

    state extern_function_abort {
        h.op.a = 0x10 + h.op.a;
        transition accept;
    }

    state parser_apply_copyin_abort {
        h.op.a = 0x10 + h.op.a;
        subparser.apply(b, h.op, h, h.h.last.a);
        transition accept;
    }

    state parser_apply_local_decl_abort {
        h.op.a = 0x10 + h.op.a;
        subparser.apply(b, h.op, h, 8w1);
        transition accept;
    }

    state control_apply_copyin_abort {
        h.op.a = 0x10 + h.op.a;
        transition accept;
    }

    state control_apply_local_decl_abort {
        h.op.a = 0x10 + h.op.a;
        transition accept;
    }
}

control vrfy(inout Headers h, inout Meta m) { apply {} }
control update(inout Headers h, inout Meta m) { apply {} }

control c(in bit<8> w) {
    bit<8> x = n_exit();

    apply {}
}

control ingress(inout Headers h, inout Meta m, inout standard_metadata_t sm) {
    action copy_in_abort(bit<8> a) {
    }

    table t1 {
        actions = { copy_in_abort; }
        key = {
            h.op.a : exact;
        }
        const entries = {
            0x12  : copy_in_abort(n_exit());
        }
    }

    apply {
        if (h.op.a == 0x12) {
            t1.apply();
        }

        else if (h.op.a == 0x13) {
            arg_abort(n_exit());
        }

        else if (h.op.a == 0x14) {
            digest((bit<32>) n_exit(), 8w1);
        }

        else if (h.op.a == 0x17) {
            c.apply(n_exit());
        }

        else if (h.op.a == 0x18) {
            c.apply(8w1);
        }
    }
}

control egress(inout Headers h, inout Meta m, inout standard_metadata_t sm) { apply {} }

control deparser(packet_out b, in Headers h) {
    apply { b.emit(h.op); b.emit(h.h); }
}

V1Switch(p(), vrfy(), ingress(), egress(), update(), deparser()) main;
