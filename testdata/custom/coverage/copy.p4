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

void f_in(in bit<8> a) { }
void f_inout(inout bit<8> a) { }

bit<8> bool_to_bit8(in bool x) {
    return 8w0;
}

bit<32> bool_to_bit32(in bool x) {
    return 32w0;
}

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
            0x02: defined_function_abort;
            0x03: extern_function_abort;
            0x04: control_apply_copyin_abort;
            0x05: control_apply_local_decl_abort;
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

    state defined_function_abort {
        h.op.a = 0x10 + h.op.a;
        transition accept;
    }

    state extern_function_abort {
        h.op.a = 0x10 + h.op.a;
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

control c(in bool w) {
    action f() {
        exit;
    }
    table t {
        actions = { f; }
        key = {
            8w0 : exact;
        }
        default_action = f;
    }
    apply {
        bool x = t.apply().hit;
    }
}

control ingress(inout Headers h, inout Meta m, inout standard_metadata_t sm) {
    action copy_in_abort(bit<8> a) {
    }
    action f() {
        exit;
    }

    table t1 {
        actions = { f; }
        key = {
            h.op.a : exact;
        }
        default_action = f;
    }

    table t2 {
        actions = { f; }
        key = {
            h.op.a : exact;
        }
        default_action = f;
    }

    table t3 {
        actions = { f; }
        key = {
            h.op.a : exact;
        }
        default_action = f;
    }

    c() c1;
    c() c2;

    apply {
        if (h.op.a == 0x12) {
            f_in(bool_to_bit8(t1.apply().hit));
            h.op.a = 0;
        }

        else if (h.op.a == 0x13) {
            digest(bool_to_bit32(t2.apply().hit), 8w1);
            h.op.a = 0;
        }

        else if (h.op.a == 0x14) {
            c1.apply(t3.apply().hit);
            h.op.a = 0;
        }

        else if (h.op.a == 0x15) {
            c2.apply(true);
            h.op.a = 0;
        }
    }
}

control egress(inout Headers h, inout Meta m, inout standard_metadata_t sm) { apply {} }

control deparser(packet_out b, in Headers h) {
    apply { b.emit(h.op); b.emit(h.h); }
}

V1Switch(p(), vrfy(), ingress(), egress(), update(), deparser()) main;
