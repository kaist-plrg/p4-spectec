#include <core.p4>
#include <v1model.p4>

header Hdr {
    bit<8> a;
}

struct Headers {
    Hdr op;
    bit<16> checksum;
}

struct Meta {}

bit<8> n_exit() {
    exit;
    return 8w1;
}


parser p(packet_in b, out Headers h, inout Meta m, inout standard_metadata_t sm) {
    state start {
        b.extract(h.op);
        b.extract(h.checksum);
        transition accept;
    }
}

control vrfy(inout Headers h, inout Meta m) {
    apply {
        verify_checksum(
            h.op.a == 0,
            { h.op.a, n_exit() },
            h.checksum,
            HashAlgorithm.csum16
        );
    }
}

control ingress(inout Headers h, inout Meta m, inout standard_metadata_t sm) {
    apply {
        // TODO: should checksum_error be 1 if verify block exited?
        log_msg("checksum_error={}", { sm.checksum_error });
    }
}

control egress(inout Headers h, inout Meta m, inout standard_metadata_t sm) {
    apply {
        if (h.op.a == 0x1) {
            exit;
            h.op.a = 0x2;
        }
    }
}

control check(inout Headers h, inout Meta m) {
    apply {
        update_checksum(
            h.op.a == 0x2,
            { h.op.a, n_exit() },
            h.checksum,
            HashAlgorithm.csum16
        );
    }
}

control deparser(packet_out b, in Headers h) {
    apply {
        b.emit(h.op);
        b.emit(h.checksum);
    }
}

V1Switch(p(), vrfy(), ingress(), egress(), check(), deparser()) main;
