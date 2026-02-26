#include <core.p4>
#include <v1model.p4>

header Hdr {
    bit<8> a;
}

header Checksum {
    bit<16> a;
}

struct Headers {
    Hdr op;
    Checksum checksum;
}

struct Meta {}

parser p(packet_in b, out Headers h, inout Meta m, inout standard_metadata_t sm) {
    state start {
        b.extract(h.op);
        b.extract(h.checksum);
        transition accept;
    }
}

control vrfy(inout Headers h, inout Meta m) {
    apply { }
}

control ingress(inout Headers h, inout Meta m, inout standard_metadata_t sm) {
    apply { }
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
    apply { }
}

control deparser(packet_out b, in Headers h) {
    apply {
        b.emit(h.op);
        b.emit(h.checksum);
    }
}

V1Switch(p(), vrfy(), ingress(), egress(), check(), deparser()) main;
