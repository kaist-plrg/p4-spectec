#include <core.p4>
#include <v1model.p4>
header eth_t { bit<48> dst; bit<48> src; bit<16> ty; }
struct headers_t { eth_t eth; }
struct meta_t { }
parser prs(packet_in pkt, out headers_t hdr, inout meta_t meta, inout standard_metadata_t std) {
    bit<16> x = 16w1;
    state start {
        pkt.extract(hdr.eth);
        bit<8> x = 8w2;
        transition next;
    }
    state next {
        hdr.eth.ty = x;
        transition accept;
    }
}
control vfy(inout headers_t hdr, inout meta_t meta) { apply { } }
control ingress(inout headers_t hdr, inout meta_t meta, inout standard_metadata_t std) {
    apply { std.egress_spec = 1; }
}
control egress(inout headers_t hdr, inout meta_t meta, inout standard_metadata_t std) { apply { } }
control cmp(inout headers_t hdr, inout meta_t meta) { apply { } }
control dep(packet_out pkt, in headers_t hdr) { apply { pkt.emit(hdr.eth); } }
V1Switch(prs(), vfy(), ingress(), egress(), cmp(), dep()) main;
