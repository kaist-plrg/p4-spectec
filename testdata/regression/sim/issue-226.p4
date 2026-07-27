#include <core.p4>
#include <v1model.p4>

header eth_t { bit<48> dst; bit<48> src; bit<16> ty; }
header sz_t { bit<16> sz; }
struct headers_t { eth_t eth; sz_t sz; }
struct meta_t { }

parser prs(packet_in pkt, out headers_t hdr, inout meta_t meta, inout standard_metadata_t std) {
    state start {
        pkt.extract(hdr.eth);
        transition accept;
    }
}
control vfy(inout headers_t hdr, inout meta_t meta) { apply { } }
control ingress(inout headers_t hdr, inout meta_t meta, inout standard_metadata_t std) {
    apply {
        hdr.sz.setValid();
        hdr.sz.sz = (bit<16>)hdr.eth.minSizeInBits();
        std.egress_spec = 1;
    }
}
control egress(inout headers_t hdr, inout meta_t meta, inout standard_metadata_t std) { apply { } }
control cmp(inout headers_t hdr, inout meta_t meta) { apply { } }
control dep(packet_out pkt, in headers_t hdr) { apply { pkt.emit(hdr.eth); pkt.emit(hdr.sz); } }
V1Switch(prs(), vfy(), ingress(), egress(), cmp(), dep()) main;
