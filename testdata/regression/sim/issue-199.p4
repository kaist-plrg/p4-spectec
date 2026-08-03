#include <core.p4>
#include <v1model.p4>
header eth_t { bit<48> dst; bit<48> src; bit<16> ty; }
header h_t { bit<8> f; }
struct headers_t { eth_t eth; h_t[2] hs; }
struct meta_t { }
parser prs(packet_in pkt, out headers_t hdr, inout meta_t meta, inout standard_metadata_t std) {
    state start {
        pkt.extract(hdr.eth);
        pkt.extract(hdr.hs[0]);
        pkt.extract(hdr.hs[1]);
        transition accept;
    }
}
control vfy(inout headers_t hdr, inout meta_t meta) { apply { } }
control ingress(inout headers_t hdr, inout meta_t meta, inout standard_metadata_t std) {
    apply {
        bit<32> dyn = (bit<32>) hdr.eth.ty;   // dynamic from packet
        bit<32> s = hdr.hs.size + dyn;         // fixed-width add on .size
        std.egress_spec = (bit<9>) s;
    }
}
control egress(inout headers_t hdr, inout meta_t meta, inout standard_metadata_t std) { apply { } }
control cmp(inout headers_t hdr, inout meta_t meta) { apply { } }
control dep(packet_out pkt, in headers_t hdr) { apply { pkt.emit(hdr.eth); } }
V1Switch(prs(), vfy(), ingress(), egress(), cmp(), dep()) main;
