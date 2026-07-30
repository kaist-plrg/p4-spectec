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
        transition s1;
    }
    state s1 {
        h_t h;
        h.setValid();
        h.f = hdr.eth.src[7:0];
        hdr.hs.last = h;    // write to hs.last
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
