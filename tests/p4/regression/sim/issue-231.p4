#include <core.p4>
#include <v1model.p4>

header eth_t { bit<48> dst; bit<48> src; bit<16> ty; }
header H { bit<8> f; }
struct headers_t { eth_t eth; H h; }
struct meta_t { }

control InnerType(inout headers_t hdr);
control Inner(inout headers_t hdr) {
    apply {
        hdr.h.setValid();
        hdr.h.f = 1;
    }
}

parser prs(packet_in pkt, out headers_t hdr, inout meta_t meta, inout standard_metadata_t std) {
    state start {
        pkt.extract(hdr.eth);
        transition accept;
    }
}
control vfy(inout headers_t hdr, inout meta_t meta) { apply { } }
control Outer(inout headers_t hdr, inout meta_t meta, inout standard_metadata_t std)(InnerType inner) {
    apply {
        inner.apply(hdr);
        std.egress_spec = 1;
    }
}
control egress(inout headers_t hdr, inout meta_t meta, inout standard_metadata_t std) { apply { } }
control cmp(inout headers_t hdr, inout meta_t meta) { apply { } }
control dep(packet_out pkt, in headers_t hdr) { apply { pkt.emit(hdr.eth); pkt.emit(hdr.h); } }
V1Switch(prs(), vfy(), Outer(Inner()), egress(), cmp(), dep()) main;
