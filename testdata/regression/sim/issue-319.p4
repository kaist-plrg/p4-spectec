#include <core.p4>
#include <v1model.p4>
header eth_t { bit<48> dst; bit<48> src; bit<16> ty; }
struct headers_t { eth_t eth; }
struct meta_t { }
header_union empty_t { }
struct wrap_t { empty_t u; }
parser prs(packet_in pkt, out headers_t hdr, inout meta_t meta, inout standard_metadata_t std) {
    state start { pkt.extract(hdr.eth); transition accept; }
}
control vfy(inout headers_t hdr, inout meta_t meta) { apply { } }
control ingress(inout headers_t hdr, inout meta_t meta, inout standard_metadata_t std) {
    apply {
        empty_t e;
        wrap_t s;
        hdr.eth.ty = 16w0;
        if (e.minSizeInBits() == 0 && e.maxSizeInBits() == 0) { hdr.eth.ty[7:0] = 8w0xA1; }
        if (e.minSizeInBytes() == 0 && e.maxSizeInBytes() == 0
            && s.minSizeInBits() == 0 && s.maxSizeInBits() == 0) { hdr.eth.ty[15:8] = 8w0xB2; }
        std.egress_spec = 1;
    }
}
control egress(inout headers_t hdr, inout meta_t meta, inout standard_metadata_t std) { apply { } }
control cmp(inout headers_t hdr, inout meta_t meta) { apply { } }
control dep(packet_out pkt, in headers_t hdr) { apply { pkt.emit(hdr.eth); } }
V1Switch(prs(), vfy(), ingress(), egress(), cmp(), dep()) main;
