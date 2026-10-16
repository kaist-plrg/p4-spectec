#include <core.p4>
#include <v1model.p4>
header eth_t { bit<48> dst; bit<48> src; bit<16> ty; }
struct headers_t { eth_t eth; }
struct meta_t { }
header byte_t { bit<8> value; }
struct pair_t { byte_t item; bit<8> tag; }
parser prs(packet_in pkt, out headers_t hdr, inout meta_t meta, inout standard_metadata_t std) {
    state start { pkt.extract(hdr.eth); transition accept; }
}
control vfy(inout headers_t hdr, inout meta_t meta) { apply { } }
control ingress(inout headers_t hdr, inout meta_t meta, inout standard_metadata_t std) {
    apply {
        pair_t pair = hdr.eth.isValid() ? { {#}, 0xA1 } : { {#}, 8w0 };
        hdr.eth.ty = 16w0;
        hdr.eth.ty[7:0] = pair.tag;
        if (!pair.item.isValid()) { hdr.eth.ty[15:8] = 8w0xB2; }
        std.egress_spec = 1;
    }
}
control egress(inout headers_t hdr, inout meta_t meta, inout standard_metadata_t std) { apply { } }
control cmp(inout headers_t hdr, inout meta_t meta) { apply { } }
control dep(packet_out pkt, in headers_t hdr) { apply { pkt.emit(hdr.eth); } }
V1Switch(prs(), vfy(), ingress(), egress(), cmp(), dep()) main;
