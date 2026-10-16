#include <core.p4>
#include <v1model.p4>
header eth_t { bit<48> dst; bit<48> src; bit<16> ty; }
header byte_t { bit<8> value; }
header word_t { bit<16> value; }
header_union choice_t { byte_t item; word_t other; }
struct wrapped_t { choice_t choice; }
struct headers_t { eth_t eth; }
struct meta_t { }
parser prs(packet_in pkt, out headers_t hdr, inout meta_t meta, inout standard_metadata_t std) {
    state start { pkt.extract(hdr.eth); transition accept; }
}
control vfy(inout headers_t hdr, inout meta_t meta) { apply { } }
control ingress(inout headers_t hdr, inout meta_t meta, inout standard_metadata_t std) {
    apply {
        choice_t original;
        original.item.setValid();
        original.item.value = 8w0xAB;
        choice_t copied = (choice_t) original;
        wrapped_t wrapped = {original};
        hdr.eth.ty[15:8] = copied.item.value;
        hdr.eth.ty[7:0] = wrapped.choice.item.value;
        std.egress_spec = 1;
    }
}
control egress(inout headers_t hdr, inout meta_t meta, inout standard_metadata_t std) { apply { } }
control cmp(inout headers_t hdr, inout meta_t meta) { apply { } }
control dep(packet_out pkt, in headers_t hdr) { apply { pkt.emit(hdr.eth); } }
V1Switch(prs(), vfy(), ingress(), egress(), cmp(), dep()) main;
