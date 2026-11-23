#include <core.p4>
#include <v1model.p4>
header eth_t { bit<48> dst; bit<48> src; bit<16> ty; }
struct headers_t { eth_t eth; }
struct meta_t { }
typedef bool flag_t;
type flag_t wrapped_flag_t;
struct wrapped_t { wrapped_flag_t flag; }
parser prs(packet_in pkt, out headers_t hdr, inout meta_t meta, inout standard_metadata_t std) {
    state start {
        pkt.extract(hdr.eth);
        transition select((wrapped_flag_t) true) {
            ((wrapped_flag_t) false): s_bad;
            ((wrapped_flag_t) true): accept;
            default: s_bad;
        }
    }
    state s_bad { hdr.eth.dst = 48w0xDEAD; transition accept; }
}
control vfy(inout headers_t hdr, inout meta_t meta) { apply { } }
control ingress(inout headers_t hdr, inout meta_t meta, inout standard_metadata_t std) {
    apply {
        wrapped_t wrapped = (wrapped_t) {false};
        wrapped_flag_t direct = (wrapped_flag_t) true;
        hdr.eth.ty = 16w0;
        if (!(bool) wrapped.flag) { hdr.eth.ty[7:0] = 8w0xA1; }
        if ((bool) direct) { hdr.eth.ty[15:8] = 8w0xB2; }
        std.egress_spec = 1;
    }
}
control egress(inout headers_t hdr, inout meta_t meta, inout standard_metadata_t std) { apply { } }
control cmp(inout headers_t hdr, inout meta_t meta) { apply { } }
control dep(packet_out pkt, in headers_t hdr) { apply { pkt.emit(hdr.eth); } }
V1Switch(prs(), vfy(), ingress(), egress(), cmp(), dep()) main;
