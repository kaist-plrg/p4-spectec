#include <core.p4>
#include <v1model.p4>
header eth_t { bit<48> dst; bit<48> src; bit<16> ty; }
struct headers_t { eth_t eth; }
struct meta_t { }
parser prs(packet_in pkt, out headers_t hdr, inout meta_t meta, inout standard_metadata_t std) {
    state start {
        pkt.extract(hdr.eth);
        hdr.eth.ty = 16w0;
        transition select((tuple<bit<8>, bit<8>>){8w0, 8w1}) {
            {8w0, 8w2}: s_bad;
            {8w0, 8w1}: s_seq;
            default: s_bad;
        }
    }
    state s_seq {
        hdr.eth.ty[15:8] = 8w0xA1;
        transition select((tuple<bit<8>, bit<8>>){8w2, 8w3}) {
            ((tuple<bit<8>, bit<8>>){8w4, 8w5}): s_bad;
            ((tuple<bit<8>, bit<8>>){8w2, 8w3}): s_tuple;
            default: s_bad;
        }
    }
    state s_tuple {
        hdr.eth.ty[7:0] = 8w0xB2;
        transition accept;
    }
    state s_bad {
        hdr.eth.ty = 16w0xDEAD;
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
