#include <core.p4>
#include <v1model.p4>
header eth_t { bit<48> dst; bit<48> src; bit<16> ty; }
struct headers_t { eth_t eth; }
struct meta_t { }
parser prs(packet_in pkt, out headers_t hdr, inout meta_t meta, inout standard_metadata_t std) {
    state start { pkt.extract(hdr.eth); transition accept; }
}
control vfy(inout headers_t hdr, inout meta_t meta) { apply { } }
control ingress(inout headers_t hdr, inout meta_t meta, inout standard_metadata_t std) {
    action a() { std.egress_spec = 1; }
    action b() { std.egress_spec = 2; }
    table t {
        key = { hdr.eth.dst : ternary; }
        actions = { a; b; }
        default_action = a();
        const entries = {
            48w0 &&& 48w0 : a();
            48w0 &&& 48w0 : b();
        }
        largest_priority_wins = true;
        largest_priority_wins = false;
    }
    apply { t.apply(); }
}
control egress(inout headers_t hdr, inout meta_t meta, inout standard_metadata_t std) { apply { } }
control cmp(inout headers_t hdr, inout meta_t meta) { apply { } }
control dep(packet_out pkt, in headers_t hdr) { apply { pkt.emit(hdr.eth); } }
V1Switch(prs(), vfy(), ingress(), egress(), cmp(), dep()) main;
