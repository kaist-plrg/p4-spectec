#include <core.p4>
#include <v1model.p4>

// v1model arch: checksum verify/update, hash, table apply, mark_to_drop, for-loop, runtime ops.

header eth_t { bit<48> dst; bit<48> src; bit<16> ty; }
header ipv4_t {
    bit<4>  version; bit<4>  ihl; bit<8>  diffserv; bit<16> totalLen;
    bit<16> id;      bit<3>  flags; bit<13> frag;   bit<8>  ttl;
    bit<8>  proto;   bit<16> hdrChecksum; bit<32> src; bit<32> dst;
}
struct headers_t { eth_t eth; ipv4_t ipv4; }
struct meta_t { bit<16> h; }

parser prs(packet_in pkt, out headers_t hdr, inout meta_t meta,
           inout standard_metadata_t std) {
    state start {
        pkt.extract(hdr.eth);
        transition select(hdr.eth.ty) {
            16w0x0800: ip;
            default: accept;
        }
    }
    state ip { pkt.extract(hdr.ipv4); transition accept; }
}

control vfy(inout headers_t hdr, inout meta_t meta) {
    apply {
        verify_checksum(hdr.ipv4.isValid(),
            { hdr.ipv4.version, hdr.ipv4.ihl, hdr.ipv4.diffserv, hdr.ipv4.totalLen,
              hdr.ipv4.id, hdr.ipv4.flags, hdr.ipv4.frag, hdr.ipv4.ttl, hdr.ipv4.proto,
              hdr.ipv4.src, hdr.ipv4.dst },
            hdr.ipv4.hdrChecksum, HashAlgorithm.csum16);
    }
}

control ingress(inout headers_t hdr, inout meta_t meta, inout standard_metadata_t std) {
    action fwd(bit<9> port) { std.egress_spec = port; }
    action drop() { mark_to_drop(std); }
    table t {
        key = { hdr.eth.dst : exact; }
        actions = { fwd; drop; }
        const entries = { 48w0xFFFFFFFFFFFF : drop(); }
        default_action = fwd(1);
    }
    apply {
        bit<16> acc = 0;
        for (bit<4> i in 0 .. 7) { acc = acc + (bit<16>)i; }
        hdr.eth.src[15:0] = (acc ^ (hdr.eth.src[31:16] & 16w0x00FF)) | (hdr.eth.ty << 1);
        hash(meta.h, HashAlgorithm.crc16, 16w0, { hdr.eth.src, hdr.eth.dst }, 32w65536);
        t.apply();
    }
}

control egress(inout headers_t hdr, inout meta_t meta, inout standard_metadata_t std) {
    apply { }
}

control cmp(inout headers_t hdr, inout meta_t meta) {
    apply {
        update_checksum(hdr.ipv4.isValid(),
            { hdr.ipv4.version, hdr.ipv4.ihl, hdr.ipv4.diffserv, hdr.ipv4.totalLen,
              hdr.ipv4.id, hdr.ipv4.flags, hdr.ipv4.frag, hdr.ipv4.ttl, hdr.ipv4.proto,
              hdr.ipv4.src, hdr.ipv4.dst },
            hdr.ipv4.hdrChecksum, HashAlgorithm.csum16);
    }
}

control dep(packet_out pkt, in headers_t hdr) {
    apply { pkt.emit(hdr.eth); pkt.emit(hdr.ipv4); }
}

V1Switch(prs(), vfy(), ingress(), egress(), cmp(), dep()) main;
