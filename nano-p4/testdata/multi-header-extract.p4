#include <nano_model.p4>

header Ethernet {
    bit<48> dst;
    bit<48> src;
    bit<16> ethertype;
}

header IPv4 {
    bit<4>  version;
    bit<4>  ihl;
    bit<8>  diffserv;
    bit<16> total_len;
    bit<16> id;
    bit<3>  flags;
    bit<13> frag_offset;
    bit<8>  ttl;
    bit<8>  protocol;
    bit<16> checksum;
    bit<32> src_addr;
    bit<32> dst_addr;
}

struct Header {
    Ethernet eth;
    IPv4     ipv4;
}

parser Parser(packet_in pkt, out Header hdr) {
    state start {
        pkt.extract<Ethernet>(hdr.eth);
        transition select(hdr.eth.ethertype) {
            0x0800 : parse_ipv4;
            0xFFFF : reject;
        }
    }

    state parse_ipv4 {
        pkt.extract<IPv4>(hdr.ipv4);
        transition accept;
    }
}

control Filter(inout Header hdr, out bool pass) {
    apply {
        pass = hdr.ipv4.ttl > 8w0;
    }
}

NanoSwitch(Parser(), Filter()) main;
