#include <core.p4>

// Advanced types: newtype, serializable enum, header_union.

type bit<32> id_t;
enum bit<8> EthType { IPV4 = 0x08, ARP = 0x06 }

header h8 { bit<8> v; }
header h16 { bit<16> v; }
header_union hu_t { h8 a; h16 b; }

const id_t MYID = (id_t) 32w7;
const EthType ET = EthType.IPV4;

control U(inout hu_t u) {
    apply {
        u.a.setValid();
        u.a.v = 8w1;
        bit<8> e = (bit<8>) ET;
        id_t x = MYID;
    }
}
