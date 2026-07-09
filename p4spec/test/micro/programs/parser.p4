#include <core.p4>

// Parser: states, extract, header-stack .next iteration, tuple select (exact/mask/range/wildcard).

header eth_t { bit<48> dst; bit<48> src; bit<16> ty; }
header lbl_t { bit<7> id; bit<1> bos; }
struct hdrs { eth_t eth; lbl_t[4] labels; }

parser P(packet_in pkt, out hdrs h) {
    state start {
        pkt.extract(h.eth);
        transition select(h.eth.ty, h.eth.src[7:0]) {
            (16w0x0800, 8w0): parse_ip;
            (16w0x0806 &&& 16w0xFFFF, _): parse_arp;
            (16w0 .. 16w15, _): parse_low;
            default: parse_labels;
        }
    }
    state parse_labels {
        pkt.extract(h.labels.next);
        transition select(h.labels.last.bos) { 1w1: accept; default: parse_labels; }
    }
    state parse_ip { transition accept; }
    state parse_arp { transition accept; }
    state parse_low { transition reject; }
}
