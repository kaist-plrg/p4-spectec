#include <ebpf_model.p4>
#include <core.p4>

// ebpf arch: ebpfFilter pipeline, extract, select, validity, CounterArray, array_table.

header Ethernet_h {
    bit<48> dstAddr;
    bit<48> srcAddr;
    bit<16> etherType;
}

header IPv4_h {
    bit<4>  version;
    bit<4>  ihl;
    bit<8>  diffserv;
    bit<16> totalLen;
    bit<16> identification;
    bit<3>  flags;
    bit<13> fragOffset;
    bit<8>  ttl;
    bit<8>  protocol;
    bit<16> hdrChecksum;
    bit<32> srcAddr;
    bit<32> dstAddr;
}

struct Headers_t {
    Ethernet_h ethernet;
    IPv4_h     ipv4;
}

parser prs(packet_in p, out Headers_t headers) {
    state start {
        p.extract(headers.ethernet);
        transition select(headers.ethernet.etherType) {
            16w0x800 : ip;
            default  : reject;
        }
    }
    state ip {
        p.extract(headers.ipv4);
        transition accept;
    }
}

control pipe(inout Headers_t headers, out bool pass) {
    CounterArray(32w10, true) counters;

    action invalidate() {
        headers.ipv4.setInvalid();
        headers.ethernet.setInvalid();
    }
    table t {
        actions = { invalidate; }
        implementation = array_table(1);
    }

    apply {
        if (headers.ipv4.isValid()) {
            counters.increment((bit<32>)headers.ipv4.dstAddr);
            pass = true;
        } else {
            t.apply();
            pass = false;
        }
    }
}

ebpfFilter(prs(), pipe()) main;
