/*
Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0

Positive test: directionless parameters of actions (DYN) should
still accept compile-time known values. DYN means "accepts anything"
not "rejects constants". This test verifies our fix does not break
the case where a compile-time constant is passed to an action param.
*/
#include <core.p4>
#include <v1model.p4>

header ethernet_t {
    bit<48> dstAddr;
    bit<48> srcAddr;
    bit<16> etherType;
}

struct metadata_t { }

struct headers_t {
    ethernet_t ethernet;
}

parser parserImpl(packet_in packet,
                  out headers_t hdr,
                  inout metadata_t meta,
                  inout standard_metadata_t stdmeta) {
    state start {
        packet.extract(hdr.ethernet);
        transition accept;
    }
}

control ingressImpl(inout headers_t hdr,
                    inout metadata_t meta,
                    inout standard_metadata_t stdmeta) {
    // directionless param on action
    action set_port(bit<9> port) {
        stdmeta.egress_spec = port;
    }

    table t {
        key = { hdr.ethernet.etherType : exact; }
        actions = { set_port; }
        // compile-time constant passed to directionless action param
        // DYN accepts both runtime AND compile-time values - should PASS
        const default_action = set_port(9w1);
    }

    apply {
        t.apply();
    }
}

control egressImpl(inout headers_t hdr,
                   inout metadata_t meta,
                   inout standard_metadata_t stdmeta) {
    apply { }
}

control deparserImpl(packet_out packet, in headers_t hdr) {
    apply {
        packet.emit(hdr.ethernet);
    }
}

control verifyChecksum(inout headers_t hdr, inout metadata_t meta) {
    apply { }
}

control updateChecksum(inout headers_t hdr, inout metadata_t meta) {
    apply { }
}

V1Switch(parserImpl(),
         verifyChecksum(),
         ingressImpl(),
         egressImpl(),
         updateChecksum(),
         deparserImpl()) main;
