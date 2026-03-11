/*
Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0

Negative test: directionless parameters of non-action callables
(controls, parsers, functions) must be compile-time known (CTK).
Per P4-16 spec Section 18.1, passing a runtime value to a
directionless control parameter should be rejected.
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

// inner control has a directionless parameter
// callers MUST supply a compile-time known value
control inner(inout headers_t hdr,
              inout metadata_t meta,
              inout standard_metadata_t stdmeta,
              bit<9> port) {
    apply {
        stdmeta.egress_spec = port;
    }
}

control ingressImpl(inout headers_t hdr,
                    inout metadata_t meta,
                    inout standard_metadata_t stdmeta) {
    inner() i;

    apply {
        // hdr.ethernet.srcAddr[8:0] is a runtime value - NOT CTK
        // passing it to directionless control param should be REJECTED
        i.apply(hdr, meta, stdmeta, hdr.ethernet.srcAddr[8:0]);
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
