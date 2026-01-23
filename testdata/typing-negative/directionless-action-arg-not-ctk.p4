
/*
Test case for P4-SpecTec issue #37:
Directionless parameters of actions are NOT compile-time known.

This program MUST be REJECTED by P4-SpecTec.

The bug: P4-SpecTec was incorrectly treating all directionless parameters
as compile-time known (CTK), when per P4-16 spec §18.1, directionless
parameters of actions are control-plane supplied (DYN).

The extern method `count(bit<8> idx)` has a directionless parameter `idx`,
which requires a compile-time known argument (CTK).

The action `a1(bit<8> idx)` has a directionless parameter `idx`, which
is control-plane supplied (DYN).

When `a1` calls `ctr.count(idx)`, it passes a DYN value to a CTK parameter,
which is a type error per the P4-16 specification.

References:
- P4C issue: p4lang/p4c#5405
- P4-SpecTec issue: kaist-plrg/p4-spectec#37
*/

#include <core.p4>
#include <v1model.p4>

// Extern with a directionless parameter (requires CTK argument)
extern Counter {
    Counter(bit<32> size);
    void count(bit<8> idx);  // idx is directionless → caller MUST pass CTK
}

header hdr_t {
    bit<8> f;
}

struct metadata_t {
}

struct headers_t {
    hdr_t head;
}

parser parserImpl(packet_in packet,
                  out headers_t hdr,
                  inout metadata_t meta,
                  inout standard_metadata_t stdmeta)
{
    state start {
        packet.extract(hdr.head);
        transition accept;
    }
}

control ingressImpl(inout headers_t hdr,
                    inout metadata_t meta,
                    inout standard_metadata_t stdmeta)
{
    Counter(256) ctr;

    // a0 is OK: passes compile-time constant (LCTK) to extern method
    action a0() {
        ctr.count(0);  // ✓ 0 is LCTK → OK
    }

    // a1 is INVALID: passes control-plane parameter (DYN) to CTK extern parameter
    action a1(bit<8> idx) {
        ctr.count(idx);  // ❌ idx is DYN, but count() requires CTK → ERROR
    }

    table t {
        key = { hdr.head.f : exact; }
        actions = { a0; a1; }
        default_action = a0();
    }

    apply {
        t.apply();
    }
}

control egressImpl(inout headers_t hdr,
                   inout metadata_t meta,
                   inout standard_metadata_t stdmeta)
{
    apply {
    }
}

control deparserImpl(packet_out packet,
                     in headers_t hdr)
{
    apply {
        packet.emit(hdr.head);
    }
}

control verifyChecksum(inout headers_t hdr, inout metadata_t meta) {
    apply {
    }
}

control updateChecksum(inout headers_t hdr, inout metadata_t meta) {
    apply {
    }
}

V1Switch(parserImpl(),
         verifyChecksum(),
         ingressImpl(),
         egressImpl(),
         updateChecksum(),
         deparserImpl()) main;
