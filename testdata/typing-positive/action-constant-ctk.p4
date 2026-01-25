/*
Test case for P4-SpecTec issue #37 (positive/passing case):
Actions with compile-time constant arguments ARE allowed.

This program MUST be ACCEPTED by P4-SpecTec.

This test ensures that the fix for issue #37 does not regress the
valid case where an action calls an extern method with a compile-time
constant (literal) value.

The extern method `count(bit<8> idx)` requires a compile-time known argument.
The action `a0` passes literal 0, 1, 2, 3 which are LCTK (local compile-time known).
This is valid and must be accepted.

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

    // All these actions are OK: they pass compile-time constants (LCTK)
    action a0() {
        ctr.count(0);  // ✓ 0 is LCTK → OK
    }

    action a1() {
        ctr.count(1);  // ✓ 1 is LCTK → OK
    }

    action a2() {
        ctr.count(2);  // ✓ 2 is LCTK → OK
    }

    action a3() {
        ctr.count(3);  // ✓ 3 is LCTK → OK
    }

    table t {
        key = { hdr.head.f : exact; }
        actions = { a0; a1; a2; a3; }
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
