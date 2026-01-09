/*
Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/

#include <core.p4>
#include <v1model.p4>

header ethernet_t {
    bit<48> dstAddr;
    bit<48> srcAddr;
    bit<16> etherType;
}

struct metadata_t {
}

struct headers_t {
    ethernet_t ethernet;
}

parser parserImpl(packet_in packet,
                  out headers_t hdr,
                  inout metadata_t meta,
                  inout standard_metadata_t stdmeta)
{
    state start {
        packet.extract(hdr.ethernet);
        transition accept;
    }
}

bit<32> foo()
{
    bit <32> n = 0;
    for (bit<32> i = 0; i < 10; i = i + 1) {
        n = n + i;
    }
    return n;
}

control ingressImpl(inout headers_t hdr,
                    inout metadata_t meta,
                    inout standard_metadata_t stdmeta)
{
    bit<8> n = hdr.ethernet.srcAddr[15:8];
    bit<8> i;
    apply {
        for (i = 0; i < 5; i = i + 1) {
            n = (bit<8>) foo();
        }
        for (i = 0; i < 5; i = i + 1) {
            for (bit<8> j = 0; j < 5; j = j + 1) {
                n = (bit<8>) foo();
            }
        }
        hdr.ethernet.srcAddr[7:0] = (bit<8>) i;
        hdr.ethernet.srcAddr[15:8] = n;
        stdmeta.egress_spec = 1;
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
        packet.emit(hdr.ethernet);
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
