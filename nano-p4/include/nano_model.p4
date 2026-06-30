#ifndef _NANO_MODEL_P4_
#define _NANO_MODEL_P4_

#include <nano_core.p4>

parser parse(packet_in packet, out Header hdr);
control filter(inout Header hdr, out bool accept);

package NanoSwitch(parse p, filter f);

#endif /* _NANO_MODEL_P4_ */
