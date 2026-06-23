#ifndef _NANO_MODEL_P4_
#define _NANO_MODEL_P4_

#include <nano_core.p4>

parser parse<H>(packet_in packet, out H headers);
control filter<H>(inout H headers, out bool accept);

package NanoSwitch<H>(parse<H> parse, filter<H> filter);

#endif /* _NANO_MODEL_P4_ */
