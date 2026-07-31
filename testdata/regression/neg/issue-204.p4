#include <core.p4>
header eth_t { bit<48> dst; }
type bit<8> Byte;
header_union U { eth_t h; Byte x; }   // scalar (newtype) field in a header union
