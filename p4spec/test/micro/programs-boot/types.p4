#include <core.p4>

// Types: typedef, header, struct, header stack, enum, literals.

typedef bit<48> mac_t;

header eth_t { mac_t dst; mac_t src; bit<16> ethType; }
header vlan_t { bit<3> pcp; bit<1> dei; bit<12> vid; bit<16> proto; }

struct meta_t { bit<8> a; bool b; }
struct headers_t {
    eth_t eth;
    vlan_t[4] vlans;
}

enum Color { RED, GREEN, BLUE }

const Color C0 = Color.RED;
const meta_t M0 = { a = 8w7, b = true };
