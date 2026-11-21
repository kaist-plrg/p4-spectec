# P4 Simulator

> **Disclaimer:** This is not intended as a complete or faithful implementation of a P4 target such as bmv2. It serves as a harness to run the P4 dynamic semantics of the P4-SpecTec specification on STF packet tests.

Runs STF packet tests on a P4 program, with the P4-SpecTec specification as the P4 interpreter.

```shell
./bin/p4spectec sim spec --arch v1model -i p4c/p4include \
  -p p4c/testdata/p4_16_samples/basic_routing-bmv2.p4 \
  --stf testdata/p4testgen/basic_routing-bmv2/basic_routing-bmv2_1.stf

make test-sim-sl    # every STF test, on the SL interpreter (also -al, -pl)
```

Architectures: `v1model`, `psa`, `ebpf`.

## 1. How it works (v1model)

```
simulator                                 specification
fixed-function blocks, in Rust            programmable blocks, in P4

Rx ─────────────────────────────────────▶ Parser
                                          Verify checksum
Traffic manager ◀──────────────────────── Ingress
  drop, clone, multicast,
  resubmit, recirculate ────────────────▶ Egress
                                          Update checksum
Tx ◀───────────────────────────────────── Deparser

Externs ◀ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ extern calls from any block,
  packet_in, counter, register, ...       such as pkt.extract(hdr.ethernet)
```

- Simulator: `arch/<arch>/target/` (traffic manager), `arch/core/` and `arch/<arch>/externs/` (externs). See [§4](#4-inside-an-architecture).
- Specification: the P4-SpecTec interpreter, evaluating the program's parsers and controls.
- `stf/` sends packets to Rx and matches what leaves Tx against `expect` lines.

### 1.1. Evaluation context and architecture state

Every call between the simulator and the specification carries these two values.

```
;; spec/8-dynamic/8.01.1-eval-context.watsup
syntax evalContext =
  { GLOBAL globalEvalLayer,
    BLOCK blockEvalLayer,
    LOCAL localEvalLayer }

;; spec/8-dynamic/8.01.2-eval-arch.watsup
extern syntax archState
syntax arch =
  { STATE archState,
    STORE store }
```

```rust
// arch/state.rs
pub struct SimState {
    pub value_ctx: SpecValue,   // evalContext
    pub value_arch: SpecValue,  // arch
    ...
}

// arch/v1model/target/state.rs: v1model's archState
pub struct ArchState {
    pub mirrortable: mirror::Table,
    pub multicast: multicast::State,
    ...
}

sim.get_global("standard_metadata.egress_spec")  // a variable in evalContext
sim.get_arch_state()                              // arch.STATE, as ArchState
sim.get_object("packet_in")                       // an object in arch.STORE, as ObjectState
```

- **Evaluation context**: the P4 variables in scope and their values, such as `hdr` and `standard_metadata`, in global, block, and local layers.
- **Architecture state**: `STORE` holds the program's objects, including extern objects such as a register or `packet_in`. `STATE` is the switch's own state, such as mirror sessions; the specification leaves its form to the architecture.
- The simulator holds both as handles into the specification and reads them through `sim` and `env` ([§3](#3-the-p4-level-api)).

### 1.2. Example: forwarding one packet

The ingress control of the P4 program:

```p4
control ingress(inout headers hdr, inout metadata meta,
                inout standard_metadata_t standard_metadata) {
    action set_port(bit<9> port) {
        standard_metadata.egress_spec = port;
    }
    table fwd {
        key = { hdr.ethernet.dstAddr : exact; }
        actions = { set_port; }
    }
    apply { fwd.apply(); }
}
```

The STF test: `add` installs a table entry, `packet` sends a packet to a port, and `expect` gives the packet that should leave a port.

```
add ingress.fwd hdr.ethernet.dstAddr:0x000000000002 ingress.set_port(port:1)
packet 0 000000000002 000000000001 0800
expect 1 000000000002 000000000001 0800
```

1. `stf/` parses the P4 program, and the simulator instantiates it. Extern objects such as counters and registers are constructed here.
2. `stf/` adds the entry to the table `fwd`.
3. `stf/` sends the packet to port 0.
4. The simulator installs the packet as `packet_in` and sets `standard_metadata.ingress_port` to 0.
5. The simulator runs the parser ([§4.1](#41-calling-a-p4-block)).
6. The specification evaluates the parser and calls `packet_in.extract(hdr.ethernet)`, which `arch/core/` implements ([§4.2](#42-handling-an-extern-call)).
7. The simulator runs ingress. The specification applies `fwd`, and `set_port` sets `standard_metadata.egress_spec` to 1.
8. The simulator reads `egress_spec`, then runs egress and the deparser.
9. The packet leaves on port 1, and `stf/` matches it against the `expect` line.

## 2. Where things are

```
sim/src/
├── lib.rs     build(): picks an architecture by name, returns a Simulator
├── stf/       STF tests: parse, run, match expect lines
├── arch/         §4
│   ├── core/      core.p4 externs: packet_in, packet_out, verify, static_assert
│   ├── v1model/
│   ├── psa/
│   └── ebpf/
├── p4/        the P4-level API that arch/ and stf/ are written in (§3)
└── spec/      the only code that reads or builds a specification value
```

| To change | Look in |
|---|---|
| How a packet moves between blocks | `arch/<arch>/target/pipe.rs` |
| What an extern does | `arch/<arch>/externs/` (`arch/core/` for `packet_in`, `packet_out`) |
| A control-plane command: table entry, mirror session, multicast group | `arch/<arch>/target/runtime.rs` |
| STF syntax | `stf/syntax/` |
| How STF lines run and `expect` lines match | `stf/run/` |
| How a P4 value is read from or written to the specification | `spec/bridge.rs` |
| How an extern call from the specification is read and answered | `spec/entry.rs` |

## 3. The P4-level API

Setting `standard_metadata.egress_spec` to 511 from an extern, in the specification's terms:

```
;; build the value 9 W 511
;; build the storage reference (_BARE "standard_metadata") '.' "egress_spec"
;; write it, threading the evaluation context and the architecture state
Lvalue_write: LOCAL EC ARCH |- storageReference := value : EC'
```

With the P4-level API:

```rust
env.set_local("standard_metadata.egress_spec", P4Value::Bit { width: 9, int: 511.into() })?;
```

- Values are `P4Value`s, not specification values.
- The evaluation context and architecture state stay inside `env` and `sim`.

| Type | Is | Used as |
|---|---|---|
| `P4Value` | A P4 value | `P4Value::Bit { width: 9, int: 511.into() }` |
| `ExternEnv` | An extern call's view of its arguments and type arguments | `env.get_local("index")`, `env.set_local("result", ..)`, `env.get_type("T")` |
| `CallResult` | What an extern returns | `Return(value)`, or `Reject(error)` from a parser |
| `InitEnv` | The constructor arguments of an extern object, such as `size` of `counter(size, type)` | `env.get("size")` |
| `Sim` | What runs P4 blocks and holds the tables | `sim.run_block(..)`, `sim.get_global(..)`, `sim.get_arch_state()`, `sim.add_entry(..)` |
| `Architecture` | What an architecture is: its state types, extern dispatch, and pipeline | `call_function`, `call_method`, `init_object`, `drive_pipe` |
| `RuntimeInterface` | The control plane, after bmv2's | `mc_mgrp_create`, `mirroring_add_session`, `register_read` |

## 4. Inside an architecture

```
arch/v1model/
├── mod.rs         the architecture: extern objects (ObjectState), constants
├── types.rs       enums from v1model.p4: CounterType, HashAlgorithm, ...
├── externs/       what v1model.p4 declares
│   ├── mod.rs         `impl Architecture`: extern name and parameter names → Rust function (§4.2)
│   ├── func.rs        mark_to_drop, hash, clone_preserving_field_list, ...
│   └── object/        counter, register, direct_counter, direct_meter
└── target/        the switch behind the externs
    ├── pipe.rs        runs a packet through the blocks (§4.1)
    ├── state.rs       the architecture state: mirror sessions, multicast groups, ...
    ├── packet.rs      clone requests and saved packet contexts
    ├── mirror.rs      mirror sessions
    ├── multicast.rs   multicast groups
    └── runtime.rs     control plane: table entries, groups, sessions, registers
```

`psa/` has the same shape. `ebpf/` has only `externs/` and `target/{pipe,runtime}.rs`.

- `externs/` is what the P4 program calls; `target/` is what it cannot see: mirror sessions, multicast groups.
- The control plane configures `target/` through `runtime.rs`.

### 4.1. Calling a P4 block

```rust
// arch/v1model/target/pipe.rs, in parse_and_verify
// Parser errors are visible to the controls
if let BlockResult::Reject(error) = sim.run_block("V1Model_parser")? {
    sim.set_global("standard_metadata.parser_error", P4Value::Error(error))?;
}
```

```
;; spec/9-arch/9.A-v1model.watsup
relation V1Model_parser:
  evalContext arch |- evalContext arch transitionResult
```

- `sim.run_block("V1Model_parser")` runs this relation: it takes the evaluation context and the architecture state, and returns both, updated, with how the parser ended.
- A parser that rejects returns its error, which the simulator writes to `standard_metadata.parser_error` for the controls to read.

### 4.2. Handling an extern call

```
;; spec/8-dynamic/8.02-evaluation-relation.watsup
extern relation ExternMethodCall_eval:
  evalContext arch |- objectId '.' nameIR `( nameIR* `)
                    : evalContext arch callResult

;; spec/8-dynamic/8.10.4-eval-call.watsup
    ---- ;; copy-in
    ...
    -- ExternMethodCall_eval:
        EC_callee_5 ARCH_1 |- objectId '.' nameIR `( nameIR_param* `)
                           : EC_callee_6 ARCH_2 rejectTransitionResult
```

```rust
// arch/v1model/externs/mod.rs
ObjectState::PacketIn(object) => {
    sim.run_extern(call, |env| object.call(env, name, &params))?
}
```

```rust
// arch/core/object/packet_in.rs
("extract", ["hdr"]) => self.extract(env),

pub fn extract(&mut self, env: &mut ExternEnv) -> Result<CallResult, ExternError> {
    // The header size comes from the instantiated type argument
    let typ = env.get_type("T")?;
    let size = env.max_size_in_bits(&typ)?;
    // Too few bits: reject with `PacketTooShort`
    if !self.has_size(size)? {
        return Ok(CallResult::Reject("PacketTooShort".to_owned()));
    }
    // Take the header's bits and advance the cursor past them
    let (packet_in, bits) = self.parse(size)?;
    *self = packet_in;
    // Fill the header from the bits and write it to the `out` argument
    let hdr = env.get_local("hdr")?;
    let hdr = env.from_bits(hdr, 0, &bits)?;
    env.set_local("hdr", hdr)?;
    Ok(CallResult::Return(None))
}
```

- `ExternMethodCall_eval` is an `extern relation`: the specification declares it, and the simulator implements it.
- `spec/entry.rs` is the only code that receives it: it reads the values into a `MethodCall` and hands it to the architecture's `call_method`.
- The specification calls it after copy-in, with the callee's evaluation context, the object, the method name, and its parameter names.
- The simulator finds the object's state: `packet_in` holds the packet's bits and a cursor. The object matches the method by name and parameter names, since P4 overloads externs by parameter names.
- `extract` reads the type argument `T`, takes that many bits, and writes the header to `hdr`. Copy-out returns it to the caller.
- The `CallResult` goes back to the specification: `Return`, or `Reject` with `PacketTooShort`, which ends the parser.
- An extern function such as `mark_to_drop` takes the same path through `ExternFunctionCall_eval`.

## 5. Adding an extern

### 5.1. A function

1. Write it in `externs/func.rs`, taking `&mut ExternEnv`.

   ```rust
   // arch/v1model/externs/func.rs
   pub fn mark_to_drop(env: &mut ExternEnv) -> Result<CallResult, ExternError> {
       let port_drop = P4Value::Bit { width: PORT_WIDTH, int: DEFAULT_DROP_PORT.into() };
       env.set_local("standard_metadata.egress_spec", port_drop)?;
       env.set_local("standard_metadata.mcast_grp", P4Value::Bit { width: 16, int: 0.into() })?;
       Ok(CallResult::Return(None))
   }
   ```

2. Add an arm to `call_function` in `externs/mod.rs`.

   ```rust
   // arch/v1model/externs/mod.rs, in call_function
   ("mark_to_drop", ["standard_metadata"]) => {
       sim.run_extern(call, v1model_func::mark_to_drop)?
   }
   ```

### 5.2. An object

1. Write it in `externs/object/`, with `init(&mut InitEnv)` and `call(env, name, params)`.

   ```rust
   // arch/v1model/externs/object/register.rs
   pub struct Register {
       pub typ: P4Type,
       pub values: Vec<P4Value>,
   }

   impl Register {
       pub fn init(env: &mut InitEnv) -> Result<Self, ExternError> {
           let typ = ...; // the one type argument, T
           let size: usize = env.get("size")?.try_into()?;
           let initial = env.default_of(&typ)?;
           Ok(Self { typ, values: vec![initial; size] })
       }

       pub fn call(&mut self, env: &mut ExternEnv, name: &str, params: &[&str])
           -> Result<CallResult, ExternError> {
           match (name, params) {
               ("read", ["result", "index"]) => { ... }
               ("write", ["index", "value"]) => {
                   let idx: usize = env.get_local("index")?.try_into()?;
                   let value = env.get_local("value")?;
                   self.write(idx, value);
                   Ok(CallResult::Return(None))
               }
               _ => env.unsupported(),
           }
       }
   }
   ```

2. Add an `ObjectState` variant in `mod.rs`.

   ```rust
   // arch/v1model/mod.rs
   pub enum ObjectState {
       ...
       Register(Register),
   }
   ```

3. Add an arm to `init_object` and to `call_method`.

   ```rust
   // arch/v1model/externs/mod.rs, in init_object
   "register" => Some(ObjectState::Register(Register::init(env)?)),

   // arch/v1model/externs/mod.rs, in call_method
   ObjectState::Register(object) => {
       sim.run_extern(call, |env| object.call(env, name, &params))?
   }
   ```
