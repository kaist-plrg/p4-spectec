# --------------------------------------
# Stage 1: System dependencies
# --------------------------------------
FROM ubuntu:22.04 AS base

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update && \
    apt-get install -y git make curl && \
    apt-get clean && rm -rf /var/lib/apt/lists/*

WORKDIR /home

# --------------------------------------
# Stage 2: Clone repo
# --------------------------------------
FROM base AS source

RUN git clone https://github.com/kaist-plrg/p4-spectec.git && \
    cd p4-spectec && \
    git checkout ntt-syntax && \
    git submodule update --init --recursive

WORKDIR /home/p4-spectec

# ---------------------------------------
# Stage 3: P4-SpecTec dependencies
# ---------------------------------------
FROM source AS opambase

ARG DEBIAN_FRONTEND=noninteractive
ENV TZ=Asia/Seoul

RUN apt-get update && \
    apt-get install -y opam libgmp-dev pkg-config && \
    apt-get clean && rm -rf /var/lib/apt/lists/*

# Initialize opam
RUN opam init --disable-sandboxing --auto-setup && \
    opam switch create 5.1.0 && \
    eval $(opam env) && \
    opam install dune menhir bignum core core_unix bisect_ppx -y

# Set opam environment permanently
ENV OPAM_SWITCH_PREFIX=/root/.opam/5.1.0
ENV PATH=$OPAM_SWITCH_PREFIX/bin:$PATH
ENV CAML_LD_LIBRARY_PATH=$OPAM_SWITCH_PREFIX/lib/stublibs:$OPAM_SWITCH_PREFIX/lib/ocaml/stublibs:$OPAM_SWITCH_PREFIX/lib/ocaml

# ---------------------------------------
# Stage 4: Build P4-SpecTec
# ---------------------------------------
FROM opambase AS p4specbase

RUN make build-spec && \
    chmod a+x ./p4spectec

# --------------------------------------
# Stage 5: Fuzzer & Reducer dependencies
# --------------------------------------
FROM p4specbase AS reducebase

RUN apt-get update && \
    apt-get install -y clang creduce python3 pip && \
    apt-get clean && rm -rf /var/lib/apt/lists/*

RUN python3 -m pip install psutil
COPY patches/creduce /usr/bin/creduce
RUN chmod +x /usr/bin/creduce

ENV P4SPECTEC_PATH=/home/p4-spectec

# --------------------------------------
# Stage 6: P4C dependencies
# --------------------------------------
FROM reducebase AS p4cbase
ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && \
    apt-get install -y \
    sudo bison build-essential cmake curl flex g++ git lld \
    libboost-dev libboost-graph-dev libboost-iostreams-dev \
    libfl-dev ninja-build pkg-config python3 python3-pip \
    python3-setuptools tcpdump wget ca-certificates \
    && apt-get clean && rm -rf /var/lib/apt/lists/*

# ---------------------------------------
# Stage 7: Build P4C
# ---------------------------------------
WORKDIR /home/p4-spectec/p4c/build
# RUN ccache --set-config=max_size=1G
RUN cmake -G "Unix Makefiles" .. \
    -DCMAKE_BUILD_TYPE=Debug \
    -DENABLE_BMV2=OFF \
    -DENABLE_EBPF=OFF \
    -DENABLE_P4TC=OFF \
    -DENABLE_UBPF=OFF \
    -DENABLE_DPDK=OFF \
    -DENABLE_TEST_TOOLS=ON \
    -DENABLE_GTESTS=OFF \
    -DCMAKE_EXPORT_COMPILE_COMMANDS=ON \
    -DCMAKE_CXX_FLAGS="--coverage -O0" \
    -DCMAKE_C_FLAGS="--coverage -O0"
RUN make -j$(nproc) VERBOSE=1
RUN make install .

RUN pip3 install gcovr
WORKDIR /home/p4-spectec
