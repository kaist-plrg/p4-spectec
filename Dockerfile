# Development environment with the pinned toolchain and document dependencies
FROM rust:1.97.1-bookworm

RUN apt-get update \
 && apt-get install -y --no-install-recommends build-essential ruby-dev python3 \
 && rm -rf /var/lib/apt/lists/* \
 && gem install asciidoctor asciidoctor-pdf rouge --no-document

WORKDIR /p4-spectec

# Add the clippy and rustfmt components from the pinned toolchain
COPY p4spectec/rust-toolchain.toml p4spectec/
RUN cd p4spectec && rustup toolchain install

COPY . .
RUN make build && rm -rf p4spectec/target

ENV PATH="/p4-spectec/bin:${PATH}"
CMD ["bash"]
