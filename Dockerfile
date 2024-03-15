FROM rust:slim-bullseye AS base

RUN apt-get update && apt-get -y upgrade && \
  apt-get install -y gnupg2 wget curl

RUN wget -O - https://apt.llvm.org/llvm-snapshot.gpg.key | gpg --dearmor > /usr/share/keyrings/llvm-snapshot.gpg && \
  echo 'deb [signed-by=/usr/share/keyrings/llvm-snapshot.gpg] http://apt.llvm.org/bullseye/ llvm-toolchain-bullseye-16 main' >> /etc/apt/sources.list && \
  apt-get update && \
  apt-get install -y libllvm-16-ocaml-dev libllvm16 llvm-16 llvm-16-dev llvm-16-doc llvm-16-examples llvm-16-runtime \
  clang-16 clang-tools-16 clang-16-doc libclang-common-16-dev libclang-16-dev libclang1-16 clang-format-16 python3-clang-16 clangd-16 clang-tidy-16 \
  libpolly-16-dev \
  libfuzzer-16-dev \
  lldb-16 \
  lld-16 \
  libc++-16-dev libc++abi-16-dev \
  libomp-16-dev \
  libclc-16-dev \
  libunwind-16-dev \
  libmlir-16-dev mlir-16-tools

RUN apt install -y zlib1g-dev

ENV LLVM_SYS_160_STRICT_VERSIONING=160 \
  LLVM_SYS_160_PREFIX=/usr/lib/llvm-16 \
  PATH=$PATH:/usr/lib/llvm-16/bin/

RUN curl -sL https://deb.nodesource.com/setup_18.x | bash - && \
  apt install -y nodejs && \
  npm install -g yarn

RUN rustup default nightly

# ---------------------------------------
FROM base AS development

RUN apt-get update && \
  apt-get install -y git g++ && \
  apt-get clean && \
  rm -rf /var/lib/apt/lists/*

ENV CARGO_BUILD_TARGET_DIR=/tmp/target \
  NAIL_LANGUAGE_SERVER_PATH=/tmp/target/debug/nail-language-server

RUN cargo install cargo-fuzz
RUN rustup component add rustfmt clippy rust-analysis rust-src

# ---------------------------------------
FROM base AS ci

RUN rustup component add rustfmt clippy

RUN apt-get clean && \
  rm -rf /var/lib/apt/lists/*
