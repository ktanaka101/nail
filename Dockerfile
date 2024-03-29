FROM rust:slim-bullseye AS base

RUN apt-get update && apt-get -y upgrade && \
  apt-get install -y gnupg2 wget curl

RUN wget -O - https://apt.llvm.org/llvm-snapshot.gpg.key | gpg --dearmor > /usr/share/keyrings/llvm-snapshot.gpg && \
  echo 'deb [signed-by=/usr/share/keyrings/llvm-snapshot.gpg] http://apt.llvm.org/bullseye/ llvm-toolchain-bullseye-17 main' >> /etc/apt/sources.list && \
  apt-get update && \
  apt-get install -y libllvm-17-ocaml-dev libllvm17 llvm-17 llvm-17-dev llvm-17-doc llvm-17-examples llvm-17-runtime \
  clang-17 clang-tools-17 clang-17-doc libclang-common-17-dev libclang-17-dev libclang1-17 clang-format-17 python3-clang-17 clangd-17 clang-tidy-17 \
  libpolly-17-dev \
  libfuzzer-17-dev \
  lldb-17 \
  lld-17 \
  libc++-17-dev libc++abi-17-dev \
  libomp-17-dev \
  libclc-17-dev \
  libunwind-17-dev \
  libmlir-17-dev mlir-17-tools

RUN apt install -y zlib1g-dev \
  zstd \
  libzstd-dev

ENV LLVM_SYS_170_STRICT_VERSIONING=170 \
  LLVM_SYS_170_PREFIX=/usr/lib/llvm-17 \
  PATH=$PATH:/usr/lib/llvm-17/bin/

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
