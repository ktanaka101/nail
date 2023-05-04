FROM rust:slim-bullseye AS base

RUN apt-get update && apt-get -y upgrade

# See: https://apt.llvm.org/
RUN apt-get install -y gnupg2
RUN apt-get install -y wget
RUN apt-get install -y curl

RUN wget -O - https://apt.llvm.org/llvm-snapshot.gpg.key | apt-key add -

RUN echo 'deb http://apt.llvm.org/bullseye/ llvm-toolchain-bullseye-16 main' >> /etc/apt/sources.list

RUN apt-get update

# LLVM
RUN apt-get install -y libllvm-16-ocaml-dev libllvm16 llvm-16 llvm-16-dev llvm-16-doc llvm-16-examples llvm-16-runtime
# Clang and co
# Replaced python-clang-10 to python3-clang-10
RUN apt-get install -y clang-16 clang-tools-16 clang-16-doc libclang-common-16-dev libclang-16-dev libclang1-16 clang-format-16 python3-clang-16 clangd-16 clang-tidy-16
# When compile `llvm-sys`,
#   output `error: could not find native static library `Polly`, perhaps an -L flag is missing?`
RUN apt-get install -y libpolly-16-dev
# libfuzzer
RUN apt-get install -y libfuzzer-16-dev
# lldb
RUN apt-get install -y lldb-16
# lld (linker)
RUN apt-get install -y lld-16
# libc++
RUN apt-get install -y libc++-16-dev libc++abi-16-dev
# OpenMP
RUN apt-get install -y libomp-16-dev
# libclc
RUN apt-get install -y libclc-16-dev
# libunwind
RUN apt-get install -y libunwind-16-dev
# mlir
RUN apt-get install -y libmlir-16-dev mlir-16-tools
# bolt
# RUN apt-get install -y libbolt-16-dev bolt-16

# Building error for rust: note: /usr/bin/ld: cannot find -lz
# Required zlib1g-dev
RUN apt install -y zlib1g-dev

ENV PATH $PATH:/usr/lib/llvm-16/bin/

# llvm
ENV LLVM_SYS_160_STRICT_VERSIONING=160
ENV LLVM_SYS_160_PREFIX=/usr/lib/llvm-16

# Node.js and yarn for VSCode extension
RUN curl -sL https://deb.nodesource.com/setup_18.x | bash -
RUN apt install -y nodejs
RUN npm install -g yarn

# using by rustfmt and cargo-fuzz
RUN rustup default nightly

# for development

FROM base AS development

RUN apt-get install -y git

# fast build by the rust in docker
ENV CARGO_BUILD_TARGET_DIR=/tmp/target

ENV NAIL_LANGUAGE_SERVER_PATH=/tmp/target/debug/nail-language-server

# using by cargo-fuzz
RUN apt-get install -y g++
RUN cargo install cargo-fuzz

RUN rustup component add rustfmt clippy rust-analysis rust-src

# for CI

FROM base AS ci

RUN rustup component add rustfmt clippy
