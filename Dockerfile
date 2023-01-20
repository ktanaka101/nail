FROM rust:slim-bullseye AS base

RUN apt-get update && apt-get -y upgrade

# See: https://apt.llvm.org/
RUN apt-get install -y gnupg2
RUN apt-get install -y wget
RUN apt-get install -y curl

RUN wget -O - https://apt.llvm.org/llvm-snapshot.gpg.key | apt-key add -

RUN echo 'deb http://apt.llvm.org/bullseye/ llvm-toolchain-bullseye-15 main' >> /etc/apt/sources.list

RUN apt-get update

# LLVM
RUN apt-get install -y libllvm-15-ocaml-dev libllvm15 llvm-15 llvm-15-dev llvm-15-doc llvm-15-runtime
# Clang and co
# Replaced python-clang-10 to python3-clang-10
RUN apt-get install -y clang-15 clang-tools-15 clang-15-doc libclang-common-15-dev libclang-15-dev libclang1-15 clang-format-15 python3-clang-15 clangd-15
# When compile `llvm-sys`,
#   output `error: could not find native static library `Polly`, perhaps an -L flag is missing?`
RUN apt-get install -y libpolly-15-dev
# libfuzzer
RUN apt-get install -y libfuzzer-15-dev
# lldb
RUN apt-get install -y lldb-15
# lld (linker)
RUN apt-get install -y lld-15
# libc++
RUN apt-get install -y libc++-15-dev libc++abi-15-dev
# OpenMP
RUN apt-get install -y libomp-15-dev

# Building error for rust: note: /usr/bin/ld: cannot find -lz
# Required zlib1g-dev
RUN apt install -y zlib1g-dev

ENV PATH $PATH:/usr/lib/llvm-15/bin/

# llvm
ENV LLVM_SYS_150_STRICT_VERSIONING=150
ENV LLVM_SYS_150_PREFIX=/usr/lib/llvm-15

# Node.js for VSCode extension
RUN curl -sL https://deb.nodesource.com/setup_18.x | bash -
RUN apt install -y nodejs

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
