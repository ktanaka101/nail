#!/bin/sh

cargo binstall tokei && \
tokei ./ \
  -e .git \
  -e Cargo.lock \
  -e target \
  -e yarn.lock \
  -e .vscode-test \
  -e node_modules \
  -e out \
  -e fuzz/artifacts \
  -e fuzz/corpus
