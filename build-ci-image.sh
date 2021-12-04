#!/bin/bash

docker buildx build --platform linux/arm64,linux/amd64 --target ci -t $1 .
