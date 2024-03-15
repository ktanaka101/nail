#!/bin/bash

image_name=""
tag_name=""
should_push=false

usage() {
  echo "Usage: $0 [-i image-name] [-t tagname] [-p(push)]"
}

while getopts i:t:ph OPT
do
  case $OPT in
    "i" ) image_name=$OPTARG ;;
    "t" ) tag_name=$OPTARG ;;
    "p" ) should_push=true ;;
    "h" ) usage; exit 1 ;;
  esac
done

cmd_args=""
if [ "$image_name" = "" ]; then
  echo "Please set '-i IMAGE_NAME'"
  exit 1
elif [ "$tag_name" = "" ]; then
  echo "Please set '-t TAG_NAME'"
  exit 1
else
  cmd_args+="-t $image_name:$tag_name"
fi

if $should_push; then
  cmd_args+=" --push"
fi

docker buildx build --platform linux/arm64,linux/amd64 --target ci $cmd_args .
