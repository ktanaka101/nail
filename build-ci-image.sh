#!/bin/bash

usage() {
  echo "Usage: $0 [-i IMAGE_NAME] [-t TAG_NAME] [-p]"
  echo "  -i IMAGE_NAME   The name of the Docker image (required)"
  echo "  -t TAG_NAME     The tag name for the Docker image (required)"
  echo "  -p              Push the image to a registry (optional)"
  echo "  Environment Variables:"
  echo "    IMAGE_NAME: The name of the Docker image (required)"
  echo "    TAG_NAME: The tag name for the Docker image (required)"
  echo "    SHOULD_PUSH: Set to 'true' to push the image to a registry (optional, default: false)"
}

image_name=""
tag_name=""
should_push=""

# Load environment variables from .env.local file if it exists
if [ -f .env.local ]; then
  while read -r line; do
    export "$line"
  done < <(grep -v '^#' .env.local)
fi

# Override with command line arguments if provided
while getopts ":i:t:ph" opt; do
  case ${opt} in
    i )
      image_name=$OPTARG
      ;;
    t )
      tag_name=$OPTARG
      ;;
    p )
      should_push=true
      ;;
    \? )
      echo -e "Invalid option: $OPTARG"
      usage
      exit 1
      ;;
    : )
      echo -e "Invalid option: $OPTARG requires an argument"
      usage
      exit 1
      ;;
  esac
done
shift $((OPTIND -1))

# Use environment variables if not overridden by command line arguments
image_name="${image_name:-$IMAGE_NAME}"
tag_name="${tag_name:-$TAG_NAME}"
should_push="${should_push:-$SHOULD_PUSH}"

# Set default values if still empty
image_name="${image_name:-}"
tag_name="${tag_name:-}"
should_push="${should_push:-false}"

if [ -z "$image_name" ]; then
  echo -e "Please provide the IMAGE_NAME using the -i option or set the 'IMAGE_NAME' environment variable"
  usage
  exit 1
fi

if [ -z "$tag_name" ]; then
  echo -e "Please provide the TAG_NAME using the -t option or set the 'TAG_NAME' environment variable"
  usage
  exit 1
fi

cmd_args="-t $image_name:$tag_name"

if [ "$should_push" = "true" ]; then
  cmd_args+=" --push"
fi

docker buildx build --platform linux/arm64,linux/amd64 --target ci $cmd_args .
