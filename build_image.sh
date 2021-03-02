#!/bin/bash

image_name=$1
dist_version=$2

docker image inspect "${image_name}:${dist_version}" >/dev/null 2>&1 || docker pull "${image_name}:${dist_version}" >/dev/null 2>&1

remote_vcs_ref=$(docker image inspect "${image_name}:${dist_version}" 2>/dev/null | jq -M -r '.[0].Config.Labels["org.label-schema.vcs-ref"]')

if [ "$remote_vcs_ref" = "null" ] || \
   [ "$(git rev-parse --short $remote_vcs_ref 2>/dev/null)" = "" ] || \
   [ "$(git diff --exit-code $remote_vcs_ref...HEAD Dockerfile)" != "" ]; then
  echo "Dockerfile is changed between $remote_vcs_ref...HEAD"
  docker build -t "${image_name}:${dist_version}" \
    --build-arg DIST_VERSION=${dist_version} \
    --build-arg BUILD_DATE=`date -u +"%Y-%m-%dT%H:%M:%SZ"` \
    --build-arg VCS_REF=`git rev-parse --short HEAD` \
    .
fi
