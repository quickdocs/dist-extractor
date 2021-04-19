#!/bin/bash

dist_name=$1
dist_version=$2
destination=${3:-./output}

echo '{"name":"quicklisp","latest_version","${dist_version}","latest_version_info_url":"${BUCKET_BASE_URL}/quicklisp/$(version)/info.json","available_versions_url":"${BUCKET_BASE_URL}/quicklisp/versions.json"}' | jq . -M

# TODO: Add versions.json which contains an array of available versions.
cat << EOF | jq . -M > $destination/$dist_name/info.json
{
  "name": "$dist_name",
  "latest_version": "$dist_version",
  "latest_version_info_url": "$BUCKET_BASE_URL/$dist_name/$dist_version/info.json",
}
EOF
