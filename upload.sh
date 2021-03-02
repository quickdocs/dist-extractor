#!/bin/bash

# Usage:
#   $ ./upload.sh <directory>

dist=quicklisp
directory=${1:-./output}

get_local_md5hash() {
  filename=$1
  gsutil -q hash "$filename" | grep 'Hash (md5):' | sed -r 's/^\s*Hash \(md5\):\s*//'
}
get_gcs_md5hash() {
  filename=$1
  gsutil -q stat "gs://quickdocs-dist/$filename" | grep 'Hash (md5):' | sed -r 's/^\s*Hash \(md5\):\s*//'
}

cd "$directory"
for file in `ls $dist/**/*.json`; do
  md5hash=$(get_local_md5hash $file)
  gcs_md5hash=$(get_gcs_md5hash $file)
  if [ "$gcs_md5hash" != "$md5hash" ]; then
    echo "Upload '$file'"
    gsutil cp $file gs://quickdocs-dist/$file
    sleep 1
  else
    echo "No changes in '$file'"
  fi
done
