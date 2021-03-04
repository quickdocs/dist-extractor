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
  gsutil stat "gs://quickdocs-dist/$filename" 2>/dev/null | grep 'Hash (md5):' | sed -r 's/^\s*Hash \(md5\):\s*//'
}

upload_if_changed() {
  file=$1
  md5hash=$(get_local_md5hash $file)
  gcs_md5hash=$(get_gcs_md5hash $file)
  if [ "$gcs_md5hash" != "$md5hash" ]; then
    echo "Upload '$file'"
    gsutil cp $file gs://quickdocs-dist/$file
  else
    echo "No changes in '$file'"
  fi
}

remove_if_exists() {
  file=$1
  if [ $(gsutil -q stat "gs://quickdocs-dist/$file") ]; then
    echo "Delete '$file' if exists on remote"
    gsutil rm "gs://quickdocs-dist/$file"
  fi
}

cd "$directory"
for version in `ls $dist`; do
  for file in `find $dist/$version -maxdepth 1 -not -type d`; do
    upload_if_changed $file
  done
  for release in `ls -d $dist/$version/*/`; do
    for file in `ls $release`; do
      upload_if_changed "${release}${file}"
    done
    if [ ! -f ${release}error.log ]; then
      remove_if_exists "${release}error.log"
    fi
  done
done
