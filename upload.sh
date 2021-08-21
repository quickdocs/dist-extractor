#!/bin/bash

# Usage:
#   $ ./upload.sh <directory>

BASEDIR=$(dirname `readlink -f $0`)
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
  ext=${file##*.}
  case "$ext" in
    json)
      content_type=application/json
      ;;
    *)
      content_type=text/plain
      ;;
  esac
  md5hash=$(get_local_md5hash $file)
  gcs_md5hash=$(get_gcs_md5hash $file)
  if [ "$gcs_md5hash" != "$md5hash" ]; then
    echo "Upload '$file'"
    gsutil -h "Content-Type:$content_type" cp $file gs://quickdocs-dist/$file
  else
    echo "No changes in '$file'"
  fi
}

if [ ! -d "$directory" ]; then
  echo "Directory '$directory' doesn't exist. Nothing to upload."
  exit
fi

cd "$directory"
for entry in `ls $dist`; do
  if [ -f "$dist/$entry" ]; then
    upload_if_changed "$dist/$entry"
  elif [ -d "$dist/$entry" ]; then
    version=$entry
    for file in `find $dist/$version -maxdepth 1 -not -type d`; do
      upload_if_changed $file
    done
    ls -d $dist/$version/releases/*/ | xargs -P8 -L1 $BASEDIR/upload_release.sh
  fi
done
