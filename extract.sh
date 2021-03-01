#!/bin/bash

# Usage:
#   $ ./extract.sh <destination>

IFS=$'\n'
set -f

trap quit SIGINT
quit() { echo "SIGINT"; exit; }

delete_if_empty() {
  [ -s "$1" ] || rm "$1"
}
validate_json() {
  file=$1
  (cat $file | jq >/dev/null) || (echo "[ERROR] Failed to parse a JSON file '$file'." >&2 && exit 1)
}

dist=quicklisp
version=$(scripts/dist.lisp "$dist" version)
output=./.output
destination=${1:-./output}
dist_dir="$output/$dist/$version"

mkdir -p "$dist_dir"
scripts/dist.lisp "$dist" \
    2> "$dist_dir/errors.log" \
  | scripts/sexp-to-json.lisp > "$dist_dir/info.json"
delete_if_empty "$dist_dir/errors.log"

## Parsing releases
releases=( $(scripts/dist.lisp "$dist" releases) )
current=0
for release in "${releases[@]}"; do
  current=$((++current))
  echo "[$current/${#releases[@]}] Release '${release}'"
  release_dir="$dist_dir/$release"
  mkdir -p "$release_dir"
  scripts/release.lisp "$release" \
      2> "$release_dir/errors.log" \
    | scripts/sexp-to-json.lisp > "$release_dir/info.json"
  validate_json "$release_dir/info.json" 2> "$release_dir/errors.log"
  delete_if_empty "$release_dir/errors.log"

  ## Parsing systems
  for system in `scripts/release.lisp "$release" systems`; do
    system_dir="$release_dir/$system"
    mkdir -p "$system_dir"
    timeout -k 10 -s TERM 60 \
      scripts/system.lisp "$system" \
          2> "$system_dir/errors.log" \
        | scripts/sexp-to-json.lisp > "$system_dir/info.json"
    validate_json "$system_dir/info.json" 2> "$system_dir/errors.log"
    delete_if_empty "$system_dir/errors.log"
  done
done

mv "$output" "$destination"
chmod 777 -R "$destination"
