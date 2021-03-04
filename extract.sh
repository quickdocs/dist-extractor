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
  (cat "$file" | jq . -M >/dev/null) || (echo "[ERROR] Failed to parse a JSON file '$file'." >&2 && exit 1)
}

dist=quicklisp
version=$(scripts/dist.lisp "$dist" version)
output=./.output
destination=${1:-./output}
dist_dir="$output/$dist/$version"

mkdir -p "$destination/$dist/$version"
scripts/dist.lisp "$dist" \
  | scripts/sexp-to-json.lisp | jq . -M > "$destination/$dist/$version/info.json"

scripts/dist.lisp "$dist" releases \
  | scripts/sexp-to-json.lisp | jq . -M > "$destination/$dist/$version/releases.json"

## Parsing releases
current=0
mkdir -p "$dist_dir/releases"
releases=( $(cat "$destination/$dist/$version/releases.json" | jq -r '. | keys | .[]') )
for release in "${releases[@]}"; do
  current=$((++current))
  echo "[$current/${#releases[@]}] Release '${release}'"
  release_dir="$dist_dir/releases/$release"
  mkdir -p "$release_dir"
  scripts/release.lisp "$release" \
      2> >(tee "$release_dir/error.log" >&2) \
    | scripts/sexp-to-json.lisp > "$release_dir/info.json" 2> >(tee -a "$release_dir/error.log" >&2)
      validate_json "$release_dir/info.json" 2> >(tee -a "$release_dir/error.log" >&2)
  delete_if_empty "$release_dir/error.log"

  ## Parsing systems
  mkdir -p "$release_dir/systems"
  for system in `scripts/release.lisp "$release" systems`; do
    system_dir="$release_dir/systems/$system"
    mkdir -p "$system_dir"
    timeout -k 10 -s TERM 60 \
      scripts/system.lisp "$system" \
          2> >(tee "$system_dir/error.log" >&2) \
        | scripts/sexp-to-json.lisp > "$system_dir/info.json" 2> >(tee -a "$system_dir/error.log" >&2)
        validate_json "$system_dir/info.json" 2> >(tee -a "$system_dir/error.log" >&2)
    delete_if_empty "$system_dir/error.log"
  done

  base_path="$(cat "$release_dir/info.json" | jq -r '.archive_url' | sed -s -r 's!http://beta.quicklisp.org/archive/[^/]+/!!' | sed -s 's/\.tgz$//')"
  mkdir -p "$destination/$dist/$base_path"

  ## Concatenate error logs
  destination_error_log="$destination/$dist/$base_path/error.log"
  error_logs=( $(find "$release_dir" -name "error.log") )
  if [ ${#error_logs} != 0 ]; then
    tail -n +1 -v ${error_logs[@]} > $destination_error_log
  fi

  ## Concatenate JSON files
  find "$release_dir/systems" -name info.json | \
    xargs jq -s 'map({(.name): .}) | add | {systems:.}' | \
    jq -M --slurpfile release "$release_dir/info.json" '$release[0] * .' \
      > "$destination/$dist/$base_path/info.json"
done

## Concatenate error logs
find "$dist_dir" -name "error.log" | xargs tail -n +1 -v > "$destination/$dist/$version/errors.log"

chmod 777 -R "$destination"
