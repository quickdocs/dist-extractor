#!/bin/bash

# Usage:
#   $ ./extract.sh <destination>

IFS=$'\n'
set -f

trap quit SIGINT
quit() { echo "SIGINT"; exit; }

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

current_extracted_version=$(curl -sL https://storage.googleapis.com/quickdocs-dist/quicklisp/info.json | jq -r '.latest_version' 2>/dev/null || echo '0000-00-00')
if [ "$current_extracted_version" == "$version" ]; then
  releases=( $(cat "$destination/$dist/$version/releases.json" | jq -r "to_entries | map(select(.value | scan(\"[0-9]{4}-[0-9]{2}-[0-9]{2}\") == \"${version}\")) | map(.key) | .[]") )
  echo "Extracting new/updated projects in $version."
elif [ "$current_extracted_version" > "$version" ]; then
  releases=( $(cat "$destination/$dist/$version/releases.json" | jq -r '. | keys | .[]') )
else
  releases=( $(cat "$destination/$dist/$version/releases.json" | jq -r "to_entries | map(select(.value | scan(\"[0-9]{4}-[0-9]{2}-[0-9]{2}\") > \"${current_extracted_version}\")) | map(.key) | .[]") )
  echo "Extracting new/updated projects between $current_extracted_version...$version."
fi

for release in "${releases[@]}"; do
  current=$((++current))
  echo "[$current/${#releases[@]}] Release '${release}'"
  release_dir="$dist_dir/releases/$release"
  mkdir -p "$release_dir"
  scripts/release.lisp "$release" \
      2> >(tee -a "$release_dir/error.log" >&2) \
    | scripts/sexp-to-json.lisp > "$release_dir/info.json" 2> >(tee -a "$release_dir/error.log" >&2)
  validate_json "$release_dir/info.json" 2> >(tee -a "$release_dir/error.log" >&2)

  ## Write readme.json
  scripts/readme.lisp "$release" \
      2> >(tee -a "$release_dir/error.log" >&2) \
    | scripts/sexp-to-json.lisp > "$release_dir/readme.json" 2> >(tee -a "$release_dir/error.log" >&2)
  validate_json "$release_dir/readme.json" 2> >(tee -a "$release_dir/error.log" >&2)

  ## Get upstream URL
  scripts/quicklisp-upstream.lisp "$release" \
      2> >(tee -a "$release_dir/error.log" >&2) > "$release_dir/upstream_url.txt"
  upstream_url=$(cat "$release_dir/upstream_url.txt")

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
  done

  version_and_prefix="$(cat "$release_dir/info.json" | jq -r '.archive_url' | sed -s -r 's!http://beta.quicklisp.org/archive/[^/]+/!!' | sed -s 's/\.tgz$//')"
  release_version=$(echo $version_and_prefix | sed -s -r 's![^/]+$!!')
  prefix=$(echo $version_and_prefix | sed -s -r 's!^[^/]+!!')
  mkdir -p "$destination/$dist/$release_version/releases/$prefix"

  ## Concatenate error logs
  destination_error_log="$destination/$dist/$release_version/releases/$prefix/error.log"
  error_logs=( $(find "$release_dir" -name "error.log" -size +0c) )
  if [ ${#error_logs} != 0 ]; then
    tail -n +1 -v ${error_logs[@]} > $destination_error_log
  fi

  ## Output release info.json
  cat "$release_dir/info.json" | jq ". += {\"upstream_url\": \"$upstream_url\"}" -M \
    > "$destination/$dist/$release_version/releases/$prefix/info.json"

  ## Output release readme.json
  cat "$release_dir/readme.json" | jq . -M > "$destination/$dist/$release_version/releases/$prefix/readme.json"

  ## Concatenate system JSON files
  find "$release_dir/systems" -name info.json | \
    xargs jq -s 'map({(.name): .}) | add' \
      > "$destination/$dist/$release_version/releases/$prefix/systems.json"
done

## Concatenate error logs
find "$dist_dir" -name "error.log" -size +0c | xargs tail -n +1 -v > "$destination/$dist/$version/errors.log"

chmod 777 -R "$destination"
