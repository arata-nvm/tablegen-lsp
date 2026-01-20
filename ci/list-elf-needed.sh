#!/usr/bin/env bash
set -euo pipefail

usage() {
  echo "usage $0 <file>"
}

bin_path="${1:-}"
if [[ -z "${bin_path}" ]]; then
  usage
  exit 1
fi

if [[ "$(uname -s)" != "Linux" ]]; then
  echo "error: this check is intended to run on Linux (got: $(uname -s))" >&2
  exit 1
fi

if [[ ! -f "${bin_path}" ]]; then
  echo "error: ELF not found: ${bin_path}" >&2
  exit 1
fi

readelf -d "${bin_path}" \
  | awk -F'[][]' '/\(NEEDED\)/ { print $2 }' \
  | awk 'NF' \
  | LC_ALL=C sort -u

