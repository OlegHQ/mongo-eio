#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
SPEC_REPO="${ROOT}/test/fixtures/mongodb-specifications"
DEST="${ROOT}/test/fixtures/imported"

if [[ ! -d "${SPEC_REPO}/.git" ]]; then
  git clone --depth 1 https://github.com/mongodb/specifications.git "${SPEC_REPO}"
fi

mkdir -p "${DEST}/bson-corpus" "${DEST}/uri-options" "${DEST}/auth"

copy_tree() {
  local src="$1"
  local dst="$2"
  if [[ -d "${SPEC_REPO}/${src}" ]]; then
    rm -rf "${dst}"
    cp -R "${SPEC_REPO}/${src}" "${dst}"
  fi
}

copy_tree "source/bson-corpus/tests" "${DEST}/bson-corpus/tests"
copy_tree "source/uri-options/tests" "${DEST}/uri-options/tests"
copy_tree "source/auth/tests" "${DEST}/auth/tests"

echo "Imported spec fixtures into ${DEST}"
