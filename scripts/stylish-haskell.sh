#!/bin/bash
#
# Runs stylish-haskell on all the Haskell files in the project. If --apply is
# passed, overwrites the files with the styled output. Otherwise, errors if
# differences are detected.

set -eo pipefail

builtin cd "$(dirname "${BASH_SOURCE[0]}")/.."

STYLISH_APPLY=0

for arg in "$@"; do
    case "$arg" in
        (--apply) STYLISH_APPLY=1 ;;
    esac
done

function get_files() {
    git ls-files | grep .hs$
}

function diff_no_fail() {
    diff "$@" || true
}

function check_file_empty() {
    if [[ -n "$(cat $1)" ]]; then
        return 1
    fi
}

if [[ "$STYLISH_APPLY" == 1 ]]; then
    get_files | xargs stack exec -- stylish-haskell --inplace
else
    trap 'rm -rf .tmp' 0
    get_files | while read FILE; do
        mkdir -p ".tmp/$(dirname "$FILE")"
        stack exec -- stylish-haskell "$FILE" | diff_no_fail --unified "$FILE" - > .tmp/"$FILE"
    done
    find .tmp -type f | xargs cat | tee .tmp/diffs.txt
    check_file_empty .tmp/diffs.txt
fi
