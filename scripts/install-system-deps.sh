#!/bin/bash
#
# Install system dependencies.

set -eo pipefail

function is_command() {
    type "$1" &> /dev/null
}

function install_darwin() {
    if is_command brew; then
        if ! is_command postgres; then
            brew install postgresql
        fi
    fi
}

function install_linux() {
    if is_command yum; then
        yum update -y --exclude=filesystem
        yum install -y zlib-devel postgresql
    fi
}

case "$(uname)" in
    (Darwin) install_darwin ;;
    (Linux) install_linux ;;
esac
