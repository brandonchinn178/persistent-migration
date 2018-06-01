#!/bin/bash
#
# Install third-party dependencies.

set -eo pipefail

stack build --test --only-dependencies
stack build hlint stylish-haskell
