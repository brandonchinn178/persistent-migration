#!/bin/bash
#
# Install third-party stack dependencies.

set -eo pipefail

stack build -j1 --test --only-dependencies
stack install -j1 hlint stylish-haskell
