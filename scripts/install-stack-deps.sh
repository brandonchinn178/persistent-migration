#!/bin/bash
#
# Install third-party stack dependencies.

set -eo pipefail

stack build --test --only-dependencies
stack install hlint stylish-haskell
