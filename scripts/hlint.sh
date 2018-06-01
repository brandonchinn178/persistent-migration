#!/bin/bash

set -eo pipefail

stack build hlint
stack exec -- hlint .
