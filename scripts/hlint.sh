#!/bin/bash

set -eo pipefail

stack exec -- hlint .
