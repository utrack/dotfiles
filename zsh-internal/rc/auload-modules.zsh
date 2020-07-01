#!/usr/bin/env zsh
set -euo pipefail

# expand ... -> ../..
autoload -Uz manydots-magic
manydots-magic
