#!/usr/bin/env bash

[[ -v UZENV_SOURCED ]] && return
export UZENV_SOURCED=1

# sourced by root zshenv
export ZSOURCED=zshenv-dots:$ZSOURCED

# Source non-interactive env configs
for file in $ZDOTSROOT/env/*.zsh; do
  source $file
done
