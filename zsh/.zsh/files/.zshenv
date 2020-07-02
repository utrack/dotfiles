#!/usr/bin/env zsh

export DOTSPATH=$HOME/.dotfiles
export ZDOTSROOT=$DOTSPATH/zsh-internal

export ZDOTDIR=$ZDOTSROOT/zdots
ZDOTDIR=$ZDOTSROOT/zdots
export ZSOURCED=zshenv-.z/files:$ZSOURCED

. "$ZDOTDIR/.zshenv"
