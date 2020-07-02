#!/usr/bin/env zsh

# Do not write command to history if it starts with space
setopt HIST_IGNORE_SPACE
setopt HIST_NO_STORE

# Dot not save duplicate commands
setopt HIST_SAVE_NO_DUPS
HISTFILE=$HOME/.zhistory
HISTSIZE=2000
SAVEHIST=2000

# One history for all sessions
setopt APPEND_HISTORY
setopt INC_APPEND_HISTORY
setopt SHARE_HISTORY

# Save the time and how long a command ran
#setopt EXTENDED_HISTORY
