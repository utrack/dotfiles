#!/usr/bin/env zsh
set -euo pipefail

# completions
# courtesy of blaenk
unsetopt menu_complete
unsetopt flowcontrol

setopt COMPLETE_IN_WORD    # Complete from both ends of a word.
setopt ALWAYS_TO_END       # Move cursor to the end of a completed word.
setopt PATH_DIRS           # Perform path search even on command names with slashes.
setopt AUTO_MENU           # Show completion menu on a successive tab press.
setopt AUTO_LIST           # Automatically list choices on ambiguous completion.
setopt AUTO_PARAM_SLASH    # If completed parameter is a directory, add a trailing slash.
unsetopt MENU_COMPLETE     # Do not autoselect the first completion entry.
unsetopt FLOW_CONTROL      # Disable start/stop characters in shell editor.

# Load and initialize the completion system ignoring insecure directories.
autoload -Uz compinit && compinit -i

# directories
setopt auto_name_dirs
setopt auto_cd

zmodload -i zsh/complist

zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

zstyle ':completion:*::::' completer _expand _complete _ignored _approximate
zstyle ':completion:*:cd:*' tag-order local-directories directory-stack path-directories
cdpath=(.)

# case-insensitive substring completion
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'

zstyle ':completion:*:*:*:*:*' menu select=1 _complete _ignored _approximate

# use a cache
zstyle ':completion::complete:*' use-cache on
zstyle ':completion::complete:*' cache-path $ZDOTSROOT/cache/compcache

# ignore _functions
zstyle ':completion:*:functions' ignored-patterns '_*'

# Directories
zstyle ':completion:*:*:cd:*' tag-order local-directories directory-stack path-directories
zstyle ':completion:*:*:cd:*:directory-stack' menu yes select
zstyle ':completion:*:-tilde-:*' group-order 'named-directories' 'path-directories' 'users' 'expand'
zstyle ':completion:*' squeeze-slashes true

# theme
zstyle ':completion:*' list-colors "${(@s.:.)LS_COLORS}"

zstyle '*' single-ignored complete

# rehash binaries in PATH
zstyle ':completion:*' rehash true

# Style
# all
# zstyle ':completion:*:parameters'  list-colors '=*=32'
# aliases
# zstyle ':completion:*:aliases' list-colors '=*=2;38;5;128'
